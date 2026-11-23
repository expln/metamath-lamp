// WebSocket server which a metamath-lamp browser tab connects to.
// The browser side is the macro in mm-lamp-macro.js.
//
// Protocol (all messages are JSON):
//   browser -> relay  {type:"hello", client:string, version:number, url:string}
//   relay   -> browser {type:"call", id:number, path:string, editorId?:number, params?:any}
//   browser -> relay  {type:"result", id:number, resp:{isOk:boolean, res?:any, err?:string}}

import { WebSocketServer } from 'ws'

const PROTOCOL_VERSION = 1
const HEARTBEAT_INTERVAL_MS = 15_000
const CLOSE_CODE_ANOTHER_TAB_CONNECTED = 4001
const CLOSE_CODE_ORIGIN_NOT_ALLOWED = 4003
const CLOSE_CODE_PROTOCOL_MISMATCH = 4004

function log(msg) {
    // stdout is reserved for the MCP protocol, so all logging goes to stderr.
    console.error(`[mm-lamp-bridge] ${msg}`)
}

function isLocalhostOrigin(origin) {
    try {
        const {protocol, hostname} = new URL(origin)
        return (protocol === 'http:' || protocol === 'https:')
            && (hostname === 'localhost' || hostname === '127.0.0.1' || hostname === '[::1]')
    } catch {
        return false
    }
}

export class BrowserBridge {
    constructor({port, allowedOrigins}) {
        this.port = port
        this.allowedOrigins = allowedOrigins
        this.socket = null
        this.clientUrl = null
        this.listenError = null
        this.nextCallId = 1
        this.pendingCalls = new Map()
    }

    start() {
        const wss = new WebSocketServer({host: '127.0.0.1', port: this.port})
        wss.on('listening', () => log(`Waiting for a metamath-lamp tab on ws://127.0.0.1:${this.port}`))
        wss.on('error', err => {
            this.listenError = err.code === 'EADDRINUSE'
                ? `Port ${this.port} is already in use (is another mm-lamp MCP server running?). `
                    + `Set MM_LAMP_BRIDGE_PORT to use a different port.`
                : `WebSocket server error: ${err.message}`
            log(this.listenError)
        })
        wss.on('connection', (ws, req) => this.onConnection(ws, req))
        this.heartbeat = setInterval(() => this.checkHeartbeat(), HEARTBEAT_INTERVAL_MS)
        this.heartbeat.unref()
    }

    isOriginAllowed(origin) {
        if (origin === undefined) {
            // Non-browser clients don't send Origin. Browsers always do, so web pages cannot get here.
            return true
        }
        return isLocalhostOrigin(origin) || this.allowedOrigins.includes(origin)
    }

    onConnection(ws, req) {
        const origin = req.headers.origin
        if (!this.isOriginAllowed(origin)) {
            log(`Rejected a connection from origin ${origin}`)
            ws.close(CLOSE_CODE_ORIGIN_NOT_ALLOWED, 'Origin is not allowed')
            return
        }
        if (this.socket !== null) {
            ws.close(CLOSE_CODE_ANOTHER_TAB_CONNECTED, 'Another metamath-lamp tab is already connected')
            return
        }
        this.socket = ws
        ws.isAlive = true
        ws.on('pong', () => { ws.isAlive = true })
        ws.on('message', data => this.onMessage(ws, data))
        ws.on('close', () => {
            if (this.socket === ws) {
                log('The metamath-lamp tab disconnected.')
                this.socket = null
                this.clientUrl = null
                this.rejectAllPending('The metamath-lamp tab disconnected before responding.')
            }
        })
        ws.on('error', err => log(`WebSocket error: ${err.message}`))
    }

    onMessage(ws, data) {
        let msg
        try {
            msg = JSON.parse(data.toString())
        } catch {
            log('Received a non-JSON message from the browser, ignoring it.')
            return
        }
        if (msg.type === 'hello') {
            if (msg.version !== PROTOCOL_VERSION) {
                ws.close(
                    CLOSE_CODE_PROTOCOL_MISMATCH,
                    `Protocol version mismatch: relay=${PROTOCOL_VERSION}, macro=${msg.version}`
                )
                return
            }
            this.clientUrl = msg.url
            log(`A metamath-lamp tab connected: ${msg.url}`)
        } else if (msg.type === 'result') {
            const pending = this.pendingCalls.get(msg.id)
            if (pending === undefined) {
                return
            }
            this.pendingCalls.delete(msg.id)
            clearTimeout(pending.timer)
            pending.resolve(msg.resp)
        }
    }

    checkHeartbeat() {
        const ws = this.socket
        if (ws === null) {
            return
        }
        if (!ws.isAlive) {
            log('The metamath-lamp tab stopped responding, dropping the connection.')
            ws.terminate()
            return
        }
        ws.isAlive = false
        ws.ping()
    }

    rejectAllPending(errMsg) {
        for (const pending of this.pendingCalls.values()) {
            clearTimeout(pending.timer)
            pending.resolve({isOk: false, err: errMsg})
        }
        this.pendingCalls.clear()
    }

    /*
    * Returns an error message if no browser tab is connected, otherwise undefined.
    * */
    getNotConnectedError() {
        if (this.socket !== null) {
            return undefined
        }
        return this.listenError ?? 'No metamath-lamp tab is connected to the MCP server. '
            + 'Ask the user to open metamath-lamp and run the "Connect to MCP server" macro.'
    }

    /*
    * Calls a metamath-lamp API function in the connected browser tab.
    * path - e.g. "editor.getState" or "editor.unifyAll".
    * editorId - only for "editor.*" paths; undefined means the last opened editor tab.
    * Resolves to the API response {isOk, res, err}. Never rejects.
    * */
    call({path, params, editorId, timeoutMs}) {
        const notConnectedErr = this.getNotConnectedError()
        if (notConnectedErr !== undefined) {
            return Promise.resolve({isOk: false, err: notConnectedErr})
        }
        const id = this.nextCallId++
        return new Promise(resolve => {
            const timer = setTimeout(() => {
                this.pendingCalls.delete(id)
                resolve({isOk: false, err: `metamath-lamp did not respond to '${path}' within ${timeoutMs} ms.`})
            }, timeoutMs)
            this.pendingCalls.set(id, {resolve, timer})
            this.socket.send(JSON.stringify({type: 'call', id, path, editorId, params}))
        })
    }
}
