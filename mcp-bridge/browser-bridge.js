// WebSocket server which a metamath-lamp browser tab connects to.
// The browser side is the macro in mm-lamp-macro.js.
// The WebSocket server runs on top of an HTTP server, which also serves other HTTP requests
// (e.g. local files, see file-server.js) on the same port.
//
// Protocol (all messages are JSON):
//   browser -> relay  {type:"hello", client:string, version:number, url:string}
//   relay   -> browser {type:"call", id:number, path:string, editorId?:number, params?:any}
//   browser -> relay  {type:"result", id:number, resp:{isOk:boolean, res?:any, err?:string}}

import { createServer } from 'node:http'
import { WebSocketServer } from 'ws'

const PROTOCOL_VERSION = 1
const HEARTBEAT_INTERVAL_MS = 15_000
// How long a tool call waits for a tab to (re)connect.
// The macro retries every 3 s (RECONNECT_DELAY_MS in mm-lamp-macro.js),
// so this is enough for a tab to connect after the port is opened.
const TAB_CONNECT_WAIT_MS = 5000
const CLOSE_CODE_ANOTHER_TAB_CONNECTED = 4001
const CLOSE_CODE_ORIGIN_NOT_ALLOWED = 4003
const CLOSE_CODE_PROTOCOL_MISMATCH = 4004

export function log(msg) {
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

/*
* Returns true if a browser page with this origin may connect to the bridge or download files from it.
* */
export function isOriginAllowed(origin, allowedOrigins) {
    return isLocalhostOrigin(origin) || allowedOrigins.includes(origin)
}

export class BrowserBridge {
    /*
    * httpHandler - async (req, res) => boolean, handles plain HTTP requests (not WebSocket upgrades).
    * It returns false if it didn't handle the request; then 404 is returned.
    * */
    constructor({port, allowedOrigins, httpHandler}) {
        this.port = port
        this.allowedOrigins = allowedOrigins
        this.httpHandler = httpHandler
        this.listeningPromise = null
        this.wss = null
        this.tabWaiters = []
        this.socket = null
        this.nextCallId = 1
        this.pendingCalls = new Map()
    }

    /*
    * Starts listening on the port, unless already listening.
    * This is done lazily (on the first tool call) rather than on startup,
    * because agent clients may start idle copies of MCP servers (e.g. pre-warmed sessions),
    * and an idle copy must not take the port from the session which is actually used.
    * Resolves to undefined on success or to an error message. If the port is busy, the next call tries again.
    * */
    _ensureListening() {
        if (this.wss !== null) {
            return Promise.resolve(undefined)
        }
        if (this.listeningPromise === null) {
            this.listeningPromise = new Promise(resolve => {
                const httpServer = createServer((req, res) => this._onHttpRequest(req, res))
                // The WebSocket server re-emits 'listening' and 'error' events of the HTTP server,
                // so the handlers below work the same as for a standalone WebSocket server.
                const wss = new WebSocketServer({server: httpServer})
                wss.once('listening', () => {
                    // A bound server is never closed: it is supposed to work until the process exits.
                    // The process exits only when the MCP client closes stdin, i.e. when the agent session ends
                    // (see process.exit() at the end of server.js).
                    // Errors after binding don't stop the server, they are only logged by the handler below.
                    // That's why this.wss is never reset to null, and why the single heartbeat timer is never
                    // stored or cleared.
                    // If a bound server is ever closed (e.g. to free the port when the tab disconnects),
                    // reset this.wss to null on its 'close' event, so the next tool call binds again,
                    // and store the heartbeat timer to clear it, so timers don't accumulate on rebinding.
                    this.wss = wss
                    this.listeningPromise = null
                    wss.on('error', err => log(`WebSocket server error: ${err.message}`))
                    setInterval(() => this._checkHeartbeat(), HEARTBEAT_INTERVAL_MS).unref()
                    log(`Waiting for a metamath-lamp tab on ws://127.0.0.1:${this.port}`)
                    resolve(undefined)
                })
                wss.once('error', err => {
                    // This handler is meant only for bind errors.
                    // But 'once' removes it only after it fires, so after a successful bind it stays registered
                    // and would also get the first error of the working server.
                    // this.wss === wss means this server is bound: leave the error to the logger
                    // added in the 'listening' handler, and don't close a working server.
                    if (this.wss === wss) {
                        return
                    }
                    this.listeningPromise = null
                    wss.close()
                    const errMsg = err.code === 'EADDRINUSE'
                        ? `Port ${this.port} is already in use by another program `
                            + `(probably the mm-lamp MCP server of another agent session). `
                            + `Close that session, or set MM_LAMP_BRIDGE_PORT to use a different port.`
                        : `Could not start the WebSocket server: ${err.message}`
                    log(errMsg)
                    resolve(errMsg)
                })
                wss.on('connection', (ws, req) => this._onConnection(ws, req))
                httpServer.listen(this.port, '127.0.0.1')
            })
        }
        return this.listeningPromise
    }

    /*
    * Makes sure a browser tab is connected, waiting up to TAB_CONNECT_WAIT_MS for it to (re)connect.
    * Resolves to undefined if a tab is connected, otherwise to an error message.
    * */
    async ensureConnected() {
        const listenErr = await this._ensureListening()
        if (listenErr !== undefined) {
            return listenErr
        }
        if (this.socket === null) {
            await new Promise(resolve => {
                const timer = setTimeout(() => {
                    this.tabWaiters = this.tabWaiters.filter(w => w !== waiter)
                    resolve()
                }, TAB_CONNECT_WAIT_MS)
                const waiter = () => {
                    clearTimeout(timer)
                    resolve()
                }
                this.tabWaiters.push(waiter)
            })
        }
        return this._getNotConnectedError()
    }

    _isOriginAllowed(origin) {
        if (origin === undefined) {
            // Non-browser clients don't send Origin. Browsers always do, so web pages cannot get here.
            return true
        }
        return isOriginAllowed(origin, this.allowedOrigins)
    }

    async _onHttpRequest(req, res) {
        try {
            const handled = this.httpHandler !== undefined && await this.httpHandler(req, res)
            if (!handled) {
                res.writeHead(404, {'Content-Type': 'text/plain; charset=utf-8'})
                res.end('Not found')
            }
        } catch (e) {
            log(`Error handling HTTP request ${req.method} ${req.url}: ${e?.message ?? e}`)
            if (!res.headersSent) {
                res.writeHead(500, {'Content-Type': 'text/plain; charset=utf-8'})
            }
            res.end()
        }
    }

    _onConnection(ws, req) {
        const origin = req.headers.origin
        if (!this._isOriginAllowed(origin)) {
            log(`Rejected a connection from origin ${origin}`)
            ws.close(CLOSE_CODE_ORIGIN_NOT_ALLOWED, 'Origin is not allowed')
            return
        }
        if (this.socket !== null) {
            ws.close(CLOSE_CODE_ANOTHER_TAB_CONNECTED, 'Another metamath-lamp tab is already connected')
            return
        }
        this.socket = ws
        const waiters = this.tabWaiters
        this.tabWaiters = []
        waiters.forEach(waiter => waiter())
        ws.isAlive = true
        ws.on('pong', () => { ws.isAlive = true })
        ws.on('message', data => this._onMessage(ws, data))
        ws.on('close', () => {
            if (this.socket === ws) {
                log('The metamath-lamp tab disconnected.')
                this.socket = null
                this._rejectAllPending('The metamath-lamp tab disconnected before responding.')
            }
        })
        ws.on('error', err => log(`WebSocket error: ${err.message}`))
    }

    _onMessage(ws, data) {
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

    _checkHeartbeat() {
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

    _rejectAllPending(errMsg) {
        for (const pending of this.pendingCalls.values()) {
            clearTimeout(pending.timer)
            pending.resolve({isOk: false, err: errMsg})
        }
        this.pendingCalls.clear()
    }

    /*
    * Returns an error message if no browser tab is connected, otherwise undefined.
    * */
    _getNotConnectedError() {
        if (this.socket !== null) {
            return undefined
        }
        return 'No metamath-lamp tab is connected to the MCP server. '
            + 'Ask the user to open metamath-lamp and run the "Connect to MCP server" macro.'
    }

    /*
    * Calls a metamath-lamp API function in the connected browser tab.
    * path - e.g. "editor.getState" or "editor.unifyAll".
    * editorId - only for "editor.*" paths; undefined means the last opened editor tab.
    * Resolves to the API response {isOk, res, err}. Never rejects.
    * */
    call({path, params, editorId, timeoutMs}) {
        const notConnectedErr = this._getNotConnectedError()
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
