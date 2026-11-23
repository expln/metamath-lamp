/*
* metamath-lamp macro module which connects the browser tab to the metamath-lamp MCP server (mcp-bridge/server.js).
* Paste this whole file as a new macro module in metamath-lamp and make the module active.
* Then run the "Connect to MCP server" macro.
* The tab keeps reconnecting until "Disconnect from MCP server" is run,
* so the MCP server may be started before or after connecting.
* */

const moduleName = 'MCP server'

// Must match MM_LAMP_BRIDGE_PORT of the MCP server.
const PORT = 8765

// Connect automatically when this macro module is loaded (e.g. on page reload).
const AUTO_CONNECT = false

const PROTOCOL_VERSION = 1
const RECONNECT_DELAY_MS = 3000
const LOG_PREFIX = '[mm-lamp MCP]'

// The state is kept on window, so it survives re-execution of this script after editing it.
const bridge = window.__mmLampMcpBridge ??= {socket: null, enabled: false, reconnectTimer: null, status: 'disconnected'}

function setStatus(status) {
    bridge.status = status
    console.log(`${LOG_PREFIX} ${status}`)
}

function errResp(err) {
    return {isOk: false, err: String(err)}
}

/*
* Finds an API function by its path, e.g. "editor.getState" -> api.editor(editorId).getState
* */
function resolveApiFunc(path, editorId) {
    const [namespace, ...rest] = path.split('.')
    let obj = namespace === 'editor' ? api.editor(editorId ?? undefined) : api[namespace]
    for (const name of rest) {
        obj = obj?.[name]
    }
    return typeof obj === 'function' ? obj : undefined
}

async function handleCall({path, editorId, params}) {
    const func = resolveApiFunc(path, editorId)
    if (func === undefined) {
        return errResp(`Unknown API function '${path}'.`)
    }
    try {
        return await func(params)
    } catch (e) {
        return errResp(e?.message ?? e)
    }
}

function send(socket, msg) {
    if (socket.readyState === WebSocket.OPEN) {
        socket.send(JSON.stringify(msg))
    }
}

function scheduleReconnect() {
    clearTimeout(bridge.reconnectTimer)
    if (bridge.enabled) {
        bridge.reconnectTimer = setTimeout(connect, RECONNECT_DELAY_MS)
    }
}

function connect() {
    if (!bridge.enabled || bridge.socket !== null) {
        return
    }
    const socket = new WebSocket(`ws://127.0.0.1:${PORT}`)
    bridge.socket = socket
    socket.onopen = () => {
        send(socket, {type: 'hello', client: 'mm-lamp-macro', version: PROTOCOL_VERSION, url: location.href})
        setStatus(`connected to ws://127.0.0.1:${PORT}`)
    }
    socket.onmessage = async event => {
        let msg
        try {
            msg = JSON.parse(event.data)
        } catch {
            return
        }
        if (msg.type === 'call') {
            send(socket, {type: 'result', id: msg.id, resp: await handleCall(msg)})
        }
    }
    socket.onclose = event => {
        if (bridge.socket !== socket) {
            return
        }
        bridge.socket = null
        if (event.code === 4003 || event.code === 4004) {
            // Origin not allowed or protocol mismatch: retrying won't help.
            bridge.enabled = false
            setStatus(`disconnected: ${event.reason}`)
            return
        }
        if (bridge.status.startsWith('connected')) {
            setStatus(`disconnected${event.reason ? ': ' + event.reason : ''}, will keep reconnecting`)
        } else if (event.code === 4001) {
            bridge.status = `waiting: ${event.reason}`
        } else {
            bridge.status = 'waiting for the MCP server to start'
        }
        scheduleReconnect()
    }
}

function disconnect() {
    bridge.enabled = false
    clearTimeout(bridge.reconnectTimer)
    const socket = bridge.socket
    bridge.socket = null
    socket?.close()
    setStatus('disconnected')
}

async function showInfoMsg(msg) {
    await api.showInfoMsg({msg})
}

async function connectMacro() {
    bridge.enabled = true
    connect()
    await showInfoMsg(
        `Connecting to the MCP server on port ${PORT}.\n`
        + `The tab will keep reconnecting until "Disconnect from MCP server" is run.\n`
        + `Keep this tab visible: browsers slow down background tabs.`
    )
}

async function disconnectMacro() {
    disconnect()
    await showInfoMsg('Disconnected from the MCP server.')
}

async function showStatusMacro() {
    await showInfoMsg(`MCP server connection: ${bridge.enabled ? bridge.status : 'disconnected'}`)
}

if (AUTO_CONNECT) {
    bridge.enabled = true
    connect()
}

function makeMacro(name, func) {
    return {name, run: func}
}

await api.macro.registerMacroModule({
    moduleName,
    macros: [
        makeMacro('Connect to MCP server', connectMacro),
        makeMacro('Disconnect from MCP server', disconnectMacro),
        makeMacro('Show MCP server connection status', showStatusMacro),
    ]
})
