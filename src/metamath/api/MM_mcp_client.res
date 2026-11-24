// Connection of this browser tab to the metamath-lamp MCP server (mcp-bridge/server.js).
// The MCP server forwards tool calls to this tab, and this tab executes them via MM_api.api.
// The state is kept on the module level. The MCP tab disconnects when it is closed (see MM_cmp_root.res).
// See the protocol description in mcp-bridge/browser-bridge.js.

let serverHostUrl = "ws://127.0.0.1"
let defaultPort = 8765

let protocolVersion = 1
let reconnectDelayMs = 3000
let closeCodeAnotherTabConnected = 4001
let closeCodeOriginNotAllowed = 4003
let closeCodeProtocolMismatch = 4004
let wsReadyStateOpen = 1

type webSocket
type closeEvent = {code:int, reason:string}
type messageEvent = {data:string}

@new external newWebSocket: string => webSocket = "WebSocket"
@set external setOnOpen: (webSocket, unit => unit) => unit = "onopen"
@set external setOnMessage: (webSocket, messageEvent => unit) => unit = "onmessage"
@set external setOnClose: (webSocket, closeEvent => unit) => unit = "onclose"
@get external readyState: webSocket => int = "readyState"
@send external wsSend: (webSocket, string) => unit = "send"
@send external wsClose: webSocket => unit = "close"
@val @scope(("window", "location")) external locationHref: string = "href"

type status =
    | Disconnected
    | Connecting
    | WaitingForServer
    | WaitingForOtherTab(string)
    | Connected
    | Failed(string)

type callMsg = {
    @as("type") type_: string,
    id: JSON.t,
    path: string,
    tabId: Nullable.t<string>,
    params: MM_api.apiInput,
}

type state = {
    mutable enabled: bool,
    mutable port: int,
    mutable socket: option<webSocket>,
    mutable reconnectTimer: option<timeoutId>,
    mutable status: status,
    mutable listeners: array<status=>unit>,
}

let state = {
    enabled: false,
    port: defaultPort,
    socket: None,
    reconnectTimer: None,
    status: Disconnected,
    listeners: [],
}

let getStatus = ():status => state.status
let getPort = ():int => state.port

/*
* Registers a listener of status changes. Returns a function which unregisters the listener.
* */
let subscribe = (listener:status=>unit):(unit=>unit) => {
    state.listeners = state.listeners->Array.concat([listener])
    () => {
        state.listeners = state.listeners->Array.filter(l => l !== listener)
    }
}

let setStatus = (status:status):unit => {
    state.status = status
    state.listeners->Array.forEach(listener => listener(status))
}

let errResp = (msg:string):JSON.t => {
    JSON.Encode.object(Dict.fromArray([
        ("isOk", JSON.Encode.bool(false)),
        ("err", JSON.Encode.string(msg)),
    ]))
}

/*
* Finds an API function by its path, e.g. "editor.getState" -> api.editor(tabId).getState
* */
let resolveApiFunc: (
    {..}, string, Nullable.t<string>
) => Nullable.t<MM_api.apiInput => promise<MM_api.apiResp>> = %raw(`
    (api, path, tabId) => {
        const [namespace, ...rest] = path.split('.')
        let obj = namespace === 'editor' ? api.editor(tabId ?? undefined) : api[namespace]
        for (const name of rest) {
            obj = obj?.[name]
        }
        return typeof obj === 'function' ? obj : undefined
    }
`)

let exnToMsg = (exn:exn):string => {
    switch exn {
        | Common.MmException({msg}) => msg
        | exn => Common.jsErrorToExnData(exn).msg
    }
}

let handleCall = async (msg:callMsg):JSON.t => {
    switch resolveApiFunc(MM_api.api, msg.path, msg.tabId)->Nullable.toOption {
        | None => errResp(`Unknown API function '${msg.path}'.`)
        | Some(func) => {
            try {
                let resp = await func(msg.params)
                resp->Obj.magic
            } catch {
                | exn => errResp(exnToMsg(exn))
            }
        }
    }
}

let send = (socket:webSocket, msg:JSON.t):unit => {
    if (socket->readyState == wsReadyStateOpen) {
        socket->wsSend(msg->JSON.stringify)
    }
}

let clearReconnectTimer = ():unit => {
    state.reconnectTimer->Option.forEach(clearTimeout)
    state.reconnectTimer = None
}

let rec connectSocket = ():unit => {
    if (state.enabled && state.socket->Option.isNone) {
        let port = state.port
        let socket = newWebSocket(`${serverHostUrl}:${port->Int.toString}`)
        state.socket = Some(socket)
        socket->setOnOpen(() => {
            send(socket, JSON.Encode.object(Dict.fromArray([
                ("type", JSON.Encode.string("hello")),
                ("client", JSON.Encode.string("mm-lamp")),
                ("version", JSON.Encode.int(protocolVersion)),
                ("url", JSON.Encode.string(locationHref)),
            ])))
            setStatus(Connected)
        })
        socket->setOnMessage(event => {
            let msg = try {
                Some(JSON.parseOrThrow(event.data)->(Obj.magic:JSON.t=>callMsg))
            } catch {
                | _ => None
            }
            switch msg {
                | Some(msg) if msg.type_ == "call" => {
                    handleCall(msg)->Promise.thenResolve(resp => {
                        send(socket, JSON.Encode.object(Dict.fromArray([
                            ("type", JSON.Encode.string("result")),
                            ("id", msg.id),
                            ("resp", resp),
                        ])))
                    })->ignore
                }
                | _ => ()
            }
        })
        socket->setOnClose(event => {
            if (state.socket->Option.mapOr(false, s => s === socket)) {
                state.socket = None
                if (event.code == closeCodeOriginNotAllowed || event.code == closeCodeProtocolMismatch) {
                    // Retrying won't help.
                    state.enabled = false
                    setStatus(Failed(event.reason))
                } else {
                    if (event.code == closeCodeAnotherTabConnected) {
                        setStatus(WaitingForOtherTab(event.reason))
                    } else {
                        setStatus(WaitingForServer)
                    }
                    scheduleReconnect()
                }
            }
        })
    }
}
and scheduleReconnect = ():unit => {
    clearReconnectTimer()
    if (state.enabled) {
        state.reconnectTimer = Some(setTimeout(connectSocket, reconnectDelayMs))
    }
}

/*
* Starts connecting to the MCP server on the given port.
* Keeps reconnecting until disconnect() is called.
* */
let connect = (~port:int):unit => {
    if (!state.enabled) {
        state.enabled = true
        state.port = port
        setStatus(Connecting)
        connectSocket()
    }
}

let disconnect = ():unit => {
    state.enabled = false
    clearReconnectTimer()
    let socket = state.socket
    state.socket = None
    socket->Option.forEach(wsClose)
    setStatus(Disconnected)
}
