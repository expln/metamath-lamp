// WebSocket server which a metamath-lamp browser tab connects to.
// The browser side is src/metamath/api/MM_mcp_client.res (controlled from the MCP tab of metamath-lamp).
// If an HTTP handler is given, the WebSocket server runs on top of an HTTP server,
// which also serves other HTTP requests (e.g. local files, see FileServer.res) on the same port.
// Otherwise a standalone WebSocket server is started.
//
// Protocol (all messages are JSON):
//   browser -> relay  {type:"hello", client:string, version:number, url:string}
//   relay   -> browser {type:"call", id:number, path:string, tabId?:string, params?:any}
//   browser -> relay  {type:"result", id:number, resp:{isOk:boolean, res?:any, err?:string}}

open NodeApi

let protocolVersion = 1
let heartbeatIntervalMs = 15_000
// How long a tool call waits for a tab to (re)connect.
// The browser tab retries every 3 s (reconnectDelayMs in MM_mcp_client.res),
// so this is enough for a tab to reconnect while the port is open.
let tabConnectWaitMs = 5000.
// How long after the port is opened tool calls keep waiting for a tab to connect.
// The port is opened lazily (see ensureListening), so a tab may have been failing to connect for a while before,
// and Firefox delays reconnects to a failing server (network.websocket.delay-failed-reconnects)
// by up to a minute, so the tab may connect much later than in 3 s.
let firstTabConnectWaitMs = 65_000.
let closeCodeAnotherTabConnected = 4001
let closeCodeOriginNotAllowed = 4003
let closeCodeProtocolMismatch = 4004

let log = (msg:string):unit => {
    // stdout is reserved for the MCP protocol, so all logging goes to stderr.
    Console.error(`[mm-lamp-bridge] ${msg}`)
}

// Bindings to the ws package.
module Ws = {
    type webSocket
    type rawData
    @send external rawDataToString: rawData => string = "toString"

    @send external onPong: (webSocket, @as("pong") _, unit => unit) => unit = "on"
    @send external onMessage: (webSocket, @as("message") _, rawData => unit) => unit = "on"
    @send external onClose: (webSocket, @as("close") _, unit => unit) => unit = "on"
    @send external onError: (webSocket, @as("error") _, nodeError => unit) => unit = "on"
    @send external close: (webSocket, int, string) => unit = "close"
    @send external terminate: webSocket => unit = "terminate"
    @send external ping: webSocket => unit = "ping"
    @send external send: (webSocket, string) => unit = "send"

    type server
    type serverOptions = {
        host?: string,
        port?: int,
        server?: Http.server,
    }
    @module("ws") @new external makeServer: serverOptions => server = "WebSocketServer"
    @send external serverOnceListening: (server, @as("listening") _, unit => unit) => unit = "once"
    @send external serverOnceError: (server, @as("error") _, nodeError => unit) => unit = "once"
    @send external serverOnError: (server, @as("error") _, nodeError => unit) => unit = "on"
    @send external serverOnConnection: (server, @as("connection") _, (webSocket, Http.request) => unit) => unit = "on"
    @send external closeServer: server => unit = "close"
}

let isLocalhostOrigin = (origin:string):bool => {
    try {
        let url = Url.make(origin)
        let protocol = url->Url.protocol
        let hostname = url->Url.hostname
        (protocol == "http:" || protocol == "https:")
            && (hostname == "localhost" || hostname == "127.0.0.1" || hostname == "[::1]")
    } catch {
        | _ => false
    }
}

let isOriginAllowed = (origin:string, allowedOrigins:array<string>):bool => {
    isLocalhostOrigin(origin) || allowedOrigins->Array.includes(origin)
}

type apiResp = {
    isOk: bool,
    res?: JSON.t,
    err?: string,
}

type httpHandler = (Http.request, Http.response) => promise<bool>

type pendingCall = {
    resolve: apiResp => unit,
    timer: timeoutId,
}

type connection = {
    ws: Ws.webSocket,
    // Reset on each heartbeat and set again when the tab answers the ping.
    mutable isAlive: bool,
}

type t = {
    port: int,
    allowedOrigins: array<string>,
    httpHandler: option<httpHandler>,
    mutable listeningPromise: option<promise<option<string>>>,
    mutable wss: option<Ws.server>,
    // When the port was opened, see firstTabConnectWaitMs.
    mutable listeningSince: option<float>,
    mutable tabWaiters: array<unit => unit>,
    mutable connection: option<connection>,
    mutable nextCallId: int,
    pendingCalls: Map.t<int, pendingCall>,
}

type makeParams = {
    port: int,
    allowedOrigins: array<string>,
    httpHandler?: httpHandler,
}

let make = (params:makeParams):t => {
    {
        port: params.port,
        allowedOrigins: params.allowedOrigins,
        httpHandler: params.httpHandler,
        listeningPromise: None,
        wss: None,
        listeningSince: None,
        tabWaiters: [],
        connection: None,
        nextCallId: 1,
        pendingCalls: Map.make(),
    }
}

let isConnectionOf = (t:t, ws:Ws.webSocket):bool => {
    t.connection->Option.mapOr(false, conn => conn.ws === ws)
}

let isOriginAllowedForWs = (t:t, origin:option<string>):bool => {
    switch origin {
        // Non-browser clients don't send Origin. Browsers always do, so web pages cannot get here.
        | None => true
        | Some(origin) => isOriginAllowed(origin, t.allowedOrigins)
    }
}

let onHttpRequest = async (handler:httpHandler, req:Http.request, res:Http.response):unit => {
    try {
        let handled = await handler(req, res)
        if (!handled) {
            res->Http.writeHead(404, Dict.fromArray([("Content-Type", "text/plain; charset=utf-8")]))
            res->Http.endWithText("Not found")
        }
    } catch {
        | exn => {
            log(`Error handling HTTP request ${req->Http.method} ${req->Http.url}: ${exnToMsg(exn)}`)
            if (!(res->Http.headersSent)) {
                res->Http.writeHead(500, Dict.fromArray([("Content-Type", "text/plain; charset=utf-8")]))
            }
            res->Http.end
        }
    }
}

let rejectAllPending = (t:t, errMsg:string):unit => {
    t.pendingCalls->Map.forEach(pending => {
        clearTimeout(pending.timer)
        pending.resolve({isOk: false, err: errMsg})
    })
    t.pendingCalls->Map.clear
}

// A message from the browser. Fields are optional, because the browser may send anything.
type incomingMsg = {
    @as("type") type_?: string,
    version?: JSON.t,
    url?: string,
    id?: int,
    resp?: apiResp,
}

let onMessage = (t:t, ws:Ws.webSocket, data:Ws.rawData):unit => {
    let json = try {
        Some(JSON.parseOrThrow(data->Ws.rawDataToString))
    } catch {
        | _ => None
    }
    switch json {
        | None => log("Received a non-JSON message from the browser, ignoring it.")
        | Some(Object(_) as json) => {
            let msg = json->(Obj.magic: JSON.t => incomingMsg)
            switch msg.type_ {
                | Some("hello") => {
                    switch msg.version {
                        | Some(Number(version)) if version == protocolVersion->Int.toFloat =>
                            log(`A metamath-lamp tab connected: ${msg.url->anyToString}`)
                        | _ => {
                            ws->Ws.close(
                                closeCodeProtocolMismatch,
                                `Protocol version mismatch: relay=${protocolVersion->Int.toString}, `
                                    ++ `tab=${msg.version->anyToString}`
                            )
                        }
                    }
                }
                | Some("result") => {
                    switch msg.id->Option.flatMap(id => t.pendingCalls->Map.get(id)->Option.map(p => (id, p))) {
                        | None => ()
                        | Some((id, pending)) => {
                            t.pendingCalls->Map.delete(id)->ignore
                            clearTimeout(pending.timer)
                            pending.resolve(
                                msg.resp->Option.getOr({
                                    isOk: false,
                                    err: "metamath-lamp sent a result without a response.",
                                })
                            )
                        }
                    }
                }
                | _ => ()
            }
        }
        // Other JSON values (numbers, strings, arrays, null) are not messages of the protocol.
        | Some(_) => ()
    }
}

let onConnection = (t:t, ws:Ws.webSocket, req:Http.request):unit => {
    let origin = req->Http.headers->Dict.get("origin")
    if (!isOriginAllowedForWs(t, origin)) {
        log(`Rejected a connection from origin ${origin->anyToString}`)
        ws->Ws.close(closeCodeOriginNotAllowed, "Origin is not allowed")
    } else if (t.connection->Option.isSome) {
        ws->Ws.close(closeCodeAnotherTabConnected, "Another metamath-lamp tab is already connected")
    } else {
        let conn = {ws, isAlive: true}
        t.connection = Some(conn)
        let waiters = t.tabWaiters
        t.tabWaiters = []
        waiters->Array.forEach(waiter => waiter())
        ws->Ws.onPong(() => conn.isAlive = true)
        ws->Ws.onMessage(data => onMessage(t, ws, data))
        ws->Ws.onClose(() => {
            if (isConnectionOf(t, ws)) {
                log("The metamath-lamp tab disconnected.")
                t.connection = None
                rejectAllPending(t, "The metamath-lamp tab disconnected before responding.")
            }
        })
        ws->Ws.onError(err => log(`WebSocket error: ${err.message}`))
    }
}

let checkHeartbeat = (t:t):unit => {
    switch t.connection {
        | None => ()
        | Some(conn) => {
            if (!conn.isAlive) {
                log("The metamath-lamp tab stopped responding, dropping the connection.")
                conn.ws->Ws.terminate
            } else {
                conn.isAlive = false
                conn.ws->Ws.ping
            }
        }
    }
}

/*
* Starts listening on the port, unless already listening.
* This is done lazily (on the first tool call) rather than on startup,
* because agent clients may start idle copies of MCP servers (e.g. pre-warmed sessions),
* and an idle copy must not take the port from the session which is actually used.
* Resolves to None on success or to an error message. If the port is busy, the next call tries again.
* */
let ensureListening = (t:t):promise<option<string>> => {
    switch (t.wss, t.listeningPromise) {
        | (Some(_), _) => Promise.resolve(None)
        | (None, Some(listeningPromise)) => listeningPromise
        | (None, None) => {
            let listeningPromise = Promise.make((resolve, _) => {
                // With an HTTP server, the WebSocket server re-emits its 'listening' and 'error' events,
                // so the handlers below work the same as for a standalone WebSocket server.
                let httpServer = t.httpHandler->Option.map(handler => {
                    Http.createServer((req, res) => onHttpRequest(handler, req, res)->ignore)
                })
                let wss = switch httpServer {
                    | None => Ws.makeServer({host: "127.0.0.1", port: t.port})
                    | Some(httpServer) => Ws.makeServer({server: httpServer})
                }
                wss->Ws.serverOnceListening(() => {
                    // A bound server is never closed: it is supposed to work until the process exits.
                    // The process exits only when the MCP client closes stdin, i.e. when the agent session ends
                    // (see process.exit() at the end of src/server.js).
                    // Errors after binding don't stop the server, they are only logged by the handler below.
                    // That's why t.wss is never reset to None, and why the single heartbeat timer is never
                    // stored or cleared.
                    // If a bound server is ever closed (e.g. to free the port when the tab disconnects),
                    // reset t.wss to None on its 'close' event, so the next tool call binds again,
                    // and store the heartbeat timer to clear it, so timers don't accumulate on rebinding.
                    t.wss = Some(wss)
                    t.listeningSince = Some(Date.now())
                    t.listeningPromise = None
                    wss->Ws.serverOnError(err => log(`WebSocket server error: ${err.message}`))
                    setInterval(() => checkHeartbeat(t), heartbeatIntervalMs)->unrefInterval->ignore
                    log(`Waiting for a metamath-lamp tab on ws://127.0.0.1:${t.port->Int.toString}`)
                    resolve(None)
                })
                wss->Ws.serverOnceError(err => {
                    // This handler is meant only for bind errors.
                    // But 'once' removes it only after it fires, so after a successful bind it stays registered
                    // and would also get the first error of the working server.
                    // t.wss === wss means this server is bound: leave the error to the logger
                    // added in the 'listening' handler, and don't close a working server.
                    if (!(t.wss->Option.mapOr(false, w => w === wss))) {
                        t.listeningPromise = None
                        wss->Ws.closeServer
                        let errMsg = if (err.code == Some("EADDRINUSE")) {
                            `Port ${t.port->Int.toString} is already in use by another program `
                                ++ `(probably the mm-lamp MCP server of another agent session). `
                                ++ `Close that session, or use the --port argument to choose a different port.`
                        } else {
                            `Could not start the WebSocket server: ${err.message}`
                        }
                        log(errMsg)
                        resolve(Some(errMsg))
                    }
                })
                wss->Ws.serverOnConnection((ws, req) => onConnection(t, ws, req))
                httpServer->Option.forEach(httpServer => httpServer->Http.listen(t.port, "127.0.0.1"))
            })
            t.listeningPromise = Some(listeningPromise)
            listeningPromise
        }
    }
}

/*
* Returns an error message if no browser tab is connected, otherwise None.
* */
let getNotConnectedError = (t:t):option<string> => {
    switch t.connection {
        | Some(_) => None
        | None => Some(
            "No metamath-lamp tab is connected to the MCP server. "
                ++ `Ask the user to open metamath-lamp, open the MCP tab (via the "Tabs" tab) and click "Connect".`
        )
    }
}

/*
* Waits until a browser tab connects or waitMs elapses.
* */
let waitForTab = (t:t, waitMs:float):promise<unit> => {
    Promise.make((resolve, _) => {
        let timer = ref(None)
        let waiter = () => {
            timer.contents->Option.forEach(clearTimeout)
            resolve()
        }
        timer := Some(setTimeout(() => {
            t.tabWaiters = t.tabWaiters->Array.filter(w => w !== waiter)
            resolve()
        }, waitMs->Float.toInt))
        t.tabWaiters->Array.push(waiter)
    })
}

let ensureConnected = async (t:t):option<string> => {
    switch await ensureListening(t) {
        | Some(listenErr) => Some(listenErr)
        | None => {
            if (t.connection->Option.isNone) {
                let waitMs = t.listeningSince->Option.mapOr(tabConnectWaitMs, listeningSince => {
                    Math.max(tabConnectWaitMs, listeningSince +. firstTabConnectWaitMs -. Date.now())
                })
                await waitForTab(t, waitMs)
            }
            getNotConnectedError(t)
        }
    }
}

type callParams = {
    path: string,
    params?: JSON.t,
    tabId?: string,
    timeoutMs: int,
}

type callMsg = {
    @as("type") type_: string,
    id: int,
    path: string,
    tabId?: string,
    params?: JSON.t,
}

let call = (t:t, {path, ?params, ?tabId, timeoutMs}:callParams):promise<apiResp> => {
    switch t.connection {
        | None => Promise.resolve({isOk: false, err: ?getNotConnectedError(t)})
        | Some(conn) => {
            let id = t.nextCallId
            t.nextCallId = id + 1
            Promise.make((resolve, _) => {
                let timer = setTimeout(() => {
                    t.pendingCalls->Map.delete(id)->ignore
                    resolve({
                        isOk: false,
                        err: `metamath-lamp did not respond to '${path}' within ${timeoutMs->Int.toString} ms.`,
                    })
                }, timeoutMs)
                t.pendingCalls->Map.set(id, {resolve, timer})
                let msg:callMsg = {type_: "call", id, path, ?tabId, ?params}
                conn.ws->Ws.send(stringifyAny(msg))
            })
        }
    }
}
