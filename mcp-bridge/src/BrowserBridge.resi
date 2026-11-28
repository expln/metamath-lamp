// WebSocket server which a metamath-lamp browser tab connects to.
// See the protocol description in BrowserBridge.res.

/*
* Logs a message to stderr (stdout is reserved for the MCP protocol).
* */
let log: string => unit

/*
* Returns true if a browser page with this origin may connect to the bridge or download files from it.
* */
let isOriginAllowed: (string, array<string>) => bool

// The response of a metamath-lamp API function.
type apiResp = {
    isOk: bool,
    res?: JSON.t,
    err?: string,
}

/*
* Handles plain HTTP requests (not WebSocket upgrades).
* It resolves to false if it didn't handle the request; then 404 is returned.
* */
type httpHandler = (NodeApi.Http.request, NodeApi.Http.response) => promise<bool>

type t

type makeParams = {
    port: int,
    allowedOrigins: array<string>,
    // If undefined, no HTTP server is started, only a standalone WebSocket server.
    httpHandler?: httpHandler,
}

/*
* Creates a bridge. The port is opened lazily, on the first call of call.
* */
let make: makeParams => t

type callParams = {
    // e.g. "editor.getState" or "editor.unifyAll".
    path: string,
    params?: JSON.t,
    // Id of the tab the call is for, e.g. the editor tab for "editor.*" paths.
    tabId?: string,
    timeoutMs: int,
}

/*
* Calls a metamath-lamp API function in the connected browser tab.
* If no tab is connected, it first opens the port (if not yet open) and waits for a tab to (re)connect;
* if none connects in time, it resolves to an error response explaining why there is no connection.
* Resolves to the API response {isOk, res, err}. Never rejects.
* */
let call: (t, callParams) => promise<apiResp>
