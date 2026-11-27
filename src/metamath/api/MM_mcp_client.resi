// Connection of this browser tab to the metamath-lamp MCP server (mcp-bridge/src/server.js).
// The MCP server forwards tool calls to this tab, and this tab executes them via MM_api.api.
// There is at most one connection per browser tab.

// The URL of the MCP server without the port.
let serverHostUrl: string
let defaultPort: int

type status =
    | Disconnected
    | Connecting
    | WaitingForServer
    | WaitingForOtherTab(string)
    | Connected
    | Failed(string)

let getStatus: unit => status

/*
* Returns true if the MCP server may call the API function with this path, e.g. "editor.getState".
* Only the functions used by mcp-bridge/src/server.js are allowed.
* */
let isPathAllowed: string => bool

/*
* Returns the port of the current or the last connection.
* */
let getPort: unit => int

/*
* Registers a listener of status changes. Returns a function which unregisters the listener.
* */
let subscribe: (status=>unit) => (unit=>unit)

/*
* Starts connecting to the MCP server on the given port.
* Keeps reconnecting until disconnect() is called.
* Does nothing if this tab is already connected or connecting.
* */
let connect: (~port:int) => unit

let disconnect: unit => unit
