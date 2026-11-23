# metamath-lamp MCP server

An MCP server which lets AI agents work with metamath-lamp opened in a browser tab.

```
Agent  --MCP over stdio-->  server.js  --WebSocket 127.0.0.1:8765-->  browser tab (macro)  -->  window.api
```

The browser tab does all the work.
The MCP server only forwards calls to the tab and formats the results for the agent.

## Tools

* `get_editor_state` - returns the content of an editor tab: variables, disjoints, and steps with statuses,
  justifications and errors.
  By default it runs "Unify All" first, so statuses are up to date.
* `add_steps` - adds steps (and optionally new variables) before or after a given step,
  or before the goal step by default.
  Added steps are bookmarked.
  Returns the editor state after "Unify All".
* `update_steps` - changes the statement, justification, type, or bookmark of existing steps, identified by labels.
  Only the passed attributes are changed.
  Returns the editor state after "Unify All".
* `delete_steps` - deletes steps by labels.
  If any label doesn't exist, nothing is deleted.
  Returns the editor state after "Unify All".

## Setup

1. Install dependencies:
   ```
   cd mcp-bridge
   npm install
   ```
2. Register the server in your agent.
   For Claude Code:
   ```
   claude mcp add mm-lamp -- node /absolute/path/to/metamath-lamp/mcp-bridge/server.js
   ```
3. In metamath-lamp, open the macros dialog, create a new macro module,
   paste the content of `mm-lamp-macro.js` into it and make the module active.
4. Load a context, open an editor tab, and run the "Connect to MCP server" macro.
   The tab keeps reconnecting until "Disconnect from MCP server" is run,
   so it doesn't matter whether the agent session starts before or after this step.

Keep the metamath-lamp tab visible (e.g. in a separate window),
because browsers slow down background tabs.

## Configuration

Environment variables of `server.js`:

* `MM_LAMP_BRIDGE_PORT` - the WebSocket port, `8765` by default.
  If you change it, change `PORT` in the macro too.
* `MM_LAMP_BRIDGE_ORIGINS` - comma separated list of additional page origins allowed to connect.
  `http(s)://localhost:*`, `http(s)://127.0.0.1:*` and `https://expln.github.io` are always allowed.

Set `AUTO_CONNECT = true` in the macro to connect automatically when the page loads.

## Limitations

* Only one browser tab can be connected at a time.
  Other tabs keep retrying and take over when the connected tab disconnects.
* Only one agent session can use the default port at a time.
  A second session reports that the port is in use.
* Loading a context and opening an editor tab must be done manually.
