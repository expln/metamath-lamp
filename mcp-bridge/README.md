# metamath-lamp MCP server

An MCP server which lets AI agents work with metamath-lamp opened in a browser tab.

```
Agent  --MCP over stdio-->  server.js  --WebSocket 127.0.0.1:8765-->  browser tab (macro)  -->  window.api
```

The browser tab does all the work.
The MCP server only forwards calls to the tab and formats the results for the agent.
The same port also serves local files over HTTP, so the tab can load them as a context (see `load_context`).

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
* `find_assertions` - searches assertions by a pattern, excluding discouraged and deprecated ones.
  With `editorId`, only assertions which can be used in that editor tab are returned,
  otherwise the whole loaded context is searched.
  Results are paginated, 100 assertions per page.
  The full result of the last search is cached in the browser tab,
  so requesting other pages of the same pattern (in any editor scope) doesn't repeat the search.
* `get_pattern_syntax` - returns the full description of the pattern syntax (from `pattern-syntax.md`).
* `load_context` - replaces the loaded context with files given as http(s) URLs or local paths,
  optionally reading each file only up to `stopBefore` or `stopAfter` labels.
  Files included via `$[ ... $]` are loaded too.
  The browser downloads URLs directly.
  Local files are served by the MCP server at `http://127.0.0.1:8765/files/<absolute path>`,
  and only from directories listed in `MM_LAMP_FILES_DIRS`.

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
4. Open an editor tab and run the "Connect to MCP server" macro.
   Load a context manually, or let the agent load it with `load_context`.
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
  The same origins are allowed to download local files.
* `MM_LAMP_FILES_DIRS` - directories from which `load_context` may load local files,
  separated by `:` (`;` on Windows).
  Subdirectories are allowed too, and a leading `~` means the home directory.
  If not set, local files cannot be loaded.
  Symbolic links are resolved before checking, so a link inside an allowed directory
  cannot point to a file outside of them.
  For example:
  ```
  claude mcp add mm-lamp -e MM_LAMP_FILES_DIRS=~/metamath -- node /absolute/path/to/metamath-lamp/mcp-bridge/server.js
  ```

Set `AUTO_CONNECT = true` in the macro to connect automatically when the page loads.

## Trusted URLs

metamath-lamp asks the user to confirm downloading from URLs which are not marked as trusted,
and `load_context` waits for that confirmation.
To avoid the confirmation, choose "Don't ask for all URLs starting with ..." in the confirmation dialog,
or add trusted web sources in the metamath-lamp settings, for example:

* `http://127.0.0.1:8765/files/*` - all local files served by the MCP server.
* `https://us.metamath.org/metamath/*` - databases from the Metamath site.

A context loaded from local files keeps their `http://127.0.0.1:8765/files/...` URLs.
If metamath-lamp reloads such a context (e.g. when an editor state is restored),
the MCP server must be running at that moment.

## Limitations

* Only one browser tab can be connected at a time.
  Other tabs keep retrying and take over when the connected tab disconnects.
* Only one agent session can use the default port at a time.
  A second session reports that the port is in use.
* Opening an editor tab must be done manually.
