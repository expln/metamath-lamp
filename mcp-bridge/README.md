# metamath-lamp MCP server

An MCP server which lets AI agents work with metamath-lamp opened in a browser tab.

```
Agent  --MCP over stdio-->  src/server.js  --WebSocket 127.0.0.1:8765-->  browser tab (MCP tab)  -->  window.api
```

The browser tab does all the work.
The MCP server only forwards calls to the tab and formats the results for the agent.
The same port also serves local files over HTTP, so the tab can load them as a context (see `load_context`),
if allowed directories are configured.

## Tools

Tools which work with an editor tab take its id in the required `tabId` parameter.
Tab ids are random strings, e.g. `k3f9x2m7qa`.
Ids of editor tabs don't change when metamath-lamp is reloaded.
Editors created before tab ids were introduced keep their old numeric ids as strings, e.g. `0`.
Use `list_tabs` to find the ids.

Tools run one at a time, even if the agent calls several of them in parallel.
A tool starts only after the previous one has finished,
so the calls one tool makes to the browser tab never interleave with calls of another tool.
A slow tool (e.g. `load_context` waiting for the user to confirm downloads) delays all tools called after it.
`get_pattern_syntax` doesn't use the browser tab, so it never waits.
Each call to the browser tab has a timeout.
After a timeout the next call is sent, even though the tab may still be running the timed out one.

* `list_tabs` - returns all tabs, including Settings and Tabs, in the order they are displayed,
  as a JSON array of `{tabId, type, title, isVisible}`.
  `type` is one of `settings`, `tabs`, `editor`, `explorer`, `assertion`, `db-info`, `mcp`.
  `isVisible` is true only for the currently displayed tab.
* `create_editor_tab` - creates a new empty editor tab and returns its id as `{tabId}`.
  With `open` (true by default), the new tab becomes visible.
  `title` is optional, by default a title like `EDITOR [2]` is generated.
* `close_tab` - closes a tab by id without asking the user for confirmation.
  Closing an editor tab deletes its content.
  Settings, Tabs, and MCP tabs cannot be closed by this tool.
* `open_tab` - makes an existing tab visible, as if the user clicked it.
* `rename_tab` - changes the title of a tab.
  Settings and Tabs cannot be renamed.
* `get_editor_state` - returns the content of an editor tab: variables, disjoints, and steps with statuses,
  justifications and errors.
  By default it runs "Unify All" first, so statuses are up to date.
  The editor description is included only with `includeDescription` (false by default),
  because it may be long.
* `get_compressed_proof` - returns the compressed proof of a proved step as a Metamath `$p` statement,
  the same text as in the "Show completed proof" dialog, but without the proof table and with only the used hypotheses.
  The editor description is included as a comment only with `includeDescription` (false by default).
  By default it runs "Unify All" first.
* `add_steps` - adds steps (and optionally new variables) before or after a given step,
  or before the goal step by default.
  Added steps are not bookmarked unless `isBookmarked` is true for a step.
  Returns the editor state after "Unify All".
* `update_steps` - changes the statement, justification, type, or bookmark of existing steps, identified by labels.
  Only the passed attributes are changed.
  Returns the editor state after "Unify All".
* `delete_steps` - deletes steps by labels.
  If any label doesn't exist, nothing is deleted.
  Returns the editor state after "Unify All".
* `set_description` - replaces the description of an editor tab.
  An empty string clears it.
* `set_disjoints` - replaces all disjoint variable groups of an editor tab.
  An empty list removes all disjoints.
  Returns the editor state after "Unify All".
* `set_location` - sets which assertions can be used in an editor tab:
  those `before` a label, those up to and including (`after`) a label, or all of them (`last`).
  Returns the editor state after "Unify All".
* `find_assertions` - searches assertions by a pattern and/or labels, excluding discouraged and deprecated ones.
  `pattern`, `label`, and `strictLabel` are optional.
  With `editorTabId`, only assertions which can be used in that editor tab are returned,
  otherwise the whole loaded context is searched.
  With `label`, only assertions whose label contains at least one of its space-separated parts are returned.
  With `strictLabel`, only assertions whose label is equal to one of its space-separated labels are returned.
  Results are paginated, 20 assertions per page by default (`pageSize`, at most 100).
  The full result of the last search is cached in the browser tab,
  so requesting other pages of the same pattern and labels (in any editor scope) doesn't repeat the search.
* `get_pattern_syntax` - returns the full description of the pattern syntax (from `src/pattern-syntax.md`).
* `load_context` - replaces the loaded context with files given as http(s) URLs or local paths,
  optionally reading each file only up to `stopBefore` or `stopAfter` labels.
  Files included via `$[ ... $]` are loaded too.
  The browser downloads URLs directly.
  Local files are served by the MCP server at `http://127.0.0.1:8765/files/<absolute path>`,
  and only from directories listed in the `--files-dirs` argument.
* `get_loaded_context_info` - describes the loaded context: its sources in the order of loading,
  how each of them is read (the whole file, or up to a `stopBefore`/`stopAfter` label),
  and how many assertions (and axioms among them) are loaded in total, per source,
  and per file included via `$[ ... $]`.
  Local files served by the MCP server are shown by their local paths.

## Setup

1. Install dependencies:
   ```
   cd mcp-bridge
   npm install
   ```
   This also compiles the ReScript sources in `src` (the `prepare` script).
   After changing them, run `npm run compile`.
2. Register the server in your agent.
   For Claude Code:
   ```
   claude mcp add mm-lamp -- node /absolute/path/to/metamath-lamp/mcp-bridge/src/server.js
   ```
   The MCP tab of metamath-lamp (see the next step) can build this command for you,
   including the arguments described in [Configuration](#configuration).
3. In metamath-lamp, open the "Tabs" tab and click "MCP".
   In the MCP tab, check the port and click "Connect".
   Load a context manually, or let the agent load it with `load_context`.
   The browser tab keeps reconnecting until "Disconnect" is clicked,
   so it doesn't matter whether the agent session starts before or after this step.
   Closing the MCP tab disconnects, so keep it open while the agent works.

Keep the metamath-lamp tab visible (e.g. in a separate window),
because browsers slow down background tabs.

## Configuration

Command-line arguments of `src/server.js`.
If an option is given more than once, the last occurrence overrides the previous ones.
For example, `--files-dirs ~/a --files-dirs ~/b` allows only `~/b` (the lists are not merged).

* `--port <port>` - the WebSocket port, `8765` by default.
  If you change it, change the port in the MCP tab of metamath-lamp too.
* `--origins <origins>` - comma-separated list of additional page origins allowed to connect.
  `http(s)://localhost:*`, `http(s)://127.0.0.1:*` and `https://expln.github.io` are always allowed.
  The same origins are allowed to download local files.
* `--files-dirs <dirs>` - directories from which `load_context` may load local files,
  separated by `:` (`;` on Windows).
  Subdirectories are allowed too, and a leading `~` means the home directory.
  Entries which are not existing directories are ignored with a warning on stderr.
  If no valid directories remain, local files cannot be loaded,
  and the port serves only the WebSocket (no HTTP server is started).
  Symbolic links are resolved before checking, so a link inside an allowed directory
  cannot point to a file outside of them.
  For example:
  ```
  claude mcp add mm-lamp -- node /absolute/path/to/metamath-lamp/mcp-bridge/src/server.js --files-dirs ~/metamath
  ```

* `--log-file <path>` - a file to which all MCP requests of the agent and the responses to them are appended,
  see [Request log](#request-log).
  A leading `~` means the home directory.
  If omitted, nothing is logged.
  If the file cannot be opened (e.g. its directory doesn't exist), the server doesn't start.

## Request log

The request log shows how the agent uses the tools.
It is meant to be read by a human.
Each run of the server starts with a line of `#` characters, so several runs can be appended to one file.
Each entry starts with a line of `=` characters.
An entry is a request together with its response, or a single notification.

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
