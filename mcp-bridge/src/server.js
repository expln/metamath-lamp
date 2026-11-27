#!/usr/bin/env node
// MCP server (stdio) which lets AI agents work with metamath-lamp opened in a browser tab.
// The browser tab connects to this process over a WebSocket from the MCP tab of metamath-lamp.
//
// Command-line arguments (if an option is repeated, the last occurrence overrides the previous ones):
//   --port <port>         - WebSocket port to listen on (127.0.0.1 only). Default: 8765.
//   --origins <origins>   - comma separated list of additional allowed page origins.
//                           http(s)://localhost:* and https://expln.github.io are always allowed.
//   --files-dirs <dirs>   - directories from which local files may be loaded by load_context,
//                           separated by ':' (';' on Windows). Entries which are not existing directories
//                           are ignored. If no valid directories remain, local files cannot be loaded,
//                           and no HTTP server is started (only the WebSocket server).

import { readFileSync } from 'node:fs'
import { parseArgs } from 'node:util'
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js'
import { z } from 'zod'
import * as BrowserBridge from './BrowserBridge.res.mjs'
import * as SerialQueue from './SerialQueue.res.mjs'
import { assertionToText, editorStateToText, getInsertIdx, loadedContextInfoToText } from './DataTransform.res.mjs'
import * as FileServer from './FileServer.res.mjs'

const DEFAULT_PORT = 8765
const DEFAULT_ALLOWED_ORIGINS = ['https://expln.github.io']
const UNIFY_ALL_TIMEOUT_MS = 60_000
const GET_STATE_TIMEOUT_MS = 30_000
const GET_COMPRESSED_PROOF_TIMEOUT_MS = 30_000
const LIST_TABS_TIMEOUT_MS = 10_000
const CREATE_EDITOR_TAB_TIMEOUT_MS = 30_000
const CLOSE_TAB_TIMEOUT_MS = 10_000
const OPEN_TAB_TIMEOUT_MS = 10_000
const RENAME_TAB_TIMEOUT_MS = 10_000
const ADD_STEPS_TIMEOUT_MS = 30_000
const UPDATE_STEPS_TIMEOUT_MS = 30_000
const DELETE_STEPS_TIMEOUT_MS = 30_000
const SET_DESCRIPTION_TIMEOUT_MS = 10_000
const SET_DISJOINTS_TIMEOUT_MS = 10_000
const SET_LOCATION_TIMEOUT_MS = 10_000
const GET_LOADED_CONTEXT_INFO_TIMEOUT_MS = 10_000
const FIND_ASSERTIONS_TIMEOUT_MS = 120_000
const FIND_ASSERTIONS_DEFAULT_PAGE_SIZE = 20
const FIND_ASSERTIONS_MAX_PAGE_SIZE = 100
// Includes downloading and parsing the files, loading the context,
// and waiting for the user to confirm downloading from untrusted URLs.
const LOAD_CONTEXT_TIMEOUT_MS = 15 * 60_000

const PATTERN_SYNTAX = readFileSync(new URL('./pattern-syntax.md', import.meta.url), 'utf8')

function parsePort(str) {
    if (str === undefined || str === '') {
        return DEFAULT_PORT
    }
    const port = Number(str)
    if (!Number.isInteger(port) || port < 1 || port > 65535) {
        BrowserBridge.log(`Invalid --port: '${str}'`)
        process.exit(1)
    }
    return port
}

const CLI_OPTIONS = {
    port: {type: 'string'},
    origins: {type: 'string'},
    'files-dirs': {type: 'string'},
}

/*
* Parses the command-line arguments. Exits the process if they are invalid.
* If an option is given more than once, the last occurrence wins.
* */
function parseCliArgs(args) {
    try {
        return parseArgs({args, options: CLI_OPTIONS, strict: true, allowPositionals: false}).values
    } catch (e) {
        BrowserBridge.log(`Invalid command-line arguments: ${e.message}`)
        process.exit(1)
    }
}

const cliArgs = parseCliArgs(process.argv.slice(2))
const port = parsePort(cliArgs.port)
const allowedOrigins = [
    ...DEFAULT_ALLOWED_ORIGINS,
    ...(cliArgs.origins ?? '').split(',').map(s => s.trim()).filter(s => s.length > 0),
]
const allowedDirs = FileServer.parseAllowedDirs(cliArgs['files-dirs'])
// undefined if loading local files is disabled.
const fileServer = allowedDirs.length === 0 ? undefined : FileServer.make({
    allowedDirs,
    isOriginAllowed: origin => BrowserBridge.isOriginAllowed(origin, allowedOrigins),
})
const fileServerBaseUrl = `http://127.0.0.1:${port}`
const bridge = BrowserBridge.make({
    port,
    allowedOrigins,
    httpHandler: fileServer === undefined
        ? undefined
        : (req, res) => FileServer.handleRequest(fileServer, req, res),
})

/*
* Calls a metamath-lamp API function in the connected browser tab, see BrowserBridge.call.
* */
function callBridge(params) {
    return BrowserBridge.call(bridge, params)
}

function textResult(text) {
    return {content: [{type: 'text', text}]}
}

function errorResult(text) {
    return {content: [{type: 'text', text}], isError: true}
}

// Tool handlers run one at a time, see exclusive.
const handlerQueue = SerialQueue.make()

/*
* Wraps a tool handler, so it runs only after all previously started handlers are finished.
* Many tools make several calls to the browser tab, e.g. read the editor state, change it based on what was read,
* then unify and read the state again. Calls of a tool running in parallel must not get in between.
* BrowserBridge.call also sends calls one at a time, but that doesn't keep the calls of one tool together.
* */
function exclusive(handler) {
    return (args, extra) => SerialQueue.run(handlerQueue, () => handler(args, extra))
}

/*
* Lists all tabs of metamath-lamp in the order they are displayed.
* Resolves to the API response: res is [{tabId, type, title, isVisible}].
* */
function listTabs() {
    return callBridge({path: 'tabs.list', timeoutMs: LIST_TABS_TIMEOUT_MS})
}

/*
* Creates a new empty editor tab.
* open - whether to make the new tab visible.
* title - the title of the new tab; undefined means a default title.
* Resolves to the API response: res is the id of the new tab.
* */
function createEditorTab({open, title}) {
    return callBridge({
        path: 'tabs.createEditorTab',
        params: {open, title},
        timeoutMs: CREATE_EDITOR_TAB_TIMEOUT_MS,
    })
}

/*
* Closes a tab without asking the user for a confirmation.
* Resolves to the API response: res is true if the tab was closed.
* */
function closeTab({tabId}) {
    return callBridge({
        path: 'tabs.close',
        params: {tabId, askConfirmation: false},
        timeoutMs: CLOSE_TAB_TIMEOUT_MS,
    })
}

/*
* Makes a tab visible.
* Resolves to the API response: res is null.
* */
function openTab({tabId}) {
    return callBridge({path: 'tabs.open', params: {tabId}, timeoutMs: OPEN_TAB_TIMEOUT_MS})
}

/*
* Changes the title of a tab.
* Resolves to the API response: res is null.
* */
function renameTab({tabId, title}) {
    return callBridge({path: 'tabs.rename', params: {tabId, title}, timeoutMs: RENAME_TAB_TIMEOUT_MS})
}

/*
* Runs 'Unify All' in an editor tab.
* Resolves to the API response: res is true if 'Unify All' ran, false if it could not start.
* The compressed proof dialog is suppressed, so it does not pop up in the browser when the goal gets proved.
* */
function unifyAll({tabId}) {
    return callBridge({
        path: 'editor.unifyAll', tabId, params: {showProofDialog: false}, timeoutMs: UNIFY_ALL_TIMEOUT_MS
    })
}

/*
* Reads the state of an editor tab.
* includeDescription - include the description of the editor (it may be long, so it is omitted by default).
* Resolves to the API response: res is the editor state as returned by api.editor().getState().
* */
function getEditorState({tabId, includeDescription = false}) {
    return callBridge({
        path: 'editor.getState', tabId, params: {includeDescription}, timeoutMs: GET_STATE_TIMEOUT_MS
    })
}

/*
* Generates the compressed proof of a proved step.
* Resolves to the API response: res is the proof text as returned by api.editor().getCompressedProof().
* */
function getCompressedProof({tabId, label, includeDescription}) {
    return callBridge({
        path: 'editor.getCompressedProof',
        tabId,
        params: {label, includeDescription},
        timeoutMs: GET_COMPRESSED_PROOF_TIMEOUT_MS,
    })
}

/*
* Adds steps to an editor tab.
* atIdx - index of the first new step; undefined means before the topmost selected step, or at the very end.
* vars - new variables as [[type, name]].
* steps - [{label?, type?: 'h'|'p'|'g', stmt, jstf?, isBkm?}].
* Resolves to the API response: res is the array of labels of the added steps.
* */
function addSteps({tabId, atIdx, vars, steps}) {
    return callBridge({path: 'editor.addSteps', tabId, params: {atIdx, vars, steps}, timeoutMs: ADD_STEPS_TIMEOUT_MS})
}

/*
* Updates existing steps of an editor tab.
* steps - [{label, type?: 'h'|'p'|'g', stmt?, jstf?, isBkm?}]. label identifies the step and cannot be changed;
* attributes which are undefined are not changed.
* Resolves to the API response: res is true.
* */
function updateSteps({tabId, steps}) {
    return callBridge({path: 'editor.updateSteps', tabId, params: {steps}, timeoutMs: UPDATE_STEPS_TIMEOUT_MS})
}

/*
* Deletes steps of an editor tab.
* Labels of non-existing steps are silently ignored by the API.
* Resolves to the API response: res is null.
* */
function deleteSteps({tabId, labels}) {
    return callBridge({path: 'editor.deleteSteps', tabId, params: {labels}, timeoutMs: DELETE_STEPS_TIMEOUT_MS})
}

/*
* Replaces the description of an editor tab.
* Resolves to the API response: res is null.
* */
function setDescription({tabId, descr}) {
    return callBridge({path: 'editor.setDescription', tabId, params: {descr}, timeoutMs: SET_DESCRIPTION_TIMEOUT_MS})
}

/*
* Replaces the disjoint variable groups of an editor tab.
* disj - [[var1, var2, ...]], one array per group.
* Resolves to the API response: res is null.
* */
function setDisjoints({tabId, disj}) {
    return callBridge({path: 'editor.setDisjoints', tabId, params: {disj}, timeoutMs: SET_DISJOINTS_TIMEOUT_MS})
}

/*
* Sets the location of an editor tab, i.e. which assertions can be used in it.
* place - 'before', 'after', or 'last'; label - required for 'before' and 'after', must be undefined for 'last'.
* Resolves to the API response: res is null.
* */
function setLocation({tabId, place, label}) {
    return callBridge({path: 'editor.setLocation', tabId, params: {place, label}, timeoutMs: SET_LOCATION_TIMEOUT_MS})
}

function isBlank(str) {
    return str === undefined || str.trim() === ''
}

/*
* Searches assertions by a pattern and labels (discouraged and deprecated assertions are excluded).
* pattern - if undefined or blank, assertions are not filtered by a pattern.
* label - if defined and not blank, only assertions whose label contains at least one of
* its space-separated parts (case-insensitive) are returned.
* strictLabel - if defined and not blank, only assertions whose label is equal to
* one of its space-separated labels (case-sensitive) are returned.
* editorTabId - if defined, only assertions which can be used in that editor tab are returned;
* if undefined, the whole loaded context is searched.
* pageNum is 1-based.
* Resolves to the API response: res is
* {totalCount, pageNum, pageSize, numOfPages, editorScope?, asrts:[{label, isAxiom, disj, hyps, asrt}]}.
* */
function findAssertions({label, strictLabel, pattern, pageNum, pageSize, editorTabId}) {
    return callBridge({
        path: 'explorer.findAssertions',
        params: {label, strictLabel, pattern, pageNum, pageSize, editorTabId},
        timeoutMs: FIND_ASSERTIONS_TIMEOUT_MS,
    })
}

/*
* Replaces the loaded context with files downloaded from the given URLs.
* srcs - [{url, stopBefore?, stopAfter?}], in the order of loading.
* Resolves to the API response: res is {summary, numOfAsrts}.
* */
function loadContext({srcs}) {
    return callBridge({path: 'context.load', params: {srcs}, timeoutMs: LOAD_CONTEXT_TIMEOUT_MS})
}

/*
* Describes the loaded context.
* Resolves to the API response: res is {numOfAsrts, numOfAxioms, srcs}, where each of srcs is
* {type, url, fileName, alias, readInstr, label, numOfAsrts, numOfAxioms, files},
* and each of files is {fileName, numOfAsrts, numOfAxioms}.
* */
function getLoadedContextInfo() {
    return callBridge({path: 'context.getLoadedContextInfo', timeoutMs: GET_LOADED_CONTEXT_INFO_TIMEOUT_MS})
}

/*
* Optionally runs 'Unify All', then reads the editor state and returns it as a tool result in the text format.
* notes - additional lines to show at the top of the state.
* */
async function editorStateResult({tabId, unifyFirst, onlyRelevantSteps, includeDescription = false, notes = []}) {
    notes = [...notes]
    if (unifyFirst) {
        const unifyResp = await unifyAll({tabId})
        if (!unifyResp.isOk) {
            return errorResult(`'Unify All' failed: ${unifyResp.err}`)
        }
        if (unifyResp.res === false) {
            notes.push("'Unify All' could not start (another action may be running), statuses may be outdated.")
        }
    }
    const stateResp = await getEditorState({tabId, includeDescription})
    if (!stateResp.isOk) {
        return errorResult(`Could not get the editor state: ${stateResp.err}`)
    }
    return textResult(editorStateToText({state: stateResp.res, onlyRelevantSteps, notes}))
}

const server = new McpServer({name: 'metamath-lamp', version: '0.0.1'})

server.registerTool(
    'list_tabs',
    {
        title: 'List tabs',
        description: `Returns all tabs of metamath-lamp in the order they are displayed, \
as a JSON array of \`{tabId, type, title, isVisible}\`.

type is one of:
* settings - the Settings tab
* tabs - the Tabs tab, which manages the other tabs
* editor - an editor tab with a proof
* explorer - an explorer tab, which lists assertions of the loaded database
* assertion - a tab which shows a single assertion of the loaded database
* db-info - the tab with information about the loaded database
* mcp - the MCP tab, which connects metamath-lamp to this MCP server

isVisible is true only for the tab which is currently displayed in metamath-lamp.

Use the tabId of an editor tab in the tools which work with editors.`,
        inputSchema: {},
        annotations: {
            readOnlyHint: true,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async () => {
        const resp = await listTabs()
        if (!resp.isOk) {
            return errorResult(`Could not list the tabs: ${resp.err}`)
        }
        return textResult(JSON.stringify(resp.res))
    })
)

server.registerTool(
    'create_editor_tab',
    {
        title: 'Create editor tab',
        description: `Creates a new empty editor tab in metamath-lamp.

Returns the id of the new tab as JSON \`{tabId}\`. \
The new editor can be used right away in the tools which work with editors.`,
        inputSchema: {
            open: z.boolean().default(true).describe(
                'Make the new tab visible. If false, the currently visible tab stays visible.'
            ),
            title: z.string().trim().min(1).optional().describe(
                'Title of the new tab. Omit to get a default title like "EDITOR [2]".'
            ),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    exclusive(async ({open, title}) => {
        const resp = await createEditorTab({open, title})
        if (!resp.isOk) {
            return errorResult(`Could not create the tab: ${resp.err}`)
        }
        return textResult(JSON.stringify({tabId: resp.res}))
    })
)

server.registerTool(
    'close_tab',
    {
        title: 'Close tab',
        description: `Closes a tab of metamath-lamp without asking the user for a confirmation.

Closing an editor tab permanently deletes its content. \
The Settings, Tabs, and MCP tabs cannot be closed by this tool.`,
        inputSchema: {
            tabId: z.string().trim().min(1).describe('Id of the tab to close (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: true,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    exclusive(async ({tabId}) => {
        const resp = await closeTab({tabId})
        if (!resp.isOk) {
            return errorResult(`Could not close the tab: ${resp.err}`)
        }
        if (resp.res !== true) {
            return errorResult(`The tab '${tabId}' was not closed.`)
        }
        return textResult(`Closed the tab '${tabId}'.`)
    })
)

server.registerTool(
    'open_tab',
    {
        title: 'Open tab',
        description: `Makes an existing tab of metamath-lamp visible, as if the user clicked it. \
Only one tab is visible at a time, so the previously visible tab gets hidden.

Use it to show the user the result of your work, e.g. an editor tab with a proof.`,
        inputSchema: {
            tabId: z.string().trim().min(1).describe('Id of the tab to make visible (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({tabId}) => {
        const resp = await openTab({tabId})
        if (!resp.isOk) {
            return errorResult(`Could not open the tab: ${resp.err}`)
        }
        return textResult(`The tab '${tabId}' is visible now.`)
    })
)

server.registerTool(
    'rename_tab',
    {
        title: 'Rename tab',
        description: `Changes the title of an existing tab of metamath-lamp. \
The Settings and Tabs tabs cannot be renamed. \
Titles of editor tabs are saved with the editor content.`,
        inputSchema: {
            tabId: z.string().trim().min(1).describe('Id of the tab to rename (see list_tabs).'),
            title: z.string().trim().min(1).describe('New title of the tab.'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({tabId, title}) => {
        const resp = await renameTab({tabId, title})
        if (!resp.isOk) {
            return errorResult(`Could not rename the tab: ${resp.err}`)
        }
        return textResult(`Renamed the tab '${tabId}' to '${title}'.`)
    })
)

server.registerTool(
    'get_editor_state',
    {
        title: 'Get editor state',
        description: `Returns the content of a metamath-lamp editor tab: local variables, \
disjoint variable groups, proof steps, and optionally the description (only with includeDescription=true).

The \`Scope:\` line shows which assertions of the database can be used in justifications in this editor \
(all assertions, or only those before or up to some label). \
Pass editorTabId to find_assertions to search only within this scope.

Each step is printed as one line: \`type status label statement\`, optionally followed by indented lines
\`jstf: <hypothesis labels> : <assertion label>\` (the justification) and \`error: <message>\`.

type:
* H - hypothesis
* P / p - provable step, bookmarked / not bookmarked
* G - the goal step

status:
* v - proved
* ? - not proved (no justification, or the justification doesn't unify)
* ~ - valid justification, but some steps it depends on are not proved
* x - invalid justification
* . - hypothesis, or status is unknown (if many steps show '.', one erroring step probably blocks the status \
detection for the whole editor)

By default 'Unify All' runs before the state is read, so statuses are up to date. \
'Unify All' may fill in missing justifications and clears the user's step selection. \
Pass unifyFirst=false to read the editor exactly as it is, including the selected steps.`,
        inputSchema: {
            unifyFirst: z.boolean().default(true).describe(
                "Run 'Unify All' before reading the state, so step statuses are up to date."
            ),
            onlyRelevantSteps: z.boolean().default(true).describe(
                'Show only bookmarked steps, hypotheses, the goal, unproved steps, and steps with errors. '
                + 'Use it to save context on large proofs when only the unfinished part matters.'
            ),
            includeDescription: z.boolean().default(false).describe(
                'Include the description of the editor. It may be long, so pass true only when you need it.'
            ),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({unifyFirst, onlyRelevantSteps, includeDescription, tabId}) => {
        return editorStateResult({tabId, unifyFirst, onlyRelevantSteps, includeDescription})
    })
)

server.registerTool(
    'get_compressed_proof',
    {
        title: 'Get compressed proof',
        description: `Returns the compressed proof of a proved step of a metamath-lamp editor tab, \
as a Metamath \`$p\` statement ready to be pasted into a .mm file. \
The label of the step is used as the label of the theorem. \
If the proof needs new variables, disjoint variable conditions, or hypotheses, \
the statement is wrapped into a \`\${ ... \$}\` block with the required \`$v\`, \`$f\`, \`$d\` and \`$e\` statements.

This is the same text which metamath-lamp shows in the "Show completed proof" dialog \
(without the proof table, and with only the hypotheses which are used in the proof).

Only a provable step (P or G) with the status 'v' (proved) has a proof. \
By default 'Unify All' runs first, so statuses are up to date. \
'Unify All' may fill in missing justifications and clears the user's step selection.`,
        inputSchema: {
            label: z.string().trim().min(1).describe('Label of the proved step.'),
            includeDescription: z.boolean().default(false).describe(
                'Include the description of the editor as a comment before the theorem.'
            ),
            unifyFirst: z.boolean().default(true).describe(
                "Run 'Unify All' before generating the proof, so step statuses are up to date."
            ),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({label, includeDescription, unifyFirst, tabId}) => {
        if (unifyFirst) {
            const unifyResp = await unifyAll({tabId})
            if (!unifyResp.isOk) {
                return errorResult(`'Unify All' failed: ${unifyResp.err}`)
            }
        }
        const proofResp = await getCompressedProof({tabId, label, includeDescription})
        if (!proofResp.isOk) {
            return errorResult(
                `Could not get the proof: ${proofResp.err} Use get_editor_state to see the statuses of the steps.`
            )
        }
        return textResult(proofResp.res)
    })
)

server.registerTool(
    'add_steps',
    {
        title: 'Add steps',
        description: `Adds new steps (and optionally new variables) to a metamath-lamp editor tab.

New steps are inserted before \`beforeLabel\`, or after \`afterLabel\` (\`beforeLabel\` wins if both are given). \
If neither is given, they are inserted before the goal step, or at the end if there is no goal step. \
A step can be derived only from steps above it, so the order matters.

After adding, 'Unify All' runs and the editor state is returned in the same format as get_editor_state \
(only relevant steps), so you can see the status of each new step.`,
        inputSchema: {
            beforeLabel: z.string().optional().describe('Label of an existing step to insert the new steps before.'),
            afterLabel: z.string().optional().describe('Label of an existing step to insert the new steps after.'),
            variables: z.array(z.tuple([z.string(), z.string()])).optional().describe(
                'New variables to add, as [type, name] pairs, e.g. [["setvar","x1"],["class","A1"]]. '
                + 'Only needed for variables that are not defined yet. Unused variables are removed automatically.'
            ),
            steps: z.array(z.object({
                label: z.string().optional().describe(
                    'Label of the step, without whitespace. Omit to generate a new unique label.'
                ),
                type: z.enum(['H', 'P', 'G']).default('P').describe(
                    'H - hypothesis, P - provable step, G - the goal step.'
                ),
                statement: z.string().describe(
                    'The statement including its typecode, e.g. "|- ( ph -> ph )".'
                ),
                justification: z.string().optional().describe(
                    'Justification in the form "<hypothesis labels separated by spaces> : <assertion label>", '
                    + 'e.g. "1 2 : ax-mp" or ": idi" when there are no hypotheses. '
                    + 'Omit to leave the step without a justification. Ignored for hypotheses.'
                ),
                isBookmarked: z.boolean().default(false).describe(
                    'Bookmark the step. '
                    + 'Proved steps which are not bookmarked are hidden in the returned editor state.'
                ),
            })).min(1).describe('Steps to add, in order.'),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    exclusive(async ({beforeLabel, afterLabel, variables, steps, tabId}) => {
        const stateResp = await getEditorState({tabId})
        if (!stateResp.isOk) {
            return errorResult(`Could not get the editor state: ${stateResp.err}`)
        }
        const {idx, err} = getInsertIdx({steps: stateResp.res.steps, beforeLabel, afterLabel})
        if (err !== undefined) {
            return errorResult(err)
        }
        const addResp = await addSteps({
            tabId,
            atIdx: idx,
            vars: variables,
            steps: steps.map(step => ({
                label: step.label,
                type: step.type.toLowerCase(),
                stmt: step.statement,
                jstf: step.justification,
                isBkm: step.isBookmarked,
            })),
        })
        if (!addResp.isOk) {
            return errorResult(`Could not add the steps: ${addResp.err}`)
        }
        return editorStateResult({
            tabId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [`Added steps: ${addResp.res.join(' ')}`],
        })
    })
)

server.registerTool(
    'update_steps',
    {
        title: 'Update steps',
        description: `Changes existing steps of a metamath-lamp editor tab: statement, justification, type, \
or bookmark.

Each step is identified by its label, which cannot be changed by this tool. \
Only the attributes you pass are changed, all others stay as they are.

After updating, 'Unify All' runs and the editor state is returned in the same format as get_editor_state \
(only relevant steps), so you can see the effect of the changes.`,
        inputSchema: {
            steps: z.array(z.object({
                label: z.string().describe('Label of an existing step to update.'),
                type: z.enum(['H', 'P', 'G']).optional().describe(
                    'New type: H - hypothesis, P - provable step, G - the goal step.'
                ),
                statement: z.string().optional().describe(
                    'New statement including its typecode, e.g. "|- ( ph -> ph )".'
                ),
                justification: z.string().optional().describe(
                    'New justification in the form "<hypothesis labels separated by spaces> : <assertion label>", '
                    + 'e.g. "1 2 : ax-mp". Pass an empty string to remove the justification. '
                    + 'Ignored for hypotheses.'
                ),
                isBookmarked: z.boolean().optional().describe(
                    'Bookmark or unbookmark the step. '
                    + 'Proved steps which are not bookmarked are hidden in the returned editor state.'
                ),
            })).min(1).describe('Steps to update.'),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({steps, tabId}) => {
        const updateResp = await updateSteps({
            tabId,
            steps: steps.map(step => ({
                label: step.label,
                type: step.type?.toLowerCase(),
                stmt: step.statement,
                jstf: step.justification,
                isBkm: step.isBookmarked,
            })),
        })
        if (!updateResp.isOk) {
            return errorResult(`Could not update the steps: ${updateResp.err}`)
        }
        return editorStateResult({
            tabId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [`Updated steps: ${steps.map(step => step.label).join(' ')}`],
        })
    })
)

server.registerTool(
    'delete_steps',
    {
        title: 'Delete steps',
        description: `Deletes steps from a metamath-lamp editor tab.

If any of the labels doesn't exist, nothing is deleted and an error is returned.

Justifications of other steps which reference deleted steps are not changed, \
so such steps will show errors or become unproved.

After deleting, 'Unify All' runs and the editor state is returned in the same format as get_editor_state \
(only relevant steps).`,
        inputSchema: {
            labels: z.array(z.string()).min(1).describe('Labels of the steps to delete.'),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: true,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    exclusive(async ({labels, tabId}) => {
        const stateResp = await getEditorState({tabId})
        if (!stateResp.isOk) {
            return errorResult(`Could not get the editor state: ${stateResp.err}`)
        }
        const existingLabels = new Set(stateResp.res.steps.map(step => step.label))
        const unknownLabels = labels.filter(label => !existingLabels.has(label))
        if (unknownLabels.length > 0) {
            return errorResult(`Nothing was deleted, because these steps don't exist: ${unknownLabels.join(' ')}`)
        }
        const deleteResp = await deleteSteps({tabId, labels})
        if (!deleteResp.isOk) {
            return errorResult(`Could not delete the steps: ${deleteResp.err}`)
        }
        return editorStateResult({
            tabId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [`Deleted steps: ${labels.join(' ')}`],
        })
    })
)

server.registerTool(
    'set_description',
    {
        title: 'Set description',
        description: `Replaces the description of a metamath-lamp editor tab. \
The description is free text shown at the top of the editor. \
It is saved with the editor content and may be included as a comment in the compressed proof \
(see get_compressed_proof). \
Pass an empty string to clear the description.`,
        inputSchema: {
            descr: z.string().describe('New description of the editor. An empty string clears it.'),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({descr, tabId}) => {
        const resp = await setDescription({tabId, descr})
        if (!resp.isOk) {
            return errorResult(`Could not set the description: ${resp.err}`)
        }
        return textResult(descr === '' ? 'Cleared the description.' : 'Set the description.')
    })
)

server.registerTool(
    'set_disjoints',
    {
        title: 'Set disjoints',
        description: `Replaces all disjoint variable groups of a metamath-lamp editor tab. \
Each group is a list of variables which must be pairwise distinct (as in a Metamath \`$d\` statement). \
Existing groups are discarded, so pass all groups which the editor must have. \
Pass an empty list to remove all disjoints.

After setting, 'Unify All' runs and the editor state is returned in the same format as get_editor_state \
(only relevant steps). Errors in disjoints are shown in the Disjoints section.`,
        inputSchema: {
            disj: z.array(
                z.array(z.string().trim().regex(/^\S+$/, 'A variable must be a non-empty string without spaces.'))
                    .min(2)
            ).describe('Disjoint variable groups, e.g. [["x", "y"], ["x", "A", "B"]].'),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: true,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({disj, tabId}) => {
        const resp = await setDisjoints({tabId, disj})
        if (!resp.isOk) {
            return errorResult(`Could not set the disjoints: ${resp.err}`)
        }
        return editorStateResult({
            tabId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [disj.length === 0 ? 'Removed all disjoints.' : `Set ${disj.length} disjoint variable groups.`],
        })
    })
)

server.registerTool(
    'set_location',
    {
        title: 'Set location',
        description: `Sets the location of a metamath-lamp editor tab, \
i.e. which assertions of the loaded database can be used in justifications in this editor \
(the \`Scope:\` line of get_editor_state, and the scope of find_assertions with editorTabId):
* before - only assertions declared before the assertion with the given label. \
Use it to prove an existing theorem without using the theorem itself and the theorems after it.
* after - assertions declared before the given label, and the assertion with that label.
* last - all assertions of the database.

If the label doesn't exist, the location is not changed and an error is returned.

After setting, 'Unify All' runs and the editor state is returned in the same format as get_editor_state \
(only relevant steps), because justifications which use assertions outside of the new scope become invalid.`,
        inputSchema: {
            place: z.enum(['before', 'after', 'last']).describe('Where the scope of the editor ends.'),
            label: z.string().trim().min(1).optional().describe(
                "Label of an assertion. Required for 'before' and 'after'; must be omitted for 'last'."
            ),
            tabId: z.string().trim().min(1).describe('Id of the editor tab (see list_tabs).'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({place, label, tabId}) => {
        if (place === 'last' && label !== undefined) {
            return errorResult("'label' must be omitted when 'place' is 'last'.")
        }
        if (place !== 'last' && label === undefined) {
            return errorResult(`'label' is required when 'place' is '${place}'.`)
        }
        const resp = await setLocation({tabId, place, label})
        if (!resp.isOk) {
            return errorResult(`Could not set the location: ${resp.err}`)
        }
        return editorStateResult({
            tabId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [
                place === 'last' ? 'Set the location to all assertions.' : `Set the location to ${place} '${label}'.`
            ],
        })
    })
)

server.registerTool(
    'find_assertions',
    {
        title: 'Find assertions',
        description: `Searches assertions (theorems and axioms) by a pattern and/or labels \
in the loaded Metamath database. Discouraged and deprecated assertions are excluded. \
pattern, label, and strictLabel are optional; an assertion is returned only if it matches all given criteria.

Pass editorTabId to return only assertions which can be used in that editor tab \
(an editor may be limited to assertions declared before or up to some label). \
Without editorTabId, the whole database is searched.

Pass label to return only assertions whose label contains (case-insensitive) at least one of \
its space-separated parts, e.g. \`addcom addass\` returns assertions with \`addcom\` or \`addass\` in the label.

Pass strictLabel to return only assertions whose label is equal (case-sensitive) to one of \
its space-separated labels, e.g. \`ax-mp addcom\` returns exactly \`ax-mp\` and \`addcom\`. \
Use it to look up known assertions.

The simplest pattern is a space-separated sequence of math symbols, e.g. \`A + B = B + A\`. \
Variables match any variable of the same type, typecodes match any variable of that type, \
and other symbols may occur in between the pattern symbols. \
Patterns support more features: adjacency, restricting matches to hypotheses or the assertion statement, \
operators, negation. Call get_pattern_syntax once to learn the full syntax.

Results are returned in pages of pageSize assertions, in the order of their declaration \
in the database. Each assertion is printed as its label, optionally followed by \
\`(axiom)\`, then indented lines: \`disj:\` disjoint variable groups separated by commas, \
one \`hyp:\` line per essential hypothesis, and \`asrt:\` the assertion statement.

The search may open a progress dialog in metamath-lamp, and the user may cancel it.`,
        inputSchema: {
            pattern: z.string().optional().describe(
                'The search pattern, e.g. "|- $+a A + B = B + A". Omit to not filter by a pattern.'
            ),
            page: z.number().int().min(1).default(1).describe('Number of the page to return, starting from 1.'),
            pageSize: z.number().int().min(1).max(FIND_ASSERTIONS_MAX_PAGE_SIZE)
                .default(FIND_ASSERTIONS_DEFAULT_PAGE_SIZE)
                .describe(`Number of assertions per page, at most ${FIND_ASSERTIONS_MAX_PAGE_SIZE}.`),
            label: z.string().optional().describe(
                'Space-separated parts of labels; only assertions whose label contains at least one part are returned.'
            ),
            strictLabel: z.string().optional().describe(
                'Space-separated labels; only assertions whose label is equal to one of them are returned.'
            ),
            editorTabId: z.string().trim().min(1).optional().describe(
                'Id of an editor tab to return only assertions available in it. Omit to search the whole database.'
            ),
        },
        annotations: {
            readOnlyHint: true,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async ({label, strictLabel, pattern, page, pageSize, editorTabId}) => {
        const resp = await findAssertions({label, strictLabel, pattern, pageNum: page, pageSize, editorTabId})
        if (!resp.isOk) {
            return errorResult(`Could not find assertions: ${resp.err}`)
        }
        const {totalCount, numOfPages, editorScope, asrts} = resp.res
        const scope = editorScope === undefined ? '' : ` (scope of editor ${editorTabId}: ${editorScope})`
        const criteria = [
            ...(isBlank(pattern) ? [] : [`the pattern '${pattern}'`]),
            ...(isBlank(label) ? [] : [`the label '${label}'`]),
            ...(isBlank(strictLabel) ? [] : [`the strict label '${strictLabel}'`]),
        ].join(' and ')
        const matchText = criteria === '' ? 'found' : `match ${criteria}`
        if (totalCount === 0) {
            return textResult(`No assertions ${matchText}${scope}.`)
        }
        if (asrts.length === 0) {
            return errorResult(
                `Page ${page} doesn't exist: ${totalCount} assertions ${matchText}, ${numOfPages} pages in total.`
            )
        }
        const header = `${totalCount} assertions ${matchText}${scope}. Page ${page} of ${numOfPages}:`
        return textResult([header, ...asrts.map(assertionToText)].join('\n\n'))
    })
)

server.registerTool(
    'get_pattern_syntax',
    {
        title: 'Get pattern syntax',
        description: 'Returns the full description of the syntax of patterns used by find_assertions.',
        inputSchema: {},
        annotations: {
            readOnlyHint: true,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    async () => textResult(PATTERN_SYNTAX)
)

server.registerTool(
    'load_context',
    {
        title: 'Load context',
        description: `Loads a Metamath database (the context) into metamath-lamp, replacing the loaded one. \
This is the same as selecting sources in the context selector of metamath-lamp and clicking "Apply changes".

Each source is a file given as:
* an http(s) URL, e.g. https://us.metamath.org/metamath/set.mm (downloaded by the browser), or
* a local file, as an absolute path or a file:// URL, e.g. /home/user/mm/set.mm. \
${fileServer === undefined
    ? 'Loading local files is currently disabled (no allowed directories are configured).'
    : `Local files can be loaded only from the allowed directories: ${FileServer.describeAllowedDirs(fileServer)}.`}

Several sources are loaded one after another into one context, in the given order. \
Files included via \`$[ ... $]\` are loaded automatically, relative to the including file. \
For each source, reading may be limited to the assertions before \`stopBefore\` or up to \`stopAfter\` \
(assertion labels), e.g. to prove a theorem without using itself and the theorems after it.

If a URL is not marked as trusted in metamath-lamp, the user is asked to confirm downloading from it, \
so this call may take a while. Loading a big database such as set.mm takes from several seconds \
to a minute. Open editor tabs are not closed.`,
        inputSchema: {
            sources: z.array(z.object({
                url: z.string().min(1).describe(
                    'An http(s) URL, a file:// URL, or an absolute local path of a Metamath (.mm) file.'
                ),
                stopBefore: z.string().min(1).optional().describe(
                    'Read the file only up to this assertion label, excluding it. Cannot be used with stopAfter.'
                ),
                stopAfter: z.string().min(1).optional().describe(
                    'Read the file only up to this assertion label, including it. Cannot be used with stopBefore.'
                ),
            })).min(1).describe('Files to load, in order.'),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: true,
        },
    },
    exclusive(async ({sources}) => {
        const srcs = []
        for (const {url, stopBefore, stopAfter} of sources) {
            if (stopBefore !== undefined && stopAfter !== undefined) {
                return errorResult(`Only one of stopBefore and stopAfter may be specified for '${url}'.`)
            }
            const resolved = await FileServer.srcToUrl(fileServer, url.trim(), fileServerBaseUrl)
            if (resolved.err !== undefined) {
                return errorResult(resolved.err)
            }
            srcs.push({url: resolved.url, stopBefore, stopAfter})
        }
        const resp = await loadContext({srcs})
        if (!resp.isOk) {
            return errorResult(`Could not load the context: ${resp.err}`)
        }
        const {summary, numOfAsrts} = resp.res
        return textResult(`${summary}\nNumber of assertions: ${numOfAsrts}`)
    })
)

server.registerTool(
    'get_loaded_context_info',
    {
        title: 'Get loaded context info',
        description: `Describes the Metamath database (the context) currently loaded in metamath-lamp: \
its source files in the order of loading, how each of them is read (the whole file, \
or only up to some assertion label), and how many assertions (and how many of them are axioms) are loaded, \
in total, per source, and per file included via \`$[ ... $]\`.

Use it to check whether a context is loaded at all and whether it is the one needed, \
before calling load_context, which may take a long time for a big database. \
Assertions after the stop label of a source are not loaded and cannot be used in any editor. \
An editor tab may be limited further to a part of the context, see set_location.`,
        inputSchema: {},
        annotations: {
            readOnlyHint: true,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    exclusive(async () => {
        const resp = await getLoadedContextInfo()
        if (!resp.isOk) {
            return errorResult(`Could not get the information about the loaded context: ${resp.err}`)
        }
        return textResult(loadedContextInfoToText({
            info: resp.res,
            urlToLocalPath: url => fileServer === undefined
                ? undefined
                : FileServer.urlToLocalPath(url, fileServerBaseUrl),
        }))
    })
)

await server.connect(new StdioServerTransport())

// The WebSocket server keeps the process alive, so exit explicitly when the MCP client goes away.
// Otherwise an orphaned process would keep the port busy for the next agent session.
process.stdin.on('end', () => process.exit(0))
process.stdin.on('close', () => process.exit(0))
