#!/usr/bin/env node
// MCP server (stdio) which lets AI agents work with metamath-lamp opened in a browser tab.
// The browser tab connects to this process over a WebSocket by running the macro from mm-lamp-macro.js.
//
// Environment variables:
//   MM_LAMP_BRIDGE_PORT     - WebSocket port to listen on (127.0.0.1 only). Default: 8765.
//   MM_LAMP_BRIDGE_ORIGINS  - comma separated list of additional allowed page origins.
//                             http(s)://localhost:* and https://expln.github.io are always allowed.

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js'
import { z } from 'zod'
import { BrowserBridge, log } from './browser-bridge.js'
import { editorStateToText } from './editor-state-format.js'

const DEFAULT_PORT = 8765
const DEFAULT_ALLOWED_ORIGINS = ['https://expln.github.io']
const UNIFY_ALL_TIMEOUT_MS = 60_000
const GET_STATE_TIMEOUT_MS = 30_000
const ADD_STEPS_TIMEOUT_MS = 30_000
const UPDATE_STEPS_TIMEOUT_MS = 30_000
const DELETE_STEPS_TIMEOUT_MS = 30_000

function parsePort(str) {
    if (str === undefined || str === '') {
        return DEFAULT_PORT
    }
    const port = Number(str)
    if (!Number.isInteger(port) || port < 1 || port > 65535) {
        log(`Invalid MM_LAMP_BRIDGE_PORT: '${str}'`)
        process.exit(1)
    }
    return port
}

const bridge = new BrowserBridge({
    port: parsePort(process.env.MM_LAMP_BRIDGE_PORT),
    allowedOrigins: [
        ...DEFAULT_ALLOWED_ORIGINS,
        ...(process.env.MM_LAMP_BRIDGE_ORIGINS ?? '').split(',').map(s => s.trim()).filter(s => s.length > 0),
    ],
})

function textResult(text) {
    return {content: [{type: 'text', text}]}
}

function errorResult(text) {
    return {content: [{type: 'text', text}], isError: true}
}

/*
* Wraps a tool handler, so it runs only when a metamath-lamp tab is connected.
* Otherwise the tool returns an error explaining why there is no connection.
* */
function withConnectedTab(handler) {
    return async (args, extra) => {
        const notConnectedErr = await bridge.ensureConnected()
        if (notConnectedErr !== undefined) {
            return errorResult(notConnectedErr)
        }
        return handler(args, extra)
    }
}

/*
* Runs 'Unify All' in an editor tab (the last opened one if editorId is undefined).
* Resolves to the API response: res is true if 'Unify All' ran, false if it could not start.
* */
function unifyAll({editorId}) {
    return bridge.call({path: 'editor.unifyAll', editorId, timeoutMs: UNIFY_ALL_TIMEOUT_MS})
}

/*
* Reads the state of an editor tab (the last opened one if editorId is undefined).
* Resolves to the API response: res is the editor state as returned by api.editor().getState().
* */
function getEditorState({editorId}) {
    return bridge.call({path: 'editor.getState', editorId, timeoutMs: GET_STATE_TIMEOUT_MS})
}

/*
* Adds steps to an editor tab (the last opened one if editorId is undefined).
* atIdx - index of the first new step; undefined means before the topmost selected step, or at the very end.
* vars - new variables as [[type, name]].
* steps - [{label?, type?: 'h'|'p'|'g', stmt, jstf?, isBkm?}].
* Resolves to the API response: res is the array of labels of the added steps.
* */
function addSteps({editorId, atIdx, vars, steps}) {
    return bridge.call({path: 'editor.addSteps', editorId, params: {atIdx, vars, steps}, timeoutMs: ADD_STEPS_TIMEOUT_MS})
}

/*
* Updates existing steps of an editor tab (the last opened one if editorId is undefined).
* steps - [{label, type?: 'h'|'p'|'g', stmt?, jstf?, isBkm?}]. label identifies the step and cannot be changed;
* attributes which are undefined are not changed.
* Resolves to the API response: res is true.
* */
function updateSteps({editorId, steps}) {
    return bridge.call({path: 'editor.updateSteps', editorId, params: {steps}, timeoutMs: UPDATE_STEPS_TIMEOUT_MS})
}

/*
* Deletes steps of an editor tab (the last opened one if editorId is undefined).
* Labels of non-existing steps are silently ignored by the API.
* Resolves to the API response: res is null.
* */
function deleteSteps({editorId, labels}) {
    return bridge.call({path: 'editor.deleteSteps', editorId, params: {labels}, timeoutMs: DELETE_STEPS_TIMEOUT_MS})
}

/*
* Optionally runs 'Unify All', then reads the editor state and returns it as a tool result in the text format.
* notes - additional lines to show at the top of the state.
* */
async function editorStateResult({editorId, unifyFirst, onlyRelevantSteps, notes = []}) {
    notes = [...notes]
    if (unifyFirst) {
        const unifyResp = await unifyAll({editorId})
        if (!unifyResp.isOk) {
            return errorResult(`'Unify All' failed: ${unifyResp.err}`)
        }
        if (unifyResp.res === false) {
            notes.push("'Unify All' could not start (another action may be running), statuses may be outdated.")
        }
    }
    const stateResp = await getEditorState({editorId})
    if (!stateResp.isOk) {
        return errorResult(`Could not get the editor state: ${stateResp.err}`)
    }
    return textResult(editorStateToText({state: stateResp.res, onlyRelevantSteps, notes}))
}

/*
* Returns the index in the editor where new steps should be inserted, or an error message.
* beforeLabel takes precedence over afterLabel.
* If neither is given, new steps go before the goal step, or to the end if there is no goal step.
* */
function getInsertIdx({steps, beforeLabel, afterLabel}) {
    const findIdx = label => steps.findIndex(step => step.label === label)
    if (beforeLabel !== undefined) {
        const idx = findIdx(beforeLabel)
        return idx < 0 ? {err: `No step with label '${beforeLabel}' exists.`} : {idx}
    }
    if (afterLabel !== undefined) {
        const idx = findIdx(afterLabel)
        return idx < 0 ? {err: `No step with label '${afterLabel}' exists.`} : {idx: idx + 1}
    }
    const goalIdx = steps.findIndex(step => step.isGoal)
    return {idx: goalIdx < 0 ? steps.length : goalIdx}
}

const server = new McpServer({name: 'metamath-lamp', version: '0.0.1'})

server.registerTool(
    'get_editor_state',
    {
        title: 'Get editor state',
        description: `Returns the content of a metamath-lamp editor tab: description, local variables, \
disjoint variable groups, and proof steps.

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
            editorId: z.number().int().optional().describe(
                'Id of the editor tab. Omit to use the last opened editor tab.'
            ),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    withConnectedTab(async ({unifyFirst, onlyRelevantSteps, editorId}) => {
        return editorStateResult({editorId, unifyFirst, onlyRelevantSteps})
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

All added steps are bookmarked. \
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
            })).min(1).describe('Steps to add, in order.'),
            editorId: z.number().int().optional().describe(
                'Id of the editor tab. Omit to use the last opened editor tab.'
            ),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    withConnectedTab(async ({beforeLabel, afterLabel, variables, steps, editorId}) => {
        const stateResp = await getEditorState({editorId})
        if (!stateResp.isOk) {
            return errorResult(`Could not get the editor state: ${stateResp.err}`)
        }
        const {idx, err} = getInsertIdx({steps: stateResp.res.steps, beforeLabel, afterLabel})
        if (err !== undefined) {
            return errorResult(err)
        }
        const addResp = await addSteps({
            editorId,
            atIdx: idx,
            vars: variables,
            steps: steps.map(step => ({
                label: step.label,
                type: step.type.toLowerCase(),
                stmt: step.statement,
                jstf: step.justification,
                isBkm: true,
            })),
        })
        if (!addResp.isOk) {
            return errorResult(`Could not add the steps: ${addResp.err}`)
        }
        return editorStateResult({
            editorId,
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
            editorId: z.number().int().optional().describe(
                'Id of the editor tab. Omit to use the last opened editor tab.'
            ),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: false,
            idempotentHint: true,
            openWorldHint: false,
        },
    },
    withConnectedTab(async ({steps, editorId}) => {
        const updateResp = await updateSteps({
            editorId,
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
            editorId,
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
            editorId: z.number().int().optional().describe(
                'Id of the editor tab. Omit to use the last opened editor tab.'
            ),
        },
        annotations: {
            readOnlyHint: false,
            destructiveHint: true,
            idempotentHint: false,
            openWorldHint: false,
        },
    },
    withConnectedTab(async ({labels, editorId}) => {
        const stateResp = await getEditorState({editorId})
        if (!stateResp.isOk) {
            return errorResult(`Could not get the editor state: ${stateResp.err}`)
        }
        const existingLabels = new Set(stateResp.res.steps.map(step => step.label))
        const unknownLabels = labels.filter(label => !existingLabels.has(label))
        if (unknownLabels.length > 0) {
            return errorResult(`Nothing was deleted, because these steps don't exist: ${unknownLabels.join(' ')}`)
        }
        const deleteResp = await deleteSteps({editorId, labels})
        if (!deleteResp.isOk) {
            return errorResult(`Could not delete the steps: ${deleteResp.err}`)
        }
        return editorStateResult({
            editorId,
            unifyFirst: true,
            onlyRelevantSteps: true,
            notes: [`Deleted steps: ${labels.join(' ')}`],
        })
    })
)

await server.connect(new StdioServerTransport())

// The WebSocket server keeps the process alive, so exit explicitly when the MCP client goes away.
// Otherwise an orphaned process would keep the port busy for the next agent session.
process.stdin.on('end', () => process.exit(0))
process.stdin.on('close', () => process.exit(0))
