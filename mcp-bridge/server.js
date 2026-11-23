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
import { BrowserBridge } from './browser-bridge.js'
import { editorStateToText } from './editor-state-format.js'

const DEFAULT_PORT = 8765
const DEFAULT_ALLOWED_ORIGINS = ['https://expln.github.io']
const UNIFY_ALL_TIMEOUT_MS = 5 * 60_000
const GET_STATE_TIMEOUT_MS = 30_000
// The macro retries every 3 s, so this is enough for a tab to connect after the port is opened.
const TAB_CONNECT_WAIT_MS = 5000

function parsePort(str) {
    if (str === undefined || str === '') {
        return DEFAULT_PORT
    }
    const port = Number(str)
    if (!Number.isInteger(port) || port < 1 || port > 65535) {
        console.error(`[mm-lamp-bridge] Invalid MM_LAMP_BRIDGE_PORT: '${str}'`)
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
    async ({unifyFirst, onlyRelevantSteps, editorId}) => {
        const notConnectedErr = await bridge.ensureConnected(TAB_CONNECT_WAIT_MS)
        if (notConnectedErr !== undefined) {
            return errorResult(notConnectedErr)
        }
        const notes = []
        if (unifyFirst) {
            const unifyResp = await bridge.call({path: 'editor.unifyAll', editorId, timeoutMs: UNIFY_ALL_TIMEOUT_MS})
            if (!unifyResp.isOk) {
                return errorResult(`'Unify All' failed: ${unifyResp.err}`)
            }
            if (unifyResp.res === false) {
                notes.push("'Unify All' could not start (another action may be running), statuses may be outdated.")
            }
        }
        const stateResp = await bridge.call({path: 'editor.getState', editorId, timeoutMs: GET_STATE_TIMEOUT_MS})
        if (!stateResp.isOk) {
            return errorResult(`Could not get the editor state: ${stateResp.err}`)
        }
        return textResult(editorStateToText({state: stateResp.res, onlyRelevantSteps, notes}))
    }
)

await server.connect(new StdioServerTransport())

// The WebSocket server keeps the process alive, so exit explicitly when the MCP client goes away.
// Otherwise an orphaned process would keep the port busy for the next agent session.
process.stdin.on('end', () => process.exit(0))
process.stdin.on('close', () => process.exit(0))
