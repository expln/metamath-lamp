open Test

let parse = (str:string):JSON.t => JSON.parseOrThrow(str)

// A log which collects the written text and whose clock is advanced by the tests.
type testLog = {
    log: RequestLog.t,
    getText: unit => string,
    advanceMs: float => unit,
}

let startTime = Date.makeWithYMDHMSM(
    ~year=2026, ~month=10, ~day=27, ~hours=14, ~minutes=5, ~seconds=12, ~milliseconds=345
)

let makeTestLog = ():testLog => {
    let text = ref("")
    let time = ref(startTime->Date.getTime)
    let log = RequestLog.make({
        write: str => text := text.contents ++ str,
        now: () => Date.fromTime(time.contents),
    })
    text := ""
    {
        log,
        getText: () => text.contents,
        advanceMs: ms => time := time.contents +. ms,
    }
}

let delim = String.repeat("=", 100)

describe("formatTimestamp", () => {
    it("formats the local time with milliseconds, padding all parts with zeros", () => {
        //given
        let d = Date.makeWithYMDHMSM(~year=2026, ~month=0, ~day=2, ~hours=3, ~minutes=4, ~seconds=5, ~milliseconds=6)

        //when
        let str = RequestLog.formatTimestamp(d)

        //then
        assertEq(str, "2026-01-02 03:04:05.006")
    })
})

describe("make", () => {
    it("writes the line which marks the start of the log", () => {
        //given
        let text = ref("")

        //when
        let _ = RequestLog.make({write: str => text := text.contents ++ str, now: () => startTime})

        //then
        assertEq(text.contents, `\n${String.repeat("#", 100)}\n### 2026-11-27 14:05:12.345 LOG STARTED`)
    })
})

describe("messageReceived and messageSent", () => {
    it("writes a tool call with its arguments in one line and the text result as is", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(`{
            "jsonrpc":"2.0", "id":7, "method":"tools/call",
            "params":{"name":"add_steps", "arguments":{"tabId":"k3f", "steps":[{"statement":"|- ph"}]}}
        }`))

        //then
        assertEq(t.getText(), "")

        //when
        t.advanceMs(667.)
        t.log->RequestLog.messageSent(parse(`{
            "jsonrpc":"2.0", "id":7,
            "result":{"content":[{"type":"text", "text":"Added steps: 1\\nSteps:\\n  1 |- ph"}]}
        }`))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #7 tools/call add_steps
{"tabId":"k3f","steps":[{"statement":"|- ph"}]}
<<< 2026-11-27 14:05:13.012 RESPONSE #7 (667 ms)
Added steps: 1
Steps:
  1 |- ph`
        )
    })

    it("marks a tool result with isError and drops one new line symbol at the end of the text", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(
            `{"jsonrpc":"2.0", "id":"a1", "method":"tools/call", "params":{"name":"list_tabs"}}`
        ))
        t.log->RequestLog.messageSent(parse(
            `{"jsonrpc":"2.0", "id":"a1", "result":{"content":[{"type":"text", "text":"Failed.\\n"}], "isError":true}}`
        ))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #a1 tools/call list_tabs
{}
<<< 2026-11-27 14:05:12.345 RESPONSE #a1 (0 ms) TOOL ERROR
Failed.`
        )
    })

    it("separates several content items and writes items which are not text as JSON", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(
            `{"jsonrpc":"2.0", "id":1, "method":"tools/call", "params":{"name":"x", "arguments":{}}}`
        ))
        t.log->RequestLog.messageSent(parse(`{"jsonrpc":"2.0", "id":1, "result":{"content":[
            {"type":"text", "text":"first"},
            {"type":"image", "data":"AAA", "mimeType":"image/png"}
        ]}}`))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #1 tools/call x
{}
<<< 2026-11-27 14:05:12.345 RESPONSE #1 (0 ms)
first
--- content item 2 ---
{"type":"image","data":"AAA","mimeType":"image/png"}`
        )
    })

    it("writes params and results of other requests as JSON", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(
            `{"jsonrpc":"2.0", "id":0, "method":"initialize", "params":{"protocolVersion":"2025-06-18"}}`
        ))
        t.log->RequestLog.messageReceived(parse(`{"jsonrpc":"2.0", "id":1, "method":"ping"}`))
        t.advanceMs(5.)
        t.log->RequestLog.messageSent(parse(`{"jsonrpc":"2.0", "id":1, "result":{}}`))
        t.log->RequestLog.messageSent(parse(
            `{"jsonrpc":"2.0", "id":0, "result":{"serverInfo":{"name":"metamath-lamp"}}}`
        ))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #1 ping
<<< 2026-11-27 14:05:12.350 RESPONSE #1 (5 ms)
{}
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #0 initialize
{"protocolVersion":"2025-06-18"}
<<< 2026-11-27 14:05:12.350 RESPONSE #0 (5 ms)
{"serverInfo":{"name":"metamath-lamp"}}`
        )
    })

    it("writes a JSON-RPC error", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(`{"jsonrpc":"2.0", "id":4, "method":"nope"}`))
        t.log->RequestLog.messageSent(parse(
            `{"jsonrpc":"2.0", "id":4, "error":{"code":-32601, "message":"Method not found"}}`
        ))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #4 nope
<<< 2026-11-27 14:05:12.345 RESPONSE #4 (0 ms) JSON-RPC ERROR
{"code":-32601,"message":"Method not found"}`
        )
    })

    it("writes notifications and unmatched messages right away", () => {
        //given
        let t = makeTestLog()

        //when
        t.log->RequestLog.messageReceived(parse(`{"jsonrpc":"2.0", "method":"notifications/initialized"}`))
        t.log->RequestLog.messageReceived(parse(
            `{"jsonrpc":"2.0", "method":"notifications/cancelled", "params":{"requestId":3}}`
        ))
        t.log->RequestLog.messageSent(parse(`{"jsonrpc":"2.0", "id":99, "result":{}}`))

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 NOTIFICATION notifications/initialized
${delim}
>>> 2026-11-27 14:05:12.345 NOTIFICATION notifications/cancelled
{"requestId":3}
${delim}
<<< 2026-11-27 14:05:12.345 MESSAGE
{"jsonrpc":"2.0","id":99,"result":{}}`
        )
    })
})

describe("writePending", () => {
    it("writes the requests without responses and forgets them", () => {
        //given
        let t = makeTestLog()
        t.log->RequestLog.messageReceived(parse(
            `{"jsonrpc":"2.0", "id":5, "method":"tools/call", "params":{"name":"list_tabs", "arguments":{}}}`
        ))
        t.advanceMs(1500.)

        //when
        t.log->RequestLog.writePending
        t.log->RequestLog.writePending

        //then
        assertEq(
            t.getText(),
            `
${delim}
>>> 2026-11-27 14:05:12.345 REQUEST #5 tools/call list_tabs
{}
<<< 2026-11-27 14:05:13.845 NO RESPONSE (the server exited after 1500 ms)`
        )
    })
})
