// Checks that the API functions called by src/server.js are exactly the ones allowed by the browser side
// (allowedPaths in src/metamath/api/MM_mcp_client.res of metamath-lamp).

open Test

@module("node:fs") external readFileSync: (string, @as("utf8") _) => string = "readFileSync"
@module("node:path") external dirname: string => string = "dirname"
@module("node:url") external fileURLToPath: string => string = "fileURLToPath"
@val @scope(("import", "meta")) external importMetaUrl: string = "url"

let readRelative = (relPath:string):string => {
    readFileSync(NodeApi.Path.join(dirname(fileURLToPath(importMetaUrl)), relPath))
}

// Returns the first capture group of all matches of the regex, sorted.
let matchAll = (text:string, regex:RegExp.t):array<string> => {
    let res = []
    let continue = ref(true)
    while continue.contents {
        switch regex->RegExp.exec(text) {
            | None => continue := false
            | Some(m) => m->RegExp.Result.matches->Array.get(0)->Option.flatMap(x => x)->Option.forEach(s => res->Array.push(s))
        }
    }
    res->Array.toSorted(String.compare)
}

describe("allowed API paths", () => {
    it("are the same in server.js and in MM_mcp_client.res", () => {
        //given
        let serverJs = readRelative("../src/server.js")
        let mcpClientRes = readRelative("../../src/metamath/api/MM_mcp_client.res")
        let allowedPathsStart = mcpClientRes->String.indexOf("let allowedPaths = [")
        let allowedPathsText = mcpClientRes->String.slice(
            ~start=allowedPathsStart,
            ~end=mcpClientRes->String.indexOfFrom("]", allowedPathsStart),
        )

        //when
        let serverPaths = matchAll(serverJs, /callBridge\(\{\s*path: '([^']*)'/g)
        let numOfBridgeCalls = matchAll(serverJs, /(callBridge\(\{)/g)->Array.length
        let allowedPaths = matchAll(allowedPathsText, /"([^"]*)"/g)

        //then
        // Every call in server.js passes the path as a literal, otherwise the paths above are incomplete.
        assertEq(serverPaths->Array.length, numOfBridgeCalls)
        assertEq(allowedPathsStart >= 0, true)
        assertEq(serverPaths, allowedPaths)
    })
})
