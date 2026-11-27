// Minimal bindings to the Node.js API used by the MCP bridge.

@val @scope("process") external platform: string = "platform"

// String(value), converts any value to a string the same way as template literals in JS do.
@val external anyToString: 'a => string = "String"

@val @scope("JSON") external stringifyAny: 'a => string = "stringify"

// Errors passed to Node.js callbacks and event handlers.
type nodeError = {
    message: string,
    code?: string,
}

@send external unrefInterval: intervalId => intervalId = "unref"

/*
* Returns the message of an exception: the message of a JS error, or the exception converted to a string.
* */
let exnToMsg = (exn:exn):string => {
    switch exn {
        | JsExn(jsExn) => jsExn->JsExn.message->Option.getOr(jsExn->anyToString)
        | exn => exn->anyToString
    }
}

module Path = {
    @module("node:path") @val external sep: string = "sep"
    @module("node:path") @val external delimiter: string = "delimiter"
    @module("node:path") external join: (string, string) => string = "join"
    @module("node:path") external resolve: string => string = "resolve"
    @module("node:path") external relative: (string, string) => string = "relative"
    @module("node:path") external isAbsolute: string => bool = "isAbsolute"
}

module Os = {
    @module("node:os") external homedir: unit => string = "homedir"
}

module Url = {
    // The global WHATWG URL class.
    type t
    // Both throw TypeError on an invalid URL.
    @new external make: string => t = "URL"
    @new external makeWithBase: (string, string) => t = "URL"
    @get external protocol: t => string = "protocol"
    @get external hostname: t => string = "hostname"
    @get external pathname: t => string = "pathname"

    // Throws on invalid input.
    @module("node:url") external fileURLToPath: string => string = "fileURLToPath"
}

module Fs = {
    type stats
    @send external isFile: stats => bool = "isFile"
    @send external isDirectory: stats => bool = "isDirectory"
    @get external size: stats => float = "size"

    // Both throw if the file doesn't exist.
    @module("node:fs") external realpathSync: string => string = "realpathSync"
    @module("node:fs") external statSync: string => stats = "statSync"

    // Both reject if the file doesn't exist.
    @module("node:fs/promises") external realpath: string => promise<string> = "realpath"
    @module("node:fs/promises") external stat: string => promise<stats> = "stat"

    type readStream
    @module("node:fs") external createReadStream: string => readStream = "createReadStream"
}

module Http = {
    type request
    @get external method: request => string = "method"
    @get external url: request => string = "url"
    @get external headers: request => Dict.t<string> = "headers"

    type response
    @send external writeHead: (response, int, Dict.t<string>) => unit = "writeHead"
    @send external end: response => unit = "end"
    @send external endWithText: (response, string) => unit = "end"
    @get external headersSent: response => bool = "headersSent"

    type server
    @module("node:http") external createServer: ((request, response) => unit) => server = "createServer"
    @send external listen: (server, int, string) => unit = "listen"
}

module Stream = {
    // pipeline() ends the response when the file is sent, and destroys both streams if either of them fails
    // or the destination is closed early, so the file is always closed.
    @module("node:stream")
    external pipeline: (Fs.readStream, Http.response, Nullable.t<nodeError> => unit) => unit = "pipeline"
}
