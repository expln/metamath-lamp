@val @scope("process") external platform: string = "platform"

@val external anyToString: 'a => string = "String"

@val @scope("JSON") external stringifyAny: 'a => string = "stringify"

type nodeError = {
    message: string,
    code?: string,
}

@send external unrefInterval: intervalId => intervalId = "unref"

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
    type t
    @new external make: string => t = "URL"
    @new external makeWithBase: (string, string) => t = "URL"
    @get external protocol: t => string = "protocol"
    @get external hostname: t => string = "hostname"
    @get external pathname: t => string = "pathname"

    @module("node:url") external fileURLToPath: string => string = "fileURLToPath"
}

module Fs = {
    type stats
    @send external isFile: stats => bool = "isFile"
    @send external isDirectory: stats => bool = "isDirectory"
    @get external size: stats => float = "size"

    @module("node:fs") external realpathSync: string => string = "realpathSync"
    @module("node:fs") external statSync: string => stats = "statSync"

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
    @module("node:stream")
    external pipeline: (Fs.readStream, Http.response, Nullable.t<nodeError> => unit) => unit = "pipeline"
}
