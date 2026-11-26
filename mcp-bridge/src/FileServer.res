// Serves local files over HTTP, so that a metamath-lamp tab can download them like any other web source.
// Only files inside the allowed directories are served, and only to browser pages with an allowed origin.
//
// A local file /a/b/set.mm is served at http://127.0.0.1:<port>/files/a/b/set.mm
// (on Windows, C:\a\set.mm is served at /files/C:/a/set.mm).
// The URL mirrors the local path, so files included via $[ ... $] are resolved by metamath-lamp
// relative to the including file, and the same file always gets the same URL,
// which lets the user mark http://127.0.0.1:<port>/files/* as trusted in metamath-lamp once.

open NodeApi

let filesUrlPrefix = "/files"

let isWindows = platform == "win32"

let log = BrowserBridge.log

/*
* Expands a leading ~ to the home directory.
* */
let expandHome = (p:string):string => {
    if (p == "~") {
        Os.homedir()
    } else if (p->String.startsWith("~/") || (isWindows && p->String.startsWith("~\\"))) {
        Path.join(Os.homedir(), p->String.substring(~start=2))
    } else {
        p
    }
}

let parseAllowedDirs = (str:option<string>):array<string> => {
    let dirs = []
    str->Option.getOr("")
        ->String.split(Path.delimiter)
        ->Array.map(String.trim)
        ->Array.filter(s => s->String.length > 0)
        ->Array.forEach(entry => {
            let realDir = try {
                Some(Fs.realpathSync(Path.resolve(expandHome(entry))))
            } catch {
                | _ => None
            }
            switch realDir {
                | None => log(`Ignoring the allowed directory '${entry}': it doesn't exist.`)
                | Some(realDir) => {
                    if (!(Fs.statSync(realDir)->Fs.isDirectory)) {
                        log(`Ignoring the allowed directory '${entry}': it is not a directory.`)
                    } else if (!(dirs->Array.includes(realDir))) {
                        dirs->Array.push(realDir)
                    }
                }
            }
        })
    dirs
}

/*
* Returns true if the absolute path child is dir itself or is inside dir.
* */
let isInsideDir = (dir:string, child:string):bool => {
    let rel = Path.relative(dir, child)
    rel == "" || (rel->String.split(Path.sep)->Array.getUnsafe(0) != ".." && !Path.isAbsolute(rel))
}

let absPathToUrlPath = (absPath:string):string => {
    let segments = absPath->String.split(Path.sep)->Array.filter(s => s->String.length > 0)
    filesUrlPrefix ++ "/" ++ segments->Array.map(encodeURIComponent)->Array.join("/")
}

/*
* Converts the path part of a URL to an absolute local path, or returns None if it is not a files URL.
* */
let urlPathToAbsPath = (urlPath:string):option<string> => {
    if (!(urlPath->String.startsWith(filesUrlPrefix ++ "/"))) {
        None
    } else {
        let segments = try {
            Some(
                urlPath->String.substring(~start=filesUrlPrefix->String.length + 1)
                    ->String.split("/")
                    ->Array.map(decodeURIComponent)
            )
        } catch {
            | _ => None
        }
        switch segments {
            | None => None
            | Some(segments) => {
                let hasInvalidSegment = segments->Array.some(s => {
                    s->String.includes("/") || s->String.includes("\\") || s->String.includes("\u0000")
                })
                if (hasInvalidSegment) {
                    None
                } else {
                    let absPath = isWindows ? segments->Array.join("\\") : "/" ++ segments->Array.join("/")
                    Path.isAbsolute(absPath) ? Some(Path.resolve(absPath)) : None
                }
            }
        }
    }
}

type t = {
    allowedDirs: array<string>,
    isOriginAllowed: string => bool,
}

type makeParams = {
    allowedDirs: array<string>,
    isOriginAllowed: string => bool,
}

let make = ({allowedDirs, isOriginAllowed}:makeParams):t => {
    if (allowedDirs->Array.length == 0) {
        JsError.throwWithMessage("FileServer requires at least one allowed directory.")
    }
    {allowedDirs, isOriginAllowed}
}

let describeAllowedDirs = (t:t):string => t.allowedDirs->Array.join(", ")

type fileInfo = {
    realPath: string,
    size: float,
}

type checkFileErr =
    | FileNotFound(string)
    | AccessDenied(string)

let checkFileErrToMsg = (err:checkFileErr):string => {
    switch err {
        | FileNotFound(msg) | AccessDenied(msg) => msg
    }
}

/*
* Checks that absPath is an existing regular file inside one of the allowed directories.
* Symbolic links are resolved first, so a link cannot point outside the allowed directories.
* */
let checkFile = async (t:t, absPath:string):result<fileInfo,checkFileErr> => {
    let realPath = try {
        Some(await Fs.realpath(absPath))
    } catch {
        | _ => None
    }
    switch realPath {
        | None => Error(FileNotFound(`File not found: ${absPath}`))
        | Some(realPath) => {
            if (!(t.allowedDirs->Array.some(dir => isInsideDir(dir, realPath)))) {
                Error(AccessDenied(
                    `Access denied: ${absPath} is outside the allowed directories `
                        ++ `(${describeAllowedDirs(t)}).`
                ))
            } else {
                let stats = await Fs.stat(realPath)
                if (!(stats->Fs.isFile)) {
                    Error(AccessDenied(`Not a regular file: ${absPath}`))
                } else {
                    Ok({realPath, size: stats->Fs.size})
                }
            }
        }
    }
}

type srcToUrlResult = {
    url?: string,
    err?: string,
}

let localSrcToUrl = async (t:t, src:string, baseUrl:string):srcToUrlResult => {
    let absPath = if (src->String.toLowerCase->String.startsWith("file:")) {
        try {
            Ok(Url.fileURLToPath(src))
        } catch {
            | exn => Error(`Invalid file URL '${src}': ${exnToMsg(exn)}`)
        }
    } else {
        let absPath = expandHome(src)
        if (!Path.isAbsolute(absPath)) {
            Error(`A local path must be absolute: '${src}'.`)
        } else {
            Ok(absPath)
        }
    }
    switch absPath {
        | Error(err) => {err: err}
        | Ok(absPath) => {
            let absPath = Path.resolve(absPath)
            switch await checkFile(t, absPath) {
                | Error(err) => {err: checkFileErrToMsg(err)}
                | Ok(_) => {url: baseUrl ++ absPathToUrlPath(absPath)}
            }
        }
    }
}

let sendText = (res:Http.response, status:int, text:string, headers:array<(string,string)>):unit => {
    res->Http.writeHead(
        status,
        Dict.fromArray(headers->Array.concat([("Content-Type", "text/plain; charset=utf-8")]))
    )
    res->Http.endWithText(text)
}

let handleRequest = async (t:t, req:Http.request, res:Http.response):bool => {
    let urlPath = Url.makeWithBase(req->Http.url, "http://127.0.0.1")->Url.pathname
    if (urlPath != filesUrlPrefix && !(urlPath->String.startsWith(filesUrlPrefix ++ "/"))) {
        false
    } else {
        let headers = req->Http.headers
        // Browsers always send Origin with cross-origin requests, and a metamath-lamp page is never served
        // from this server, so a request without Origin doesn't come from metamath-lamp.
        // Checking the origin prevents other web pages opened in the browser from reading local files.
        switch headers->Dict.get("origin") {
            | Some(origin) if t.isOriginAllowed(origin) => {
                let corsHeaders = [
                    ("Access-Control-Allow-Origin", origin),
                    ("Vary", "Origin"),
                ]
                let method = req->Http.method
                if (method == "OPTIONS") {
                    res->Http.writeHead(204, Dict.fromArray(corsHeaders->Array.concat([
                        ("Access-Control-Allow-Methods", "GET, HEAD"),
                        (
                            "Access-Control-Allow-Headers",
                            headers->Dict.get("access-control-request-headers")->Option.getOr("")
                        ),
                        // Lets public pages (e.g. https://expln.github.io) access this local server in Chrome.
                        ("Access-Control-Allow-Private-Network", "true"),
                        ("Access-Control-Max-Age", "600"),
                    ])))
                    res->Http.end
                } else if (method != "GET" && method != "HEAD") {
                    res->sendText(
                        405,
                        `Method not allowed: ${method}`,
                        corsHeaders->Array.concat([("Allow", "GET, HEAD, OPTIONS")])
                    )
                } else {
                    switch urlPathToAbsPath(urlPath) {
                        | None => res->sendText(400, `Invalid file path in the URL: ${urlPath}`, corsHeaders)
                        | Some(absPath) => {
                            switch await checkFile(t, absPath) {
                                | Error(FileNotFound(msg)) => res->sendText(404, msg, corsHeaders)
                                | Error(AccessDenied(msg)) => res->sendText(403, msg, corsHeaders)
                                | Ok({realPath, size}) => {
                                    res->Http.writeHead(200, Dict.fromArray(corsHeaders->Array.concat([
                                        ("Content-Type", "text/plain; charset=utf-8"),
                                        ("Content-Length", size->Float.toString),
                                        ("Cache-Control", "no-store"),
                                    ])))
                                    if (method == "HEAD") {
                                        res->Http.end
                                    } else {
                                        Stream.pipeline(Fs.createReadStream(realPath), res, err => {
                                            switch err->Nullable.toOption {
                                                // ERR_STREAM_PREMATURE_CLOSE means the client disconnected
                                                // (e.g. the user cancelled the download).
                                                | Some(err) if err.code != Some("ERR_STREAM_PREMATURE_CLOSE") =>
                                                    log(`Error sending ${realPath}: ${err.message}`)
                                                | _ => ()
                                            }
                                        })
                                    }
                                }
                            }
                        }
                    }
                }
            }
            | origin => res->sendText(403, `Origin is not allowed: ${origin->anyToString}`, [])
        }
        true
    }
}
