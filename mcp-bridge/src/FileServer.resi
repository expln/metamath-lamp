// Serves local files over HTTP, so that a metamath-lamp tab can download them like any other web source.
// See the URL format in FileServer.res.

// The path prefix of all URLs served by the file server.
let filesUrlPrefix: string

/*
* Parses a list of directories separated by path.delimiter (':' on Linux and macOS, ';' on Windows).
* Relative directories are resolved against the current directory of the process.
* Returns the real paths (with symbolic links resolved) of the directories.
* Entries which are not existing directories are skipped with a warning.
* */
let parseAllowedDirs: option<string> => array<string>

type t

type makeParams = {
    // Real paths of directories which may be served (with all their subdirectories),
    // as returned by parseAllowedDirs(). Must not be empty.
    allowedDirs: array<string>,
    // Decides which browser pages (by origin) may download the files.
    isOriginAllowed: string => bool,
}

/*
* Creates a file server. Throws if allowedDirs is empty.
* */
let make: makeParams => t

let describeAllowedDirs: t => string

// Either url or err is defined.
type srcToUrlResult = {
    url?: string,
    err?: string,
}

/*
* Converts a local file given to load_context into a URL which the browser can download.
* src - a file:// URL, or an absolute local path (~ is expanded).
* baseUrl - the base URL of this server, e.g. http://127.0.0.1:8765.
* */
let localSrcToUrl: (t, string, string) => promise<srcToUrlResult>

/*
* Handles an HTTP request. Resolves to false if the request is not for this server
* (the URL doesn't start with filesUrlPrefix), so the caller can respond to it differently.
* */
let handleRequest: (t, NodeApi.Http.request, NodeApi.Http.response) => promise<bool>
