// Serves local files over HTTP, so that a metamath-lamp tab can download them like any other web source.
// Only files inside the allowed directories are served, and only to browser pages with an allowed origin.
//
// A local file /a/b/set.mm is served at http://127.0.0.1:<port>/files/a/b/set.mm
// (on Windows, C:\a\set.mm is served at /files/C:/a/set.mm).
// The URL mirrors the local path, so files included via $[ ... $] are resolved by metamath-lamp
// relative to the including file, and the same file always gets the same URL,
// which lets the user mark http://127.0.0.1:<port>/files/* as trusted in metamath-lamp once.

import { createReadStream } from 'node:fs'
import { realpath, stat } from 'node:fs/promises'
import { homedir } from 'node:os'
import path from 'node:path'
import { pipeline } from 'node:stream'
import { fileURLToPath } from 'node:url'
import { log } from './browser-bridge.js'

export const FILES_URL_PREFIX = '/files'

const isWindows = process.platform === 'win32'

/*
* Expands a leading ~ to the home directory.
* */
function expandHome(p) {
    if (p === '~') {
        return homedir()
    }
    if (p.startsWith('~/') || (isWindows && p.startsWith('~\\'))) {
        return path.join(homedir(), p.substring(2))
    }
    return p
}

/*
* Parses a list of directories separated by path.delimiter (':' on Linux and macOS, ';' on Windows).
* Relative directories are resolved against the current directory of the process.
* */
export function parseAllowedDirs(str) {
    return (str ?? '').split(path.delimiter)
        .map(s => s.trim())
        .filter(s => s.length > 0)
        .map(s => path.resolve(expandHome(s)))
}

/*
* Returns true if the absolute path child is dir itself or is inside dir.
* */
function isInsideDir(dir, child) {
    const rel = path.relative(dir, child)
    return rel === '' || (rel.split(path.sep)[0] !== '..' && !path.isAbsolute(rel))
}

function absPathToUrlPath(absPath) {
    const segments = absPath.split(path.sep).filter(s => s.length > 0)
    return FILES_URL_PREFIX + '/' + segments.map(encodeURIComponent).join('/')
}

/*
* Converts the path part of a URL to an absolute local path, or returns undefined if it is not a files URL.
* */
function urlPathToAbsPath(urlPath) {
    if (!urlPath.startsWith(FILES_URL_PREFIX + '/')) {
        return undefined
    }
    let segments
    try {
        segments = urlPath.substring(FILES_URL_PREFIX.length + 1).split('/').map(decodeURIComponent)
    } catch {
        return undefined
    }
    if (segments.some(s => s.includes('/') || s.includes('\\') || s.includes('\0'))) {
        return undefined
    }
    const absPath = isWindows ? segments.join('\\') : '/' + segments.join('/')
    return path.isAbsolute(absPath) ? path.resolve(absPath) : undefined
}

export class FileServer {
    /*
    * allowedDirs - absolute paths of directories which may be served (with all their subdirectories).
    * isOriginAllowed - origin => boolean, decides which browser pages may download the files.
    * */
    constructor({allowedDirs, isOriginAllowed}) {
        this.allowedDirs = allowedDirs
        this.isOriginAllowed = isOriginAllowed
    }

    describeAllowedDirs() {
        return this.allowedDirs.length === 0 ? '(none)' : this.allowedDirs.join(', ')
    }

    /*
    * Checks that absPath is an existing regular file inside one of the allowed directories.
    * Symbolic links are resolved first, so a link cannot point outside the allowed directories.
    * Resolves to {realPath, size} or {err}.
    * */
    async checkFile(absPath) {
        if (this.allowedDirs.length === 0) {
            return {
                err: 'Loading local files is disabled, because no allowed directories are configured. '
                    + 'Ask the user to set the MM_LAMP_FILES_DIRS environment variable of the MCP server.'
            }
        }
        let realPath
        try {
            realPath = await realpath(absPath)
        } catch {
            return {err: `File not found: ${absPath}`}
        }
        let isAllowed = false
        for (const dir of this.allowedDirs) {
            let realDir
            try {
                realDir = await realpath(dir)
            } catch {
                continue
            }
            if (isInsideDir(realDir, realPath)) {
                isAllowed = true
                break
            }
        }
        if (!isAllowed) {
            return {
                err: `Access denied: ${absPath} is outside the allowed directories `
                    + `(${this.describeAllowedDirs()}).`
            }
        }
        const stats = await stat(realPath)
        if (!stats.isFile()) {
            return {err: `Not a regular file: ${absPath}`}
        }
        return {realPath, size: stats.size}
    }

    /*
    * Converts a source given to load_context into a URL which the browser can download.
    * src - an http(s) URL (returned as is), a file:// URL, or an absolute local path (~ is expanded).
    * baseUrl - the base URL of this server, e.g. http://127.0.0.1:8765.
    * Resolves to {url} or {err}.
    * */
    async srcToUrl(src, baseUrl) {
        if (/^https?:\/\//i.test(src)) {
            return {url: src}
        }
        let absPath
        if (/^file:/i.test(src)) {
            try {
                absPath = fileURLToPath(src)
            } catch (e) {
                return {err: `Invalid file URL '${src}': ${e.message}`}
            }
        } else if (/^[a-z][a-z0-9+.-]*:\/\//i.test(src)) {
            return {err: `Unsupported URL scheme in '${src}': only http, https and file URLs are supported.`}
        } else {
            absPath = expandHome(src)
            if (!path.isAbsolute(absPath)) {
                return {err: `A local path must be absolute: '${src}'.`}
            }
        }
        absPath = path.resolve(absPath)
        const {err} = await this.checkFile(absPath)
        if (err !== undefined) {
            return {err}
        }
        return {url: baseUrl + absPathToUrlPath(absPath)}
    }

    /*
    * Handles an HTTP request. Returns false if the request is not for this server (the URL doesn't start with
    * FILES_URL_PREFIX), so the caller can respond to it differently.
    * */
    async handleRequest(req, res) {
        const urlPath = new URL(req.url, 'http://127.0.0.1').pathname
        if (urlPath !== FILES_URL_PREFIX && !urlPath.startsWith(FILES_URL_PREFIX + '/')) {
            return false
        }
        const sendText = (status, text, headers = {}) => {
            res.writeHead(status, {...headers, 'Content-Type': 'text/plain; charset=utf-8'})
            res.end(text)
        }
        // Browsers always send Origin with cross-origin requests, and a metamath-lamp page is never served
        // from this server, so a request without Origin doesn't come from metamath-lamp.
        // Checking the origin prevents other web pages opened in the browser from reading local files.
        const origin = req.headers.origin
        if (origin === undefined || !this.isOriginAllowed(origin)) {
            sendText(403, `Origin is not allowed: ${origin}`)
            return true
        }
        const corsHeaders = {
            'Access-Control-Allow-Origin': origin,
            'Vary': 'Origin',
        }
        if (req.method === 'OPTIONS') {
            res.writeHead(204, {
                ...corsHeaders,
                'Access-Control-Allow-Methods': 'GET, HEAD',
                'Access-Control-Allow-Headers': req.headers['access-control-request-headers'] ?? '',
                // Lets public pages (e.g. https://expln.github.io) access this local server in Chrome.
                'Access-Control-Allow-Private-Network': 'true',
                'Access-Control-Max-Age': '600',
            })
            res.end()
            return true
        }
        if (req.method !== 'GET' && req.method !== 'HEAD') {
            sendText(405, `Method not allowed: ${req.method}`, {...corsHeaders, 'Allow': 'GET, HEAD, OPTIONS'})
            return true
        }
        const absPath = urlPathToAbsPath(urlPath)
        if (absPath === undefined) {
            sendText(400, `Invalid file path in the URL: ${urlPath}`, corsHeaders)
            return true
        }
        const {realPath, size, err} = await this.checkFile(absPath)
        if (err !== undefined) {
            sendText(err.startsWith('File not found') ? 404 : 403, err, corsHeaders)
            return true
        }
        res.writeHead(200, {
            ...corsHeaders,
            'Content-Type': 'text/plain; charset=utf-8',
            'Content-Length': size,
            'Cache-Control': 'no-store',
        })
        if (req.method === 'HEAD') {
            res.end()
            return true
        }
        // pipeline() ends the response when the file is sent, and destroys both streams if either of them fails
        // or the client disconnects early, so the file is always closed.
        pipeline(createReadStream(realPath), res, err => {
            // ERR_STREAM_PREMATURE_CLOSE means the client disconnected (e.g. the user cancelled the download).
            if (err && err.code !== 'ERR_STREAM_PREMATURE_CLOSE') {
                log(`Error sending ${realPath}: ${err.message}`)
            }
        })
        return true
    }
}
