// Paths in these tests are POSIX paths, so the tests are not expected to pass on Windows.

open Test
open FileServer

let baseUrl = "http://127.0.0.1:8765"
let isOriginAllowed = _ => true

describe("make", () => {
    it("throws when there are no allowed directories", () => {
        //when, then
        assertThrows(
            () => make({allowedDirs: [], isOriginAllowed}),
            {"message": "FileServer requires at least one allowed directory."}
        )
    })
})

describe("describeAllowedDirs", () => {
    it("lists the allowed directories separated by commas", () => {
        //given
        let fileServer = make({allowedDirs: ["/home/user/mm", "/opt/mm"], isOriginAllowed})

        //when
        let text = describeAllowedDirs(fileServer)

        //then
        assertEq(text, "/home/user/mm, /opt/mm")
    })
})

describe("urlToLocalPath", () => {
    it("returns the local path of a file served by this server", () => {
        //when
        let res = urlToLocalPath(`${baseUrl}/files/home/user/set.mm`, baseUrl)

        //then
        assertEq(res, Some("/home/user/set.mm"))
    })

    it("decodes the URL encoded path segments", () => {
        //when
        let res = urlToLocalPath(`${baseUrl}/files/home/my%20dir/set%23.mm`, baseUrl)

        //then
        assertEq(res, Some("/home/my dir/set#.mm"))
    })

    it("normalizes . and .. segments", () => {
        //when
        let res = urlToLocalPath(`${baseUrl}/files/home/user/./../other/set.mm`, baseUrl)

        //then
        assertEq(res, Some("/home/other/set.mm"))
    })

    it("returns None for URLs which are not files of this server", () => {
        //when
        let res = [
            "https://us.metamath.org/metamath/set.mm",
            "http://127.0.0.1:9999/files/home/user/set.mm",
            `${baseUrl}/other/home/user/set.mm`,
            `${baseUrl}/filesx/home/user/set.mm`,
            `${baseUrl}/files`,
        ]->Array.map(urlToLocalPath(_, baseUrl))

        //then
        assertEq(res, [None, None, None, None, None])
    })

    it("returns None when a path segment is malformed or encodes a separator or a null character", () => {
        //when
        let res = [
            `${baseUrl}/files/home/a%2Fb/set.mm`,
            `${baseUrl}/files/home/a%5Cb/set.mm`,
            `${baseUrl}/files/home/a%00b/set.mm`,
            `${baseUrl}/files/home/%E0%A4%A/set.mm`,
        ]->Array.map(urlToLocalPath(_, baseUrl))

        //then
        assertEq(res, [None, None, None, None])
    })
})

describe("srcToUrl", () => {
    // Only the cases which don't access the file system are tested here.
    let fileServer = make({allowedDirs: ["/home/user/mm"], isOriginAllowed})

    itAsync("returns http and https URLs as is, even when loading local files is disabled", async () => {
        //when
        let res1 = await srcToUrl(None, "https://us.metamath.org/metamath/set.mm", baseUrl)
        let res2 = await srcToUrl(None, "HTTP://example.com/set.mm", baseUrl)

        //then
        assertEq(res1, {url: "https://us.metamath.org/metamath/set.mm"})
        assertEq(res2, {url: "HTTP://example.com/set.mm"})
    })

    itAsync("returns an error for unsupported URL schemes", async () => {
        //when
        let res = await srcToUrl(Some(fileServer), "ftp://example.com/set.mm", baseUrl)

        //then
        assertEq(
            res,
            {err: "Unsupported URL scheme in 'ftp://example.com/set.mm': only http, https and file URLs are supported."}
        )
    })

    itAsync("returns an error for local sources when loading local files is disabled", async () => {
        //given
        let expectedErr = "Loading local files is disabled, because no allowed directories are configured. "
            ++ "Ask the user to pass the --files-dirs argument to the MCP server."

        //when
        let res1 = await srcToUrl(None, "/home/user/mm/set.mm", baseUrl)
        let res2 = await srcToUrl(None, "file:///home/user/mm/set.mm", baseUrl)

        //then
        assertEq(res1, {err: expectedErr})
        assertEq(res2, {err: expectedErr})
    })

    itAsync("returns an error for relative local paths", async () => {
        //when
        let res = await srcToUrl(Some(fileServer), "mm/set.mm", baseUrl)

        //then
        assertEq(res, {err: "A local path must be absolute: 'mm/set.mm'."})
    })
})
