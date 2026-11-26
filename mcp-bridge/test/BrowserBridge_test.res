open Test
open BrowserBridge

describe("isOriginAllowed", () => {
    let allowedOrigins = ["https://expln.github.io"]

    it("allows http and https origins of localhost on any port", () => {
        //when
        let res = [
            "http://localhost",
            "https://localhost:3000",
            "http://127.0.0.1:8080",
            "http://[::1]:3000",
        ]->Array.map(isOriginAllowed(_, allowedOrigins))

        //then
        assertEq(res, [true, true, true, true])
    })

    it("allows origins from the list of allowed origins", () => {
        //when
        let res = isOriginAllowed("https://expln.github.io", allowedOrigins)

        //then
        assertEq(res, true)
    })

    it("rejects origins which only look like allowed ones", () => {
        //when
        let res = [
            "https://expln.github.io.evil.com",
            "http://expln.github.io",
            "http://localhost.evil.com",
            "ftp://localhost",
            "not a url",
            "",
        ]->Array.map(isOriginAllowed(_, allowedOrigins))

        //then
        assertEq(res, [false, false, false, false, false, false])
    })

    it("rejects all non-localhost origins when the list of allowed origins is empty", () => {
        //when
        let res = isOriginAllowed("https://expln.github.io", [])

        //then
        assertEq(res, false)
    })
})
