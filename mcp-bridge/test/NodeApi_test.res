open Test
open NodeApi

let catchExn = (f:unit => unit):exn => {
    try {
        f()
        JsError.throwWithMessage("The function didn't throw.")
    } catch {
        | exn => exn
    }
}

let throwString: unit => unit = %raw(`() => { throw "a thrown string" }`)

describe("exnToMsg", () => {
    it("returns the message of a JS error", () => {
        //given
        let exn = catchExn(() => JsError.throwWithMessage("Something failed."))

        //when
        let msg = exnToMsg(exn)

        //then
        assertEq(msg, "Something failed.")
    })

    it("converts a thrown value which is not an error to a string", () => {
        //given
        let exn = catchExn(throwString)

        //when
        let msg = exnToMsg(exn)

        //then
        assertEq(msg, "a thrown string")
    })
})
