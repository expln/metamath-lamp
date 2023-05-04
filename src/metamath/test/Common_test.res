open Expln_test
open Common

describe("strToSafeBase64", _ => {
    it("does not put plus and slash chars into the output", _ => {
        assertEq(strToBase64(" -> ("), "IC0+ICg=")
        assertEq(strToSafeBase64(" -> ("), "IC0-ICg=")

        assertEq(strToBase64(" -? ("), "IC0/ICg=")
        assertEq(strToSafeBase64(" -? ("), "IC0_ICg=")
    })

    it("correctly decodes base64 string containing minus and underscore signs", _ => {
        assertEq(safeBase64ToStr("IC0-ICg="), " -> (")
        assertEq(safeBase64ToStr("IC0_ICg="), " -? (")
    })
})