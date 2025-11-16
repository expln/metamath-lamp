open Expln_test
open MM_j_comment

describe("parseJComment", _ => {
    it("command without agruments", _ => {
        assertEq(
            parseJComment("abc ;"),
            [{command:"abc", args:[]}]
        )
    })
})