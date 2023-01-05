open Expln_test
open MM_context
open MM_wrk_search_asrt

let createFrame = (asrt:expr, varTypes:array<int>):frame => {
    {
        disj: Belt.Map.Int.empty,
        hyps: [],
        asrt,
        label: "",
        description: "",
        frameVarToSymb: Belt_MapInt.empty,
        varTypes,
        numOfVars: varTypes->Js.Array2.length,
        numOfArgs: 0,
    }
}

describe("frameMatchesPattern", _ => {
    it("matches patters correctly", _ => {
        assertEq(
            frameMatchesPattern(createFrame([-1],[]), []), 
            true
        )
        assertEq(
            frameMatchesPattern(createFrame([-1,-5,0,1,-4],[-10, -2]), []), 
            true
        )
        
        assertEq(
            frameMatchesPattern(createFrame([-1],[]), [-1]), 
            true
        )
        assertEq(
            frameMatchesPattern(createFrame([-1],[]), [-2]), 
            false
        )

        assertEq(
            frameMatchesPattern(createFrame([-1,-5,0,1,-4],[-10, -2]), [-10,-4]), 
            true
        )

        assertEq(
            frameMatchesPattern(createFrame([-1,0,-7,1,-4,2,-2],[-10, -11, -12]), [-10, -11, -12]), 
            true
        )

        assertEq(
            frameMatchesPattern(createFrame([-1,0,-7,1,-4,0,-2],[-10, -11, -12]), [-10, -11, -12]), 
            false
        )
    })
})