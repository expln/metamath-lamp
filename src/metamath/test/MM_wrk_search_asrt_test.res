open Expln_test
open MM_context
open MM_wrk_search_asrt

let createFrame = (asrt:expr, varTypes:array<int>):frame => {
    {
        ord:0,
        isAxiom:false,
        disj: Belt.Map.Int.empty,
        hyps: [],
        asrt,
        label: "",
        frameVarToSymb: [],
        varTypes,
        numOfVars: varTypes->Js.Array2.length,
        numOfArgs: 0,
        descr: None,
        proof: None,
        isDisc:false,
        isDepr:false,
        isTranDepr:false,
        dbg: None,
    }
}

describe("frameMatchesPattern", _ => {
    it("matches patterns correctly", _ => {
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