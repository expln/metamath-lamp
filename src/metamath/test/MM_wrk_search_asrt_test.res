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
        varHyps: [],
        numOfVars: varTypes->Array.length,
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
    it("matches patterns correctly when pattern consists of constants only", _ => {
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1],[]), ~varPat=[], ~constPat=[], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,-5,0,1,-4],[-10,-2]), ~varPat=[], ~constPat=[],
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1],[]), ~varPat=[-1], ~constPat=[-1], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1],[]), ~varPat=[-2], ~constPat=[-2], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,-5,0,1,-4],[-10, -2]), ~varPat=[-10,-4], ~constPat=[-10,-4], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,0,-7,1,-4,2,-2],[-10, -11, -12]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,0,-7,1,-4,0,-2],[-10, -11]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,-1,-7,-1,-4,-3,-2,2,1,0],[-10,-11,-12]), ~varPat=[-12,-11,-10], ~constPat=[-12,-11,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
    })

    it("matches patterns correctly when pattern consists of constants and variables", _ => {
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesVarPattern(
                createFrame([-1,-2,0,-3,-4,1,-5,-6,1,-7,-8],[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )

        assertEq(
            frameMatchesVarPattern(
                createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[100,101,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesVarPattern(
                createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[100,100,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesVarPattern(
                createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[99,100,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
    })
})