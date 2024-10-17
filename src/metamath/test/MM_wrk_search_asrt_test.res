open Expln_test
open MM_context
open MM_wrk_search_asrt
open Expln_utils_common

let createFrame = (asrt:expr, varTypes:array<int>, ~hyps:array<array<int>> = []):frame => {
    {
        ord:0,
        isAxiom:false,
        disj: Belt.Map.Int.empty,
        hyps: hyps->Array.mapWithIndex((hypExpr,i) => {
            typ: E,
            expr: hypExpr,
            label: "hyp_" ++ Belt_Int.toString(i)
        }),
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
            frameMatchesPattern(
                ~frame=createFrame([-1],[]), ~varPat=[], ~constPat=[], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,-5,0,1,-4],[-10,-2]), ~varPat=[], ~constPat=[],
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1],[]), ~varPat=[-1], ~constPat=[-1], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1],[]), ~varPat=[-2], ~constPat=[-2], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,-5,0,1,-4],[-10, -2]), ~varPat=[-10,-4], ~constPat=[-10,-4], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,0,-7,1,-4,2,-2],[-10, -11, -12]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,0,-7,1,-4,0,-2],[-10, -11]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,-1,-7,-1,-4,-3,-2,2,1,0],[-10,-11,-12]), ~varPat=[-12,-11,-10], ~constPat=[-12,-11,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
    })

    it("matches patterns correctly when pattern consists of constants and variables", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([-1,-2,0,-3,-4,1,-5,-6,1,-7,-8],[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[100,101,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[100,100,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame([0,1,1,2],[-10,-11,-13]), 
                ~varPat=[99,100,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
    })

    it("searches by hypotheses", _ => {
        let hyps = [
            [-1,-2,-3,-4],
            [0,-20,-30,1,-50],
            [-5,-6,-7,-8],
        ]
        let varPat = [100,-20,123,-50]
        let constPat = [-10,-20,-40,-50]

        let createFrame = () => createFrame([-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],[-10,-40], ~hyps)

        assertEq(
            frameMatchesPattern(~frame=createFrame(), ~varPat, ~constPat, ~mapping=Belt_HashMapInt.make(~hintSize=10)),
            true
        )
        (hyps->Array.getUnsafe(1))[0] = 1
        assertEq(
            frameMatchesPattern(~frame=createFrame(), ~varPat, ~constPat, ~mapping=Belt_HashMapInt.make(~hintSize=10)),
            false
        )
    })
})

describe("parseSearchStr", _ => {
    it("parses search patterns as expected", _ => {
        let res = parseSearchStr("$h a b c $a d e f")
        Console.log2(`res`, res->stringify)
    })
})