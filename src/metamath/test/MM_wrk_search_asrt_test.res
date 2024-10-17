open Expln_test
open MM_context
open MM_wrk_search_asrt
open Expln_utils_common

let createFrame = (~asrt:expr, ~varTypes:array<int>, ~hyps:array<array<int>> = []):frame => {
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
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), ~varPat=[], ~constPat=[], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-5,0,1,-4],~varTypes=[-10,-2]), ~varPat=[], ~constPat=[],
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), ~varPat=[-1], ~constPat=[-1], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), ~varPat=[-2], ~constPat=[-2], ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-5,0,1,-4],~varTypes=[-10, -2]), ~varPat=[-10,-4], ~constPat=[-10,-4], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,0,-7,1,-4,2,-2],~varTypes=[-10, -11, -12]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,0,-7,1,-4,0,-2],~varTypes=[-10, -11]), ~varPat=[-10, -11, -12], ~constPat=[-10, -11, -12], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-1,-7,-1,-4,-3,-2,2,1,0],~varTypes=[-10,-11,-12]), ~varPat=[-12,-11,-10], ~constPat=[-12,-11,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
    })

    it("matches patterns correctly when pattern consists of constants and variables", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],~varTypes=[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-2,0,-3,-4,1,-5,-6,1,-7,-8],~varTypes=[-10,-11]), 
                ~varPat=[100,-4,123,-5,100], ~constPat=[-10,-4,-11,-5,-10], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
                ~varPat=[100,101,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
                ~varPat=[100,100,101,102], ~constPat=[-10,-11,-11,-13], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
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

        let createFrame = () => createFrame(~asrt=[-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],~varTypes=[-10,-40], ~hyps)

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

describe("frameMatchesPattern2", _ => {
    let pOpen = -10
    let pClose = -11
    let arr = -12
    let eq = -13
    let wff = -1
    let setvar = -2
    let a = 1
    let b = 2
    let c = 3
    let d = 4

    it("only asrt pattern", _ => {
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Asrt], varPat:[100,arr,101], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Asrt], varPat:[100,arr,100], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Asrt], varPat:[100,eq,101], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("only one hyp pattern", _ => {
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,100], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("asrt and one hyp", _ => {
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[200,arr,201], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,100], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[200,arr,201], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[200,arr,200], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("two hyps", _ => {
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,eq,101], constPat:[wff,eq,eq,wff]},
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,arr,100], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("asrt and two hyps", _ => {
        assertEq(
            frameMatchesPattern2(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~patterns=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
    })
})

describe("parseSearchStr", _ => {
    it("parses search patterns as expected", _ => {
        let res = parseSearchStr("$h a b c $a d e f")
        Console.log2(`res`, res->stringify)
    })
})