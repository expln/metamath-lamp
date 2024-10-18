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
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), 
                ~searchPattern=[], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), 
                ~searchPattern=[ {flags:[], varPat:[], constPat:[]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-5,0,1,-4],~varTypes=[-10,-2]), 
                ~searchPattern=[ {flags:[], varPat:[], constPat:[]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), 
                ~searchPattern=[ {flags:[], varPat:[-1], constPat:[-1]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1],~varTypes=[]), 
                ~searchPattern=[ {flags:[], varPat:[-1], constPat:[-2]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-5,0,1,-4],~varTypes=[-10, -2]), 
                ~searchPattern=[ {flags:[], varPat:[-10,-4], constPat:[-10,-4]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,0,-7,1,-4,2,-2],~varTypes=[-10, -11, -12]), 
                ~searchPattern=[ {flags:[], varPat:[-10, -11, -12], constPat:[-10, -11, -12]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,0,-7,1,-4,0,-2],~varTypes=[-10, -11]), 
                ~searchPattern=[ {flags:[], varPat:[-10, -11, -12], constPat:[-10, -11, -12]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-1,-7,-1,-4,-3,-2,2,1,0],~varTypes=[-10,-11,-12]), 
                ~searchPattern=[ {flags:[], varPat:[-12,-11,-10], constPat:[-12,-11,-10]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ),
            true
        )
    })

    it("matches patterns correctly when pattern consists of constants and variables", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-2,0,-3,-4,1,-5,-6,0,-7,-8],~varTypes=[-10,-11]), 
                ~searchPattern=[ {flags:[], varPat:[100,-4,123,-5,100], constPat:[-10,-4,-11,-5,-10]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[-1,-2,0,-3,-4,1,-5,-6,1,-7,-8],~varTypes=[-10,-11]), 
                ~searchPattern=[ {flags:[], varPat:[100,-4,123,-5,100], constPat:[-10,-4,-11,-5,-10]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )

        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
                ~searchPattern=[ {flags:[], varPat:[100,101,101,102], constPat:[-10,-11,-11,-13]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
                ~searchPattern=[ {flags:[], varPat:[100,100,101,102], constPat:[-10,-11,-11,-13]}], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~asrt=[0,1,1,2],~varTypes=[-10,-11,-13]), 
                ~searchPattern=[ {flags:[], varPat:[99,100,101,102], constPat:[-10,-11,-11,-13]}], 
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
            frameMatchesPattern(~frame=createFrame(), 
            ~searchPattern=[ {flags:[], varPat:varPat, constPat:constPat}], 
            ~mapping=Belt_HashMapInt.make(~hintSize=10)),
            true
        )
        (hyps->Array.getUnsafe(1))[0] = 1
        assertEq(
            frameMatchesPattern(~frame=createFrame(), 
            ~searchPattern=[ {flags:[], varPat:varPat, constPat:constPat}], 
            ~mapping=Belt_HashMapInt.make(~hintSize=10)),
            false
        )
    })

    let arr = -12
    let dblarr = -13
    let eq = -14
    let wff = -1
    let setvar = -2
    let class = -3

    it("only asrt pattern", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Asrt], varPat:[100,arr,101], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Asrt], varPat:[100,arr,100], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Asrt], varPat:[100,eq,101], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("only one hyp pattern", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,100], constPat:[wff,eq,wff]}
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
    })

    it("asrt and one hyp", _ => {
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[200,arr,201], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,100], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[200,arr,201], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
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
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,eq,101], constPat:[wff,eq,eq,wff]},
                    {flags:[Hyp], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,arr,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
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
            frameMatchesPattern(
                ~frame=createFrame(~varTypes=[wff,wff,wff],~hyps=[[0,eq,1],[1,eq,2]], ~asrt=[0, arr, 1, arr, 2]),
                ~searchPattern=[
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Hyp], varPat:[100,eq,101], constPat:[wff,eq,wff]},
                    {flags:[Asrt], varPat:[100,arr,101], constPat:[wff,arr,wff]},
                ], 
                ~mapping=Belt_HashMapInt.make(~hintSize=10)
            ), 
            true
        )
    })

    it("single sub-pattern with Adj matches asrt or hyp at the left edge", _ => {
        let frmExprToMatch = [dblarr,2,arr,3,eq,0,eq,1,]
        let frmExpr1 = [0, eq, 1, eq, 2]
        let frmExpr2 = [1,eq,0]
        let varPat = [dblarr,100,arr,101]
        let constPat = [dblarr,wff,arr,wff]
        let mapping=Belt_HashMapInt.make(~hintSize=10)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExprToMatch, frmExpr2, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:constPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )

        //when distinct vars condition is not met
        let varPat2 = varPat->Array.copy
        varPat2[3] = varPat2->Array.getUnsafe(1)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat2, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )

        //when the expression to match is not adjacent
        let frmExprToMismatch = frmExprToMatch->Array.copy
        frmExprToMismatch->Array.splice(~start=1, ~remove=0, ~insert=[eq])
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
    })

    it("single sub-pattern with Adj matches asrt or hyp in the middle", _ => {
        let frmExprToMatch = [arr,2,dblarr,2,arr,3,eq,0,eq,1,]
        let frmExpr1 = [0, eq, 1, eq, 2]
        let frmExpr2 = [1,eq,0]
        let varPat = [dblarr,100,arr,101]
        let constPat = [dblarr,wff,arr,wff]
        let mapping=Belt_HashMapInt.make(~hintSize=10)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExprToMatch, frmExpr2, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:constPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )

        //when distinct vars condition is not met
        let varPat2 = varPat->Array.copy
        varPat2[3] = varPat2->Array.getUnsafe(1)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat2, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )

        //when the expression to match is not adjacent
        let frmExprToMismatch = frmExprToMatch->Array.copy
        frmExprToMismatch->Array.splice(~start=3, ~remove=0, ~insert=[eq])
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
    })

    it("single sub-pattern with Adj matches asrt or hyp at the right edge", _ => {
        let frmExprToMatch = [eq,0,eq,1,dblarr,2,arr,3]
        let frmExpr1 = [0, eq, 1, eq, 2]
        let frmExpr2 = [1,eq,0]
        let varPat = [dblarr,100,arr,101]
        let constPat = [dblarr,wff,arr,wff]
        let mapping=Belt_HashMapInt.make(~hintSize=10)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExprToMatch, frmExpr2, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr2, frmExprToMatch, ], ~asrt=frmExpr1, ),
                ~searchPattern=[ {flags:[Adj], varPat:constPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )        

        //when distinct vars condition is not met
        let varPat2 = varPat->Array.copy
        varPat2[3] = varPat2->Array.getUnsafe(1)
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat2, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )

        //when the expression to match is not adjacent
        let frmExprToMismatch = frmExprToMatch->Array.copy
        frmExprToMismatch->Array.splice(~start=5, ~remove=0, ~insert=[eq])
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[Adj], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            false
        )
        assertEq(
            frameMatchesPattern(
                ~frame=createFrame( ~varTypes=[setvar,class,wff,wff], ~hyps=[ frmExpr1, frmExpr2, ], ~asrt=frmExprToMismatch, ),
                ~searchPattern=[ {flags:[], varPat:varPat, constPat:constPat} ], 
                ~mapping
            ), 
            true
        )
    })
})

describe("parseSearchStr", _ => {
    it("parses search patterns as expected", _ => {
        assertEq(parseSearchStr("")->Result.getExn, [])
        assertEq(parseSearchStr("a")->Result.getExn, [([], ["a"])])
        assertEq(parseSearchStr("abc")->Result.getExn, [([], ["abc"])])
        assertEq(parseSearchStr("a b c")->Result.getExn, [([], ["a", "b", "c"])])
        assertEq(parseSearchStr("abc def")->Result.getExn, [([], ["abc", "def"])])
        assertEq(parseSearchStr("abc def ghi")->Result.getExn, [([], ["abc", "def", "ghi"])])
        assertEq(parseSearchStr("$a abc def")->Result.getExn, [([Asrt], ["abc", "def"])])
        assertEq(parseSearchStr("$h abc def")->Result.getExn, [([Hyp], ["abc", "def"])])
        assertEq(parseSearchStr("$+ abc def")->Result.getExn, [([Adj], ["abc", "def"])])
        assertEq(parseSearchStr("$! abc def")->Result.getExn, [([Exact], ["abc", "def"])])
        assertEq(parseSearchStr("$!\tabc\ndef")->Result.getExn, [([Exact], ["abc", "def"])])
        assertEq(parseSearchStr("$a+ abc def")->Result.getExn, [([Asrt, Adj], ["abc", "def"])])
        assertEq(parseSearchStr("$a! abc def")->Result.getExn, [([Asrt, Exact], ["abc", "def"])])
        assertEq(parseSearchStr("$ah abc def"), Error("A sub-pattern cannot be both a hypothesis and an assertion at position 3."))
        assertEq(parseSearchStr("$h+ abc def")->Result.getExn, [([Hyp, Adj], ["abc", "def"])])
        assertEq(parseSearchStr("$h! abc def")->Result.getExn, [([Hyp, Exact], ["abc", "def"])])
        assertEq(parseSearchStr("$ha abc def"), Error("A sub-pattern cannot be both a hypothesis and an assertion at position 3."))
        assertEq(parseSearchStr("$!+ abc def"), Error("A sub-pattern cannot be both Adjacent and Exact at position 3."))
        assertEq(parseSearchStr("$a") , Error("At least one symbol is expected at position 3."))

        assertEq(parseSearchStr("$h abc def $a ghi jkl")->Result.getExn, [([Hyp], ["abc", "def"]), ([Asrt], ["ghi", "jkl"])])
        assertEq(parseSearchStr("   $h+   abc \n $a! ghi \t")->Result.getExn, [([Hyp, Adj], ["abc"]), ([Asrt, Exact], ["ghi"])])
        assertEq(
            parseSearchStr("$h+ x = A $h+ ph <-> ps $a! |- ph")->Result.getExn, 
            [([Hyp, Adj], ["x", "=", "A"]), ([Hyp, Adj], ["ph", "<->", "ps"]), ([Asrt, Exact], ["|-", "ph"])]
        )

        assertEq(parseSearchStr("abc  $a! ghi "), Error("Each sub-pattern must be either a hypothesis or an assertion."))
        assertEq(parseSearchStr("$h abc  $h $a ghi "), Error("At least one symbol is expected at position 12."))

    })
})