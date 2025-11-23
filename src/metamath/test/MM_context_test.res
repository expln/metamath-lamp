open Expln_test
open MM_parser
open MM_context
open Common

describe("findParentheses", _ => {
    it("finds all parentheses", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let ctx = loadContext(ast)

        //when
        let actualFoundParens = findParentheses(ctx)

        //then
        assertEq(
            actualFoundParens->Array.map(ctxIntToSymExn(ctx, _)),
            ["(",")","[","]","{","}","<.",".>"]
        )
    })
})

describe("applySingleStmt", _ => {
    it("correctly adds one constant to the root context", _ => {
        //given
        let ctx = createContext(())

        //when
        ctx->applySingleStmt(Const({symbols:["c1"]}))

        //then
        assertEq( ctx->ctxSymToIntExn("c1"), -1 )
        assertEq( ctx->ctxIntToSymExn(-1), "c1" )
        assertEq( ctx->isConst("c1"), true )
        assertEq( ctx->isVar("c1"), false )
    })

    it("correctly adds a few constants to the root context", _ => {
        //given
        let ctx = createContext(())

        //when
        ctx->applySingleStmt(Const({symbols:["c1", "c2", "c3"]}))

        //then
        assertEq( ctx->ctxSymToIntExn("c1"), -1 )
        assertEq( ctx->ctxIntToSymExn(-1), "c1" )
        assertEq( ctx->isConst("c1"), true )
        assertEq( ctx->isVar("c1"), false )

        assertEq( ctx->ctxSymToIntExn("c2"), -2 )
        assertEq( ctx->ctxIntToSymExn(-2), "c2" )
        assertEq( ctx->isConst("c2"), true )
        assertEq( ctx->isVar("c2"), false )
        
        assertEq( ctx->ctxSymToIntExn("c3"), -3 )
        assertEq( ctx->ctxIntToSymExn(-3), "c3" )
        assertEq( ctx->isConst("c3"), true )
        assertEq( ctx->isVar("c3"), false )
    })

    it("doesn't allow to add constants in inner blocks", _ => {
        //given
        let ctx = createContext(())
        ctx->applySingleStmt(Const({symbols:["c1", "c2"]}))
        ctx->openChildContext

        try {
            //when
            ctx->applySingleStmt(Const({symbols:["c3", "c4"]}))
            failMsg("The line below was supposed to throw an exception.")
        } catch {
            | MmException({msg}) => {
                //then
                assertEq( msg, "An attempt to declare a constant 'c3' in an inner block." )

                assertEq( ctx->ctxSymToIntExn("c1"), -1 )
                assertEq( ctx->ctxIntToSymExn(-1), "c1" )
                assertEq( ctx->isConst("c1"), true )
                assertEq( ctx->isVar("c1"), false )

                assertEq( ctx->ctxSymToIntExn("c2"), -2 )
                assertEq( ctx->ctxIntToSymExn(-2), "c2" )
                assertEq( ctx->isConst("c2"), true )
                assertEq( ctx->isVar("c2"), false )
            }
        }
    })
})

describe("moveConstsToBegin", _ => {
    it("descreases int codes of the specified constants", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let ctx = loadContext(ast)
        let constsToMove = "( ) [ ] { }"
        assertEq(ctx->ctxStrToIntsExn(constsToMove), [-5,-6,-12,-14,-13,-15])

        //when
        let ctx = ctx->ctxOptimizeForProver(
            ~parens=constsToMove, ~removeAsrtDescr=true, ~removeProofs=true, ~removeAddInfoComments=true,
            ~updateUsageCntForFrames=false
        )

        //then
        assertEq(ctx->ctxStrToIntsExn(constsToMove)->Js.Array2.sortInPlace, [-1,-2,-3,-4,-5,-6])
    })

    it("doesn't fail if variables or unrecognized symbols are provided", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let ctx = loadContext(ast)
        let constsToMove = "( ) [ ] { }"
        assertEq(ctx->ctxStrToIntsExn(constsToMove), [-5,-6,-12,-14,-13,-15])

        //when
        let ctx = ctx->ctxOptimizeForProver(
            ~parens="( ) [ t ] { } abc yyy", ~removeAsrtDescr=true, ~removeProofs=true, ~removeAddInfoComments=true,
            ~updateUsageCntForFrames=false
        )

        //then
        assertEq(ctx->ctxStrToIntsExn(constsToMove)->Js.Array2.sortInPlace, [-1,-2,-3,-4,-5,-6])
    })

    it("doesn't break var types", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0-moveConstsToBegin-test._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let ctx = loadContext(ast)
        let t = ctx->ctxSymToIntExn("t")
        let r = ctx->ctxSymToIntExn("r")
        let s = ctx->ctxSymToIntExn("s")
        let p = ctx->ctxSymToIntExn("P")
        let q = ctx->ctxSymToIntExn("Q")
        assertEq(ctx->getTypeOfVarExn(t)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(r)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(s)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(p)->ctxIntToSymExn(ctx, _), "wff")
        assertEq(ctx->getTypeOfVarExn(q)->ctxIntToSymExn(ctx, _), "wff")

        //when
        let ctx = ctx->ctxOptimizeForProver(
            ~parens="( ) [ ] { }", ~removeAsrtDescr=true, ~removeProofs=true, ~removeAddInfoComments=true,
            ~updateUsageCntForFrames=false
        )

        //then
        assertEq(ctx->getTypeOfVarExn(t)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(r)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(s)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(p)->ctxIntToSymExn(ctx, _), "wff")
        assertEq(ctx->getTypeOfVarExn(q)->ctxIntToSymExn(ctx, _), "wff")
    })

    it("doesn't break expr-to-hyp", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0-moveConstsToBegin-test._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let ctx = loadContext(ast, ~stopBefore="mp")
        assertEq(
            (ctx->ctxStrToIntsExn("|- ( P -> Q )")->getHypByExpr(ctx, _)->Belt_Option.getExn).label,
            "maj"
        )

        //when
        let ctx = ctx->ctxOptimizeForProver(
            ~parens="( ) [ ] { }", ~removeAsrtDescr=true, ~removeProofs=true, ~removeAddInfoComments=true,
            ~updateUsageCntForFrames=false
        )

        //then
        assertEq(
            (ctx->ctxStrToIntsExn("|- ( P -> Q )")->getHypByExpr(ctx, _)->Belt_Option.getExn).label,
            "maj"
        )
    })
})


describe("disjForEachArr", _ => {
    it("merges disjoints correctly", _ => {
        //given
        let disj = disjMake() // 1,2; 3,4,5; 6,7,8,9;
        disj->disjAddPair(1,2)
        disj->disjAddPair(3,4)
        disj->disjAddPair(3,5)
        disj->disjAddPair(4,5)
        disj->disjAddPair(6,7)
        disj->disjAddPair(6,8)
        disj->disjAddPair(6,9)
        disj->disjAddPair(7,8)
        disj->disjAddPair(7,9)
        disj->disjAddPair(8,9)


        //when
        let actual = []
        disj->disjForEachArr(arr => actual->Array.push(arr))

        //then
        assertEq(
            actual,
            [
                [1,2],
                [3,4,5],
                [6,7,8,9]
            ]
        )
    })
})

describe("normalizeDescr", _ => {

    it("replaces all whitespaces with a single whitespace and lowecase all characters", _ => {
        assertEq( normalizeDescr("A  B"), "a b", )
        assertEq( normalizeDescr(" A  B "), "a b", )
        assertEq( normalizeDescr(" A \t B "), "a b", )
        assertEq( normalizeDescr(" A \n B "), "a b", )
        assertEq( normalizeDescr(" A \t\n B "), "a b", )
        assertEq( normalizeDescr(" A \t\n B      C"), "a b c", )
    })
})

describe("frmGetPatternSearchData", _ => {

    let makeFrame = (hyps:array<hypothesis>, asrt:array<int>):frame => {
        {
            ord:0, isAxiom:false, disj: Belt_MapInt.empty, hyps, asrt, label: "", frameVarToSymb: [], varTypes: [], 
            varHyps: [], numOfVars: 0, numOfArgs: 0, descr:None, descrNorm:None, proof:None, isDisc:false, isDepr:false, 
            isTranDepr:false, dbg: None, usageCnt: 0,
        }
    }

    it("correctly calculates boundaries of statements for a frame with multiple essential hypotheses", _ => {
        //given
        let frm = makeFrame(
            [
                { typ: F, label: "1", expr: [0,1,2] },
                { typ: E, label: "2", expr: [3,4,5,6,7] },
                { typ: F, label: "3", expr: [8,9] },
                { typ: E, label: "4", expr: [10,11,12,13] },
                { typ: F, label: "5", expr: [14] },
                { typ: E, label: "6", expr: [15] },
            ],
            [16,17,18]
        )

        //when
        let patData = frmGetPatternSearchData(frm)

        //then
        let expectedRes = {
            allHypsAsrt:[3,4,5,6,7,10,11,12,13,15,16,17,18],
            numOfHyps:3,
            stmtBnds:[0,5,9,10],
        }
        assertEq( patData, expectedRes )
        assertEq( frm.patSearch, Some(expectedRes) )

    })

    it("correctly calculates boundaries of statements for a frame with floating hypotheses only", _ => {
        //given
        let frm = makeFrame(
            [
                { typ: F, label: "1", expr: [0,1,2] },
                { typ: F, label: "2", expr: [8,9] },
            ],
            [16,17,18]
        )

        //when
        let patData = frmGetPatternSearchData(frm)

        //then
        let expectedRes = {
            allHypsAsrt:[16,17,18],
            numOfHyps:0,
            stmtBnds:[0],
        }
        assertEq( patData, expectedRes )
        assertEq( frm.patSearch, Some(expectedRes) )

    })

    it("correctly calculates boundaries of statements for a frame without hypotheses", _ => {
        //given
        let frm = makeFrame( [ ], [16,17,18] )

        //when
        let patData = frmGetPatternSearchData(frm)

        //then
        let expectedRes = {
            allHypsAsrt:[16,17,18],
            numOfHyps:0,
            stmtBnds:[0],
        }
        assertEq( patData, expectedRes )
        assertEq( frm.patSearch, Some(expectedRes) )

    })
})

describe("getLabelsReferencedBy", _ => {
    let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/referenced_by_depends_on._mm")
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
    let ctx = loadContext(ast)

    it("gets labels non-transitively", _ => {
        assertEq(
            ctx->getLabelsReferencedBy(
                ~rootLabels=["L9"], 
                ~transitive=false
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L1", "L7", "L8"]
        )
    })

    it("gets labels transitively", _ => {
        assertEq(
            ctx->getLabelsReferencedBy(
                ~rootLabels=["L9"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9"]
        )
        assertEq(
            ctx->getLabelsReferencedBy(
                ~rootLabels=["L6"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9"]
        )
        assertEq(
            ctx->getLabelsReferencedBy(
                ~rootLabels=["L7"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L2", "L3", "L5"]
        )
    })
})

describe("getLabelsDependingOn", _ => {
    let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/referenced_by_depends_on._mm")
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
    let ctx = loadContext(ast)
    let allFramesInDeclarationOrder = ctx->getAllFramesArr
        ->Array.toSorted(Expln_utils_common.comparatorByInt(frm => frm.ord))

    it("gets labels non-transitively", _ => {
        assertEq(
            getLabelsDependingOn(
                ~allFramesInDeclarationOrder,
                ~rootLabels=["L20"], 
                ~transitive=false
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L50"]
        )
        assertEq(
            getLabelsDependingOn(
                ~allFramesInDeclarationOrder,
                ~rootLabels=["L50"], 
                ~transitive=false
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L70", "L80"]
        )
    })

    it("gets labels transitively", _ => {
        assertEq(
            getLabelsDependingOn(
                ~allFramesInDeclarationOrder,
                ~rootLabels=["L20"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L50", "L70", "L80", "L90"]
        )
        assertEq(
            getLabelsDependingOn(
                ~allFramesInDeclarationOrder,
                ~rootLabels=["L70"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L90"]
        )
        assertEq(
            getLabelsDependingOn(
                ~allFramesInDeclarationOrder,
                ~rootLabels=["L60"], 
                ~transitive=true
            )->Belt_HashSetString.toArray->Array.toSorted(String.compare), 
            ["L80", "L90"]
        )
    })
})