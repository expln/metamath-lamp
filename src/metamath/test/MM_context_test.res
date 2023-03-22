open Expln_test
open MM_parser
open MM_context

describe("findParentheses", _ => {
    it("finds all parentheses", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0.mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())

        //when
        let actualFoundParens = findParentheses(ctx, ())

        //then
        assertEq(
            actualFoundParens->Js_array2.map(ctxIntToSymExn(ctx, _)),
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
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0.mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
        let constsToMove = "( ) [ ] { }"
        assertEq(ctx->ctxStrToIntsExn(constsToMove), [-5,-6,-12,-14,-13,-15])

        //when
        ctx->moveConstsToBegin(constsToMove)

        //then
        assertEq(ctx->ctxStrToIntsExn(constsToMove)->Js.Array2.sortInPlace, [-1,-2,-3,-4,-5,-6])
    })

    it("doesn't fail if variables or unrecognized symbols are provided", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0.mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
        let constsToMove = "( ) [ ] { }"
        assertEq(ctx->ctxStrToIntsExn(constsToMove), [-5,-6,-12,-14,-13,-15])

        //when
        ctx->moveConstsToBegin("( ) [ t ] { } abc yyy")

        //then
        assertEq(ctx->ctxStrToIntsExn(constsToMove)->Js.Array2.sortInPlace, [-1,-2,-3,-4,-5,-6])
    })

    it("doesn't break var types", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0-moveConstsToBegin-test.mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
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
        ctx->moveConstsToBegin("( ) [ ] { }")

        //then
        assertEq(ctx->getTypeOfVarExn(t)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(r)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(s)->ctxIntToSymExn(ctx, _), "term")
        assertEq(ctx->getTypeOfVarExn(p)->ctxIntToSymExn(ctx, _), "wff")
        assertEq(ctx->getTypeOfVarExn(q)->ctxIntToSymExn(ctx, _), "wff")
    })

    it("doesn't break expr-to-hyp", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0-moveConstsToBegin-test.mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ~stopBefore="mp", ())
        assertEq(
            (ctx->ctxStrToIntsExn("|- ( P -> Q )")->getHypByExpr(ctx, _)->Belt_Option.getExn).label,
            "maj"
        )

        //when
        ctx->moveConstsToBegin("( ) [ ] { }")

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
        disj->disjForEachArr(arr => actual->Js_array2.push(arr)->ignore)

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
