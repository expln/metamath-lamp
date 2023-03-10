open Expln_test
open MM_parser
open MM_context

describe("findParentheses", _ => {
    it("finds all parentheses", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0.mm")
        let (ast, _) = parseMmFile(mmFileText, ())
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
        let (ast, _) = parseMmFile(mmFileText, ())
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
        let (ast, _) = parseMmFile(mmFileText, ())
        let ctx = loadContext(ast, ())
        let constsToMove = "( ) [ ] { }"
        assertEq(ctx->ctxStrToIntsExn(constsToMove), [-5,-6,-12,-14,-13,-15])

        //when
        ctx->moveConstsToBegin("( ) [ t ] { } abc yyy")

        //then
        assertEq(ctx->ctxStrToIntsExn(constsToMove)->Js.Array2.sortInPlace, [-1,-2,-3,-4,-5,-6])
    })
})
