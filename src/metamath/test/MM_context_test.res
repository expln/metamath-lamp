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

describe("addLocalConst", _ => {
    it("correctly adds one constant in an inner block", _ => {
        //given
        let ctx = createContext(())
        ctx->applySingleStmt(Const({symbols:["c1", "c2"]}))
        ctx->openChildContext

        //when
        ctx->addLocalConst("c3")

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

    it("correctly adds a few constants in an inner block", _ => {
        //given
        let ctx = createContext(())
        ctx->applySingleStmt(Const({symbols:["c1", "c2"]}))
        ctx->openChildContext

        //when
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")

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
        
        assertEq( ctx->ctxSymToIntExn("c4"), -4 )
        assertEq( ctx->ctxIntToSymExn(-4), "c4" )
        assertEq( ctx->isConst("c4"), true )
        assertEq( ctx->isVar("c4"), false )
    })
})

describe("closeChildContext", _ => {
    it("correctly transfers local constants to the parent context", _ => {
        //given
        let ctx = createContext(())
        ctx->applySingleStmt(Const({symbols:["c1", "c2"]}))
        ctx->openChildContext
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")

        //when
        ctx->closeChildContext

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
        
        assertEq( ctx->ctxSymToIntExn("c4"), -4 )
        assertEq( ctx->ctxIntToSymExn(-4), "c4" )
        assertEq( ctx->isConst("c4"), true )
        assertEq( ctx->isVar("c4"), false )
    })
})

describe("resetToParentContext", _ => {
    it("doesn't transfer local constants to the parent context", _ => {
        //given
        let ctx = createContext(())
        ctx->applySingleStmt(Const({symbols:["c1", "c2"]}))
        ctx->openChildContext
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")

        //when
        ctx->resetToParentContext

        //then
        assertEq( ctx->ctxSymToIntExn("c1"), -1 )
        assertEq( ctx->ctxIntToSymExn(-1), "c1" )
        assertEq( ctx->isConst("c1"), true )
        assertEq( ctx->isVar("c1"), false )

        assertEq( ctx->ctxSymToIntExn("c2"), -2 )
        assertEq( ctx->ctxIntToSymExn(-2), "c2" )
        assertEq( ctx->isConst("c2"), true )
        assertEq( ctx->isVar("c2"), false )
        
        assertEq( ctx->ctxSymToInt("c3")->Belt_Option.isNone, true )
        assertEq( ctx->ctxIntToSym(-3)->Belt_Option.isNone, true )
        assertEq( ctx->isConst("c3"), false )
        assertEq( ctx->isVar("c3"), false )
        
        assertEq( ctx->ctxSymToInt("c4")->Belt_Option.isNone, true )
        assertEq( ctx->ctxIntToSym(-4)->Belt_Option.isNone, true )
        assertEq( ctx->isConst("c4"), false )
        assertEq( ctx->isVar("c4"), false )
    })
})

describe("getLocalConsts", _ => {
    it("correctly returns local constants for an inner block", _ => {
        //given
        let ctx = createContext(())
        ctx->addLocalConst("c1")
        ctx->addLocalConst("c2")
        ctx->openChildContext
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")

        //when
        let actualLocalConsts = ctx->getLocalConsts

        //then
        assertEq( actualLocalConsts, ["c3", "c4"] )
    })

    it("correctly returns local constants for an the parent context when the child context is closed", _ => {
        //given
        let ctx = createContext(())
        ctx->addLocalConst("c1")
        ctx->addLocalConst("c2")
        ctx->openChildContext
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")
        ctx->closeChildContext

        //when
        let actualLocalConsts = ctx->getLocalConsts

        //then
        assertEq( actualLocalConsts, ["c1", "c2", "c3", "c4"] )
    })

    it("correctly returns local constants for an the parent context when the child context is reset to parent", _ => {
        //given
        let ctx = createContext(())
        ctx->addLocalConst("c1")
        ctx->addLocalConst("c2")
        ctx->openChildContext
        ctx->addLocalConst("c3")
        ctx->addLocalConst("c4")
        ctx->resetToParentContext

        //when
        let actualLocalConsts = ctx->getLocalConsts

        //then
        assertEq( actualLocalConsts, ["c1", "c2"] )
    })

    it("returns an empty array when there are no local constatns in an inner block", _ => {
        //given
        let ctx = createContext(())
        ctx->addLocalConst("c1")
        ctx->addLocalConst("c2")
        ctx->openChildContext

        //when
        let actualLocalConsts = ctx->getLocalConsts

        //then
        assertEq( actualLocalConsts, [] )
    })

    it("returns an empty array when there are no local constatns in the topmost block", _ => {
        //given
        let ctx = createContext(())

        //when
        let actualLocalConsts = ctx->getLocalConsts

        //then
        assertEq( actualLocalConsts, [] )
    })
})
