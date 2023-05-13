open Expln_test
open MM_parser
open MM_context
open MM_cmp_pe_frame_summary

describe("MM_cmp_pe_frame_summary.makeInitialState", _ => {

    it("creates correct symRename when an frm var does not exist in ctx", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/var_override1._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
        let frame = ctx->getFrameExn("ax1")
        let typeColors = Belt_HashMapString.fromArray([("term", "t-color"),("wff", "w-color")])

        //when
        let state = makeInitialState(~preCtx=ctx, ~frame, ~typeColors)

        
        //then
        assertEq(
            state.symRename->Belt_Option.getExn->Belt_HashMapString.toArray,
            [("term1","b")],
        )
        assertEq(
            state.symColors->Belt_HashMapString.toArray,
            [("a","w-color"),("term1","t-color")],
        )
        assertEq(
            state.eHyps->Js_array2.map(state.frmCtx->ctxIntsToStrExn)->Js.Array2.joinWith(" ; "),
            "( ) a term1",
        )
        assertEq(
            state.frmCtx->ctxIntsToStrExn(state.asrt),
            "( a term1 )",
        )
    })

    it("creates correct symRename when an frm var has different type in ctx", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/var_override2._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
        let frame = ctx->getFrameExn("ax1")
        let typeColors = Belt_HashMapString.fromArray([("term", "t-color"),("wff", "w-color")])

        //when
        let state = makeInitialState(~preCtx=ctx, ~frame, ~typeColors)

        
        //then
        assertEq(
            state.symRename->Belt_Option.getExn->Belt_HashMapString.toArray,
            [("term1","b")],
        )
        assertEq(
            state.symColors->Belt_HashMapString.toArray,
            [("a","w-color"),("term1","t-color")],
        )
        assertEq(
            state.eHyps->Js_array2.map(state.frmCtx->ctxIntsToStrExn)->Js.Array2.joinWith(" ; "),
            "( ) a a ; ( ) term1 term1",
        )
        assertEq(
            state.frmCtx->ctxIntsToStrExn(state.asrt),
            "( a term1 )",
        )
    })

    it("does not create symRename when an frm var exists in ctx and has same type", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/var_override3._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ())
        let frame = ctx->getFrameExn("ax1")
        let typeColors = Belt_HashMapString.fromArray([("term", "t-color"),("wff", "w-color")])

        //when
        let state = makeInitialState(~preCtx=ctx, ~frame, ~typeColors)

        
        //then
        assertEq(
            state.symRename->Belt_Option.isNone,
            true,
        )
        assertEq(
            state.symColors->Belt_HashMapString.toArray,
            [("a","w-color"),("b","w-color")],
        )
        assertEq(
            state.eHyps->Js_array2.map(state.frmCtx->ctxIntsToStrExn)->Js.Array2.joinWith(" ; "),
            "( ) a a a ; ( ) b b b",
        )
        assertEq(
            state.frmCtx->ctxIntsToStrExn(state.asrt),
            "( a b )",
        )
    })
})