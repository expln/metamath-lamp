open Expln_test
open MM_int_test_utils

describe("MM_wrk_editor integration tests", _ => {
    it("prove reccot", _ => {
        setTestDataDir("prove-reccot")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="reccsc", ())

        let (st, trgtStmtId) = st->addStmt(
            ~label="reccot", 
            ~stmt="|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( 1 / ( cot ` A ) ) )",
            ()
        )
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch( ~filterLabel="tanval", ~chooseLabel="tanval", () )
        assertEditorState(st, "step2")

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let tanvalId = st->getStmtId(~contains="-> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) )")
        assertEditorState(st, "step3")

        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->addStmtsBySearch( ~filterLabel="cotval", ~chooseLabel="cotval", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let cotvalId = st->getStmtId(~contains="-> ( cot ` A ) = ( ( cos ` A ) / ( sin ` A ) )")
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let st = st->addStmtsBySearch(~filterPattern="class =/= 0 /\\ class =/= 0 -> 1 /", ~chooseLabel="recdiv", () )
        let st = st->unifyAll
        assertEditorState(st, "step6")

        let (st, tanvalCopy1Id) = st->duplicateStmt(tanvalId)
        let st = st->updateStmt(tanvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step7")

        let (st, cotvalCopy1Id) = st->duplicateStmt(cotvalId)
        let st = st->updateStmt(cotvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step8")

        let (st, cotvalCopy2Id) = st->duplicateStmt(cotvalCopy1Id)
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( cot ` A )",
            ~contReplaceWith="( 1 / ( cot ` A ) )",
            ()
        )
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( ( cos ` A ) / ( sin ` A ) )",
            ~contReplaceWith="( 1 / ( ( cos ` A ) / ( sin ` A ) ) )",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step9")

        let st = st->applySubstitution(
            ~replaceWhat="1 / ( class1 / class2 )", 
            ~replaceWith="1 / ( ( cos ` A ) / ( sin ` A ) )"
        )
        let st = st->unifyAll
        assertEditorState(st, "step10")

        let st = st->MM_wrk_editor.uncheckAllStmts
        let st = st->MM_wrk_editor.toggleStmtChecked(st.stmts[0].id)
        let (st, _) = st->addStmt(
            ~stmt="|- ( A e. CC -> ( cos ` A ) e. CC )",
            ()
        )
        let st = st->MM_wrk_editor.uncheckAllStmts
        let st = st->unifyAll
        assertEditorState(st, "step11")

        let st = st->addStmtsBySearch(~filterLabel="sincl", ~chooseLabel="sincl", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let st = st->unifyAll
        assertEditorState(st, "step12")

        let result1Id = st->getStmtId(~contains="1 / ( ( cos ` A ) / ( sin ` A ) ) ) = ( ( sin ` A ) / ( cos ` A ) ) )")
        let (st, result2Id) = st->duplicateStmt(result1Id)
        let st = st->updateStmt(result2Id, 
            ~contReplaceWhat="( cos ` A ) e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( cos ` A ) =/= 0",
            ()
        )

        let (st, result3Id) = st->duplicateStmt(result2Id)
        let st = st->updateStmt(result3Id, 
            ~contReplaceWhat="( sin ` A ) e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step13")

        let (st, result4Id) = st->duplicateStmt(result3Id)
        let st = st->updateStmt(result4Id, 
            ~contReplaceWhat="( A e. CC /\\ ( cos ` A ) =/= 0 ) /\\ ( A e. CC /\\ ( sin ` A ) =/= 0 )",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step14")
        assertProof(st, trgtStmtId, "proof1-no-hyps")

        let hyp1Id = st.stmts[0].id
        let hyp2Id = st.stmts[1].id

        let st = st->updateStmt(hyp1Id, ~typ=E, () )
        let st = st->updateStmt(hyp2Id, ~typ=E, () )
        let st = st->unifyAll
        assertEditorState(st, "step15")
        assertProof(st, trgtStmtId, "proof2-hyps")
        
        let st = st->updateStmt(hyp1Id, ~typ=P, () )
        let st = st->updateStmt(hyp2Id, ~typ=P, () )
        let st = {...st, stmts: st.stmts->Js.Array2.map(stmt => {...stmt, jstfText:""})}
        let st = st->MM_wrk_editor.updateEditorStateWithPostupdateActions(st => st)
        assertEditorState(st, "step16-no-jstf-before-unify-all")

        let st = st->unifyAll
        assertEditorState(st, "step17-after-unify-all")
        assertProof(st, trgtStmtId, "proof3-no-hyps")
    })

    it("prove nfv", _ => {
        setTestDataDir("prove-nfv")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="nfv", ())

        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( ph -> A. x ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> A. x ph )", () )
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- F/ x ph", () )
        let st = st->unifyAll
        assertEditorState(st, "step1")
        
        let st = st->MM_wrk_editor.completeDisjEditMode("x,ph")
        let st = st->MM_wrk_editor.updateEditorStateWithPostupdateActions(st => st)
        let st = st->unifyAll
        assertEditorState(st, "step2")
        assertProof(st, trgtStmtId, "proof1")

    })

    it("filtering bottom-up proof results in two ways for xmulasslem", _ => {
        setTestDataDir("bottom-up-xmulasslem")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="xmulasslem", ())
        let resultsWhenLabelParamIsNotSpecified = stmts1->arrNewStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "xmulasslem")

        let (st, stmts2) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="xmulasslem", ~chooseLabel="xmulasslem", ())
        let resultsWhenLabelParamIsSpecified = stmts2->arrNewStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof results in two ways for fvmpt", _ => {
        setTestDataDir("bottom-up-fvmpt")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="fvmpt", ())
        let resultsWhenLabelParamIsNotSpecified = stmts1->arrNewStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "fvmpt")

        let (st, stmts2) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="fvmpt", ~chooseLabel="fvmpt", ())
        let resultsWhenLabelParamIsSpecified = stmts2->arrNewStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof has same results for search depth 3 and 4", _ => {
        setTestDataDir("bottom-up-ifbieq2d")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ())

        let st = st->MM_wrk_editor.completeDisjEditMode("x,A")
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( x = A -> if ( x = 0 , 0 , if ( x < 0 , -u 1 , 1 ) ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=3, ~allowNewVars=false, ~chooseLabel="ifbieq2d", ())
        let resultsWhenDepth3 = stmts1->arrNewStmtsDtoToStr

        assertTextEqFile(resultsWhenDepth3, "ifbieq2d")

        let (st, stmts2) = st->unifyBottomUp(trgtStmtId, ~maxSearchDepth=4, ~allowNewVars=false, ~chooseLabel="ifbieq2d", ())
        let resultsWhenDepth4 = stmts2->arrNewStmtsDtoToStr

        assertTextsEq(
            resultsWhenDepth3, "resultsWhenDepth3", 
            resultsWhenDepth4, "resultsWhenDepth4"
        )
    })

    it("prove sgnval", _ => {
        setTestDataDir("prove-sgnval")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )
        let (st, stmts) = st->unifyBottomUp(trgtStmtId, 
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, ~chooseLabel="fvmpt", ())
        assertEq(stmts->Js.Array2.length, 1)
        let st = st->MM_wrk_editor.addNewStatements(stmts[0])
        let st = st->MM_wrk_editor.uncheckAllStmts
        let st = st->MM_wrk_editor.updateEditorStateWithPostupdateActions(st => st)
        let st = st->unifyAll
        assertEditorState(st, "step1")

    })
    
})
