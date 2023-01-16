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
        assertEditorState(st, "step1", ~failOnMismatch, ())

        let st = st->addStmtsBySearch( ~filterLabel="tanval", ~chooseLabel="tanval", () )
        assertEditorState(st, "step2", ~failOnMismatch, ())

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let tanvalId = st->getStmtId(~contains="-> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) )")
        assertEditorState(st, "step3", ~failOnMismatch, ())

        let st = st->unifyAll
        assertEditorState(st, "step4", ~failOnMismatch, ())

        let st = st->addStmtsBySearch( ~filterLabel="cotval", ~chooseLabel="cotval", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let cotvalId = st->getStmtId(~contains="-> ( cot ` A ) = ( ( cos ` A ) / ( sin ` A ) )")
        let st = st->unifyAll
        assertEditorState(st, "step5", ~failOnMismatch, ())

        let st = st->addStmtsBySearch(~filterPattern="class =/= 0 /\\ class =/= 0 -> 1 /", ~chooseLabel="recdiv", () )
        let st = st->unifyAll
        assertEditorState(st, "step6", ~failOnMismatch, ())

        let (st, tanvalCopy1Id) = st->duplicateStmt(tanvalId)
        let st = st->updateStmt(tanvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step7", ~failOnMismatch, ())

        let (st, cotvalCopy1Id) = st->duplicateStmt(cotvalId)
        let st = st->updateStmt(cotvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step8", ~failOnMismatch, ())

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
        assertEditorState(st, "step9", ~failOnMismatch, ())

        let st = st->applySubstitution(
            ~replaceWhat="1 / ( class1 / class2 )", 
            ~replaceWith="1 / ( ( cos ` A ) / ( sin ` A ) )"
        )
        let st = st->unifyAll
        assertEditorState(st, "step10", ~failOnMismatch, ())

        let st = st->MM_wrk_editor.uncheckAllStmts
        let st = st->MM_wrk_editor.toggleStmtChecked(st.stmts[0].id)
        let (st, _) = st->addStmt(
            ~stmt="|- ( A e. CC -> ( cos ` A ) e. CC )",
            ()
        )
        let st = st->MM_wrk_editor.uncheckAllStmts
        let st = st->unifyAll
        assertEditorState(st, "step11", ~failOnMismatch, ())

        let st = st->addStmtsBySearch(~filterLabel="sincl", ~chooseLabel="sincl", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let st = st->unifyAll
        assertEditorState(st, "step12", ~failOnMismatch, ())

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
        assertEditorState(st, "step13", ~failOnMismatch, ())

        let (st, result4Id) = st->duplicateStmt(result3Id)
        let st = st->updateStmt(result4Id, 
            ~contReplaceWhat="( A e. CC /\\ ( cos ` A ) =/= 0 ) /\\ ( A e. CC /\\ ( sin ` A ) =/= 0 )",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step14", ~failOnMismatch, ())
        assertProof(st, trgtStmtId, "proof1-no-hyps", ~failOnMismatch, ())

        let hyp1Id = st.stmts[0].id
        let hyp2Id = st.stmts[1].id

        let st = st->updateStmt(hyp1Id, ~typ=#e, () )
        let st = st->updateStmt(hyp2Id, ~typ=#e, () )
        let st = st->unifyAll
        assertEditorState(st, "step15", ~failOnMismatch, ())
        assertProof(st, trgtStmtId, "proof2-hyps", ~failOnMismatch, ())
        
        let st = st->updateStmt(hyp1Id, ~typ=#p, () )
        let st = st->updateStmt(hyp2Id, ~typ=#p, () )
        let st = {...st, stmts: st.stmts->Js.Array2.map(stmt => {...stmt, jstfText:""})}
        let st = st->MM_wrk_editor.updateEditorStateWithPostupdateActions(st => st)
        assertEditorState(st, "step16-no-jstf-before-unify-all", ~failOnMismatch, ())

        let st = st->unifyAll
        assertEditorState(st, "step17-after-unify-all", ~failOnMismatch, ())
        assertProof(st, trgtStmtId, "proof3-no-hyps", ~failOnMismatch, ())
    })

    it("prove nfv", _ => {
        setTestDataDir("prove-nfv")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="nfv", ())

        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( ph -> A. x ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> A. x ph )", () )
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- F/ x ph", () )
        let st = st->unifyAll
        assertEditorState(st, "step1", ~failOnMismatch, ())
        
        let st = st->MM_wrk_editor.completeDisjEditMode("x,ph")
        let st = st->MM_wrk_editor.updateEditorStateWithPostupdateActions(st => st)
        let st = st->unifyAll
        assertEditorState(st, "step2", ~failOnMismatch, ())
        assertProof(st, trgtStmtId, "proof1", ~failOnMismatch, ())

    })

    it("prove sgnval", _ => {
        setTestDataDir("prove-sgnval")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )
        let st = st->unifyBottomUp(trgtStmtId)

    })
    
})
