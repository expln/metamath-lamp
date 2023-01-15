open Expln_test
open MM_int_test_utils

let setMmPath = "C:/Users/Igor/igye/books/metamath/set.mm"

describe("MM_wrk_editor integration tests", _ => {
    it("proving reccot", _ => {
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

        assertProof(st, trgtStmtId, "proof1")
    })
    
})
