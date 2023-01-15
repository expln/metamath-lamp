open Expln_test
open MM_int_test_utils

let setMmPath = "C:/Users/Igor/igye/books/metamath/set.mm"

describe("MM_wrk_editor integration tests", _ => {
    it("proving reccot", _ => {
        setTestDataDir("prove-reccot")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="reccsc", ())

        let (st, trgtStmt) = st->addStmt(
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
    })
    
})
