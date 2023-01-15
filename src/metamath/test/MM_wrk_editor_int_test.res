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
        assertEditorState(st, "step3")

        let st = st->unifyAll
        assertEditorState(st, "step4")
    })
    
})
