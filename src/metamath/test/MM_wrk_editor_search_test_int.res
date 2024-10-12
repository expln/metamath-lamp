open Expln_test
open MM_int_test_utils
open MM_int_test_editor_methods

module Ed = MM_int_test_editor_methods

describe("MM_wrk_editor integration tests: add new statements by search", _ => {
    it("add an assertion with hypotheses and disjoints", _ => {
        setTestDataDir("add-stmts-by-search-1")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="mathbox", ~debug)

        let (st, _) = st->addStmt(
            ~label="1", 
            ~stmt="|- ( A + ( B + C ) ) = D"
        )
        let (st, _) = st->addStmt(
            ~label="2", 
            ~stmt="|- D = ( ( B + C ) + A )"
        )
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch( ~filterLabel="lspfixed", ~chooseLabel="lspfixed", 
            ~addBefore=st->getStmtId(~label="2"))
        let st = st->unifyAll
        assertEditorState(st, "step2")
    })

})