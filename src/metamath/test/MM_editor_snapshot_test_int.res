open Expln_test
open MM_editor_snapshot
open MM_wrk_editor
open MM_context
open MM_parenCounter
open MM_wrk_editor_json

open MM_int_test_utils
open MM_int_test_editor_methods

module Ed = MM_int_test_editor_methods

describe("editorHistory", _ => {
    it("is able to undo changes", _ => {
        setTestDataDir("editor-history")

        let st0 = createEditorState(
            ~mmFilePath="./src/metamath/test/resources/demo0._mm", ~stopBefore="th1", ~debug, ()
        )->updateEditorStateWithPostupdateActions(st=>st)
        let ht = editorHistMake(~initState=st0, ~maxLength=200)
        assertEditorState(st0, "st0")

        let (st1,s1) = st0->addNewStmt
        let ht = ht->editorHistAddSnapshot(st1)

        let st1 = st1->completeContEditMode(s1, "|- t = t")
        let ht = ht->editorHistAddSnapshot(st1)
        assertEditorState(st1, "st1")

        @warning("-8")
        let Ok(st0Restored) = st1->restoreEditorStateFromSnapshot(ht, ht->editorHistLength - 1)
        assertEditorState(st0Restored, "st0Restored")
    })

    it("records all changes as expected", _ => {
        setTestDataDir("editor-history-merge-status")

        let st = createEditorState(
            ~mmFilePath="./src/metamath/test/resources/demo0._mm", ~stopBefore="th1", ~debug, ()
        )
        let ht = editorHistMake(~initState=st, ~maxLength=200)
        assertEditorHistory(ht, "hist1")

        let (st,goalStmtId) = st->addNewStmt
        let st = st->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st = st->completeContEditMode(goalStmtId, "|- t = t")->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist2")

        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist3")

        let st = st->addStmtsBySearch( ~filterLabel="a1", ~chooseLabel="a1", () )
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist4")

        let st = st->applySubstitution(~replaceWhat="term3 = term1", ~replaceWith="t = t")
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist5")

        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist6")

        let st = st->addStmtsBySearch( ~filterLabel="a2", ~chooseLabel="a2", () )
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist7")

        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist8")

        let st = st->toggleStmtChecked(st.stmts[0].id)
        let st = st->moveCheckedStmts(false)->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist9")

        let st = st->moveCheckedStmts(false)->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist10")

        let st = st->moveCheckedStmts(true)->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist11")

        let st = st->moveCheckedStmts(true)->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist12")

        let st = st->uncheckAllStmts
        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist13")

        let st = st->applySubstitution(~replaceWhat="term2 = t", ~replaceWith="( term1 + 0 ) = term1")
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist14")

        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist15")

        let st = st->applySubstitution(~replaceWhat="term1", ~replaceWith="t")
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist16")

        let st = st->toggleStmtChecked(st.stmts[st.stmts->Js.Array2.length-1].id)
        let (st,s2) = st->addNewStmt
        let st = st->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st = st->completeContEditMode(s2, "|- ( ( t + 0 ) = t -> t = t )")->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist17")

        let st = st->unifyAll
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist18")

    })
})