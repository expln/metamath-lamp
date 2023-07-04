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
        let Ok((st0Restored, _)) = st1->restoreEditorStateFromSnapshot(ht, ht->editorHistLength - 1)
        assertEditorState(st0Restored, "st0Restored")
    })
})