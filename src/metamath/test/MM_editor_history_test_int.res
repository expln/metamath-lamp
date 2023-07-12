open Expln_test
open MM_editor_history
open MM_wrk_editor
open MM_wrk_editor_json
open MM_context

open MM_int_test_utils
open MM_int_test_editor_methods

module Ed = MM_int_test_editor_methods

let createEmptyEditorState = ():editorState => {
    createInitialEditorState(
        ~settingsV=0, 
        ~settings={
            parens: "", asrtsToSkip: [], asrtsToSkipRegex: "", editStmtsByLeftClick:false, defaultStmtType:"", 
            defaultStmtLabel:"", initStmtIsGoal:false, checkSyntax:false, stickGoalToBottom:false, typeSettings: [], 
            webSrcSettings: [], longClickEnabled:false, longClickDelayMs:0, hideContextSelector:false, 
            showVisByDefault:false, editorHistMaxLength:1000,
        }, 
        ~srcs=[],
        ~preCtxV=0, 
        ~preCtx=createContext(()), 
        ~stateLocStor=None
    )
}

let findHistIdx = (ht:editorHistory, st:editorState):int => {
    let stLocStor = st->editorStateToEditorStateLocStor
    let found = ref(None)
    for i in 0 to ht->editorHistLength - 1 {
        if (found.contents->Belt_Option.isNone) {
            @warning("-8")
            let Ok(stRestored) = ht->editorHistGetSnapshotPreview(i, st)
            let stRestoredLocStor = stRestored->editorStateToEditorStateLocStor
            if (stLocStor == stRestoredLocStor) {
                found := Some(i)
            }
        }
    }
    found.contents->Belt_Option.getExn
}

let testStateRestore = (~ht:editorHistory, ~stateToRestore:editorState, ~expected:string):unit => {
    //given
    let baseState = createEmptyEditorState()

    //when
    @warning("-8")
    let Ok(stRestored) = baseState->restoreEditorStateFromSnapshot(ht, ht->findHistIdx(stateToRestore))

    //then
    assertEditorState(stRestored, expected)
}

describe("editorHistory", _ => {
    it("is able to undo changes", _ => {
        setTestDataDir("editor-history-restore-prev")

        let st = createEditorState(
            ~mmFilePath="./src/metamath/test/resources/demo0._mm", ~stopBefore="th1", ~debug, 
            ~editorState="editor-initial-state", ()
        )->updateEditorStateWithPostupdateActions(st=>st)
        let ht = editorHistMake(~initState=st, ~maxLength=200)
        let st0 = st
        assertEditorState(st0, "st0")

        @warning("-8")
        let Ok(st) = st->renameStmt(getStmtId(st, ~label="2", ()), "20")
            ->Belt.Result.map(updateEditorStateWithPostupdateActions(_, st=>st))
        let ht = ht->editorHistAddSnapshot(st)
        let st1 = st
        assertEditorState(st1, "st1")

        @warning("-8")
        let Ok(ht) = ht->editorHistToString->editorHistFromString
        testStateRestore(~ht, ~stateToRestore=st0, ~expected="st0Restored")
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

        let htStr = ht->editorHistToString
        assertStrEqFile(htStr, "hist18-locStor")

        let htRestored = switch htStr->editorHistFromString {
            | Error(msg) => failMsg(`Could not restore editor history from a string: ${msg}`)
            | Ok(ht) => ht
        }
        assertEditorHistory(htRestored, "hist18-restored")
        assertFileContentsEq("hist18", "hist18-restored")

        let st = st->completeContEditMode(goalStmtId, "|- t = t")->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        assertEditorHistory(ht, "hist19")
    })
})