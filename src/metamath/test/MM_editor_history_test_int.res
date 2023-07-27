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
            unifMetavarPrefix:"&", defaultStmtLabel:"", initStmtIsGoal:false, checkSyntax:false, stickGoalToBottom:false, typeSettings: [], 
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
    let expectedFileName = expected ++ "-restored"
    assertEditorState(stRestored, expectedFileName)
    assertFileContentsEq(expected, expectedFileName)
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

        let st = st->completeVarsEditMode(".vvv term tmp-var")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st2 = st
        assertEditorState(st2, "st2")

        let st = st->completeDisjEditMode("tmp-var,t")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st3 = st
        assertEditorState(st3, "st3")

        let st = st->completeTypEditMode(getStmtId(st, ~label="20", ()), E, false)
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st4 = st
        assertEditorState(st4, "st4")

        let st = st->completeJstfEditMode(getStmtId(st, ~label="1", ()), ": a2")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st5 = st
        assertEditorState(st5, "st5")

        let st = st->completeContEditMode(getStmtId(st, ~label="3", ()), "|- ( ( r + 0 ) = r -> r = r )")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st6 = st
        assertEditorState(st6, "st6")

        let (st,s1) = st->addNewStmt
        let st = st->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st = st->completeContEditMode(s1, "|- r = r")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st7 = st
        assertEditorState(st7, "st7")

        let st = st->toggleStmtChecked(getStmtId(st, ~label="2", ()))
        let st = st->moveCheckedStmts(true)->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st8 = st
        assertEditorState(st8, "st8")

        let st = st->uncheckAllStmts
        let st = st->toggleStmtChecked(getStmtId(st, ~label="3", ()))
        let st = st->deleteCheckedStmts->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st = st->completeJstfEditMode(getStmtId(st, ~label="qed", ()), "")
            ->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)
        let st9 = st
        assertEditorState(st9, "st9")

        let st = st->completeDescrEditMode("sdfh dfert vzxaw")->updateEditorStateWithPostupdateActions(st=>st)
        let ht = ht->editorHistAddSnapshot(st)

        @warning("-8")
        let Ok(ht) = ht->editorHistToString->editorHistFromString
        testStateRestore(~ht, ~stateToRestore=st0, ~expected="st0")
        testStateRestore(~ht, ~stateToRestore=st1, ~expected="st1")
        testStateRestore(~ht, ~stateToRestore=st2, ~expected="st2")
        testStateRestore(~ht, ~stateToRestore=st3, ~expected="st3")
        testStateRestore(~ht, ~stateToRestore=st4, ~expected="st4")
        testStateRestore(~ht, ~stateToRestore=st5, ~expected="st5")
        testStateRestore(~ht, ~stateToRestore=st6, ~expected="st6")
        testStateRestore(~ht, ~stateToRestore=st7, ~expected="st7")
        testStateRestore(~ht, ~stateToRestore=st8, ~expected="st8")
        testStateRestore(~ht, ~stateToRestore=st9, ~expected="st9")
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