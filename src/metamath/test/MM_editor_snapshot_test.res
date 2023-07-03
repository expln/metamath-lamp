open Expln_test
open MM_editor_snapshot
open MM_wrk_editor
open MM_context
open MM_parenCounter
open MM_wrk_editor_json

let createEditorState = ():editorState => {
    createInitialEditorState(
        ~settingsV=0, 
        ~settings={
            parens: "", asrtsToSkip: [], asrtsToSkipRegex: "", editStmtsByLeftClick:false, defaultStmtType:"", 
            defaultStmtLabel:"", initStmtIsGoal:false, checkSyntax:false, stickGoalToBottom:false, typeSettings: [], 
            webSrcSettings: [], longClickEnabled:false, longClickDelayMs:0, hideContextSelector:false, 
            showVisByDefault:false,
        }, 
        ~srcs=[],
        ~preCtxV=0, 
        ~preCtx=createContext(()), 
        ~stateLocStor=None
    )
}

describe("findDiff", _ => {
    mm_editor_snapshot__test_findDiff()
})

describe("applyDiff", _ => {
    mm_editor_snapshot__test_applyDiff()
})

describe("mergeDiff", _ => {
    mm_editor_snapshot__test_mergeDiff()
})

describe("editorHistory", _ => {
    it("is able to undo simple changes", _ => {
        let st0 = createEditorState()
        let ht = editorHistMake(~initState=st0, ~maxLength=200)
        let (st1,s1) = st0->addNewStmt
        let st1 = st1->completeContEditMode(s1, "s1-content")
        assertEq(st0 != st1, true)
        let ht = ht->editorHistAddSnapshot(st1)

        @warning("-8")
        let Some((st0RestoredFromSt1, _)) = st1->restoreEditorStateFromSnapshot(ht, ht->editorHistLength - 1)
    })
})