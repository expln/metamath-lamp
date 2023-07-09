open Expln_test
open MM_editor_history
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
    mm_editor_history__test_findDiff()
})

describe("applyDiff", _ => {
    mm_editor_history__test_applyDiff()
})