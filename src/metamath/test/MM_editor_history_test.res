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
            showVisByDefault:false, editorHistMaxLength:1000,
        }, 
        ~srcs=[],
        ~preCtxV=0, 
        ~preCtx=createContext(()), 
        ~stateLocStor=None
    )
}

describe("editorHistSetMaxLength", _ => {
    it("restricts history length", _ => {
        //given
        let st = ref(createEditorState())

        //when
        let ht = ref(editorHistMake(~initState=st.contents, ~maxLength=20))
        //then
        assertEq(ht.contents->editorHistLength, 0)

        //when
        for i in 1 to 18 {
            st := st.contents->completeDescrEditMode(i->Belt_Int.toString)
            ht := ht.contents->editorHistAddSnapshot(st.contents)
        }
        //then
        assertEq(ht.contents->editorHistLength, 18)

        //when
        for i in 1 to 5 {
            st := st.contents->completeDescrEditMode(i->Belt_Int.toString)
            ht := ht.contents->editorHistAddSnapshot(st.contents)
        }
        //then
        assertEq(ht.contents->editorHistLength, 20)

        //when
        ht := ht.contents->editorHistSetMaxLength(27)
        //then
        assertEq(ht.contents->editorHistLength, 20)

        //when
        for i in 1 to 10 {
            st := st.contents->completeDescrEditMode(i->Belt_Int.toString)
            ht := ht.contents->editorHistAddSnapshot(st.contents)
        }
        //then
        assertEq(ht.contents->editorHistLength, 27)

        //when
        ht := ht.contents->editorHistSetMaxLength(15)
        //then
        assertEq(ht.contents->editorHistLength, 15)

        //when
        for i in 1 to 10 {
            st := st.contents->completeDescrEditMode(i->Belt_Int.toString)
            ht := ht.contents->editorHistAddSnapshot(st.contents)
        }
        //then
        assertEq(ht.contents->editorHistLength, 15)
    })
})

describe("findDiff", _ => {
    mm_editor_history__test_findDiff()
})

describe("applyDiff", _ => {
    mm_editor_history__test_applyDiff()
})