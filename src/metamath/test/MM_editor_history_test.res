open Expln_test
open MM_editor_history
open MM_wrk_editor
open MM_context
open MM_wrk_editor_json
open MM_wrk_pre_ctx_data

let createEditorState = ():editorState => {
    let settings:MM_wrk_settings.settings = {
        parens: "", descrRegexToDisc: "", labelRegexToDisc: "", descrRegexToDepr: "", labelRegexToDepr: "", 
        asrtsToSkip: [],
        discColor:"", deprColor:"", tranDeprColor:"",
        editStmtsByLeftClick:false, defaultStmtType:"", 
        unifMetavarPrefix:"&", sortDisjByType:"class wff", defaultStmtLabel:"", initStmtIsGoal:false, checkSyntax:false,
        stickGoalToBottom:false, autoMergeStmts:false, autoUnifyAll:false, typeSettings: [], 
        webSrcSettings: [], longClickEnabled:false, longClickDelayMs:0, hideContextSelector:false, 
        showVisByDefault:false, editorHistMaxLength:1000,
        allowedFrms: {
            inSyntax: {
                useDisc:true,
                useDepr:true,
                useTranDepr:true,
            },
            inEssen: {
                useDisc:true,
                useDepr:true,
                useTranDepr:true,
            },
        },
        useDefaultTransforms:false,
        useCustomTransforms:false,
        customTransforms:"",
        combCntMax:10000,
        bottomUpProverDefaults: {
            searchDepth: 4,
            lengthRestrict: MM_bottom_up_prover_params.lengthRestrictToStr(MM_bottom_up_prover_params.Less),
            allowNewDisjForExistingVars: true,
            allowNewStmts: true,
            allowNewVars: false,
            debugLevel: 0,
        },
    }
    createInitialEditorState(
        ~preCtxData=preCtxDataMake(~settings)->preCtxDataUpdate(~ctx=([],createContext(()))),
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