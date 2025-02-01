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
        ~stateLocStor=None,
        ~nextStmtId=0,
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

describe("editorStatesHaveSameContent", _ => {
    it("returns true for editors with different statement ids and proof statuses", _ => {
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = {
            ...st,
            stmts: [
                {...(st.stmts->Array.getUnsafe(0)), id:"1", proofStatus:Some(Ready)},
                {...(st.stmts->Array.getUnsafe(1)), id:"2", proofStatus:Some(Waiting)},
                {...(st.stmts->Array.getUnsafe(2)), id:"3", proofStatus:Some(NoJstf)},
            ]
        }
        let st2 = {
            ...st,
            stmts: [
                {...(st.stmts->Array.getUnsafe(0)), id:"4", proofStatus:Some(Waiting)},
                {...(st.stmts->Array.getUnsafe(1)), id:"5", proofStatus:Some(JstfIsIncorrect)},
                {...(st.stmts->Array.getUnsafe(2)), id:"6", proofStatus:None},
            ]
        }

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), true)
    })

    it("returns false for editors with different descr", _ => {
        //given
        let st1 = {...createEditorState(), descr:"1"}
        let st2 = {...st1, descr:"2"}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false for editors with different varsText", _ => {
        //given
        let st1 = {...createEditorState(), varsText:"1"}
        let st2 = {...st1, varsText:"2"}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false for editors with different disjText", _ => {
        //given
        let st1 = {...createEditorState(), disjText:"1"}
        let st2 = {...st1, disjText:"2"}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false for editors with different number of statements", _ => {
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = { ...st1, stmts: st1.stmts->Array.slice(~start=0, ~end=2) }

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in label", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, label:"A"}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in typ", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, typ:E}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in isGoal", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, isGoal:true}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in isBkm", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, isBkm:true}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in jstfText", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, jstfText:"A"}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })

    it("returns false when stmts.length is same but one statement differs in cont", _ => {
        //given
        //given
        let st = createEditorState()
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let st1 = st
        let st2 = {...st1, stmts:st1.stmts->Array.mapWithIndex((stmt,idx)=>idx==1?{...stmt, cont:"A"->strToCont}:stmt)}

        //when/then
        assertEq(editorStatesHaveSameContent(st1,st2), false)
    })
})