open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_editor_substitution
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open Local_storage_utils
open Common
open MM_wrk_pre_ctx_data
open MM_editor_history
open MM_proof_tree_dto
open MM_bottom_up_prover_params

type frameProofData = MM_cmp_pe_frame_full.frameProofData

let editorStateLocStorKey = "editor-state"
let editorHistLocStorKey = "editor-hist"
let editorHistRegLocStorKey = "hist-reg"
let editorHistTmpLocStorKey = "hist-tmp"
let lastUsedAsrtSearchTypLocStorKey = "search-asrt-typ"

let getEditorLocStorKey = (editorId:int):string => {
    editorStateLocStorKey ++ "-" ++ Int.toString(editorId)
}

let getEditorHistLocStorKey = (editorId:int):string => {
    editorHistLocStorKey ++ "-" ++ Int.toString(editorId)
}

let editorSaveStateToLocStor = (state:editorState, ~editorId:int):unit => {
    // Console.log("------------- Editor ids info --------------------")
    // Console.log2("nextStmtId", state.nextStmtId)
    // let allIds = state.stmts
    //     ->Array.map(({id}) => Int.fromString(id)->Option.getExn)
    //     ->Array.toSorted(Int.compare)
    //     ->Array.map(Int.toString(_))
    //     ->Array.join(", ")
    // Console.log(allIds)
    // Console.log("--------------------------------------------------")
    locStorWriteString(getEditorLocStorKey(editorId), Expln_utils_common.stringify(state->editorStateToEditorStateLocStor))
}

let histSaveToLocStor = (hist:editorHistory, ~editorId:int):unit => {
    let histStr = hist->editorHistToString
    let locStorKey = getEditorHistLocStorKey(editorId)
    let histChanged = switch locStorReadString(locStorKey) {
        | None => true
        | Some(oldHistStr) => oldHistStr != histStr
    }
    if (histChanged) {
        locStorWriteString( locStorKey, histStr )
        // hist->editorHistoryPrintIdInfo
    }
}

let histReadFromLocStor = (~editorId:int):option<editorHistory> => {
    switch locStorReadString(getEditorHistLocStorKey(editorId)) {
        | None => None
        | Some(histStr) => {
            switch editorHistFromString(histStr) {
                | Error(_) => None
                | Ok(hist) => Some(hist)
            }
        }
    }
}

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool, 
    ~ref:option<ReactDOM.domRef>=?,
    ~title:option<string>=?, 
    ~smallBtns:bool=false
) => {
    <span ?ref ?title>
        <IconButton 
            disabled={!active} 
            onClick={_ => onClick()}
            color="primary"
            style=?{
                if (smallBtns) {Some(ReactDOM.Style.make(~padding="2px", ()))} else {None}
            }
        > 
            icon 
        </IconButton>
    </span>
}

let saveLastUsedTyp = (ctx:mmContext,typInt:int):unit => {
    switch ctx->ctxIntToSym(typInt) {
        | None => ()
        | Some(typStr) =>
            Dom_storage2.localStorage->Dom_storage2.setItem(lastUsedAsrtSearchTypLocStorKey, typStr)
    }
}

let getLastUsedTyp = (ctx) => {
    switch Dom_storage2.localStorage->Dom_storage2.getItem(lastUsedAsrtSearchTypLocStorKey) {
        | None => None
        | Some(typStr) => ctx->ctxSymToInt(typStr)
    }
}

let jsonStrOptToEditorStateLocStor = (jsonStrOpt:option<string>):option<editorStateLocStor> => {
    jsonStrOpt->Belt_Option.flatMap(jsonStr => {
        readEditorStateFromJsonStr(jsonStr)->Belt.Result.mapWithDefault(None, state => Some(state))
    })
}

let loadEditorInitialStateFromLocStor = (
    ~editorId:int,
    ~preCtxData:preCtxData,
    ~initialStateLocStor:option<editorStateLocStor>,
):editorState => {
    switch histReadFromLocStor(~editorId) {
        | None => createInitialEditorState( ~preCtxData, ~stateLocStor=initialStateLocStor, ~nextStmtId=0, )
        | Some(hist) => {
            let nextStmtId = hist->editorHistGetMaxIntStmtId->Option.map(i => i+1)->Option.getOr(0)
            let stateFromLocStor = createInitialEditorState( ~preCtxData, ~stateLocStor=initialStateLocStor, ~nextStmtId, )
            switch stateFromLocStor->restoreEditorStateFromSnapshot(hist, -1) {
                | Error(_) => stateFromLocStor
                | Ok(stateFromHistory) => {
                    if (editorStatesHaveSameContent(stateFromLocStor, stateFromHistory)) {
                        stateFromHistory
                    } else {
                        stateFromLocStor
                    }
                }
            }
        }
    }
}

let previousEditingIsNotCompletedTitle = "Previous editing is not completed"
let previousEditingIsNotCompletedText = 
    `You've attempted to edit something in the editor while the previous edit is not complete.`
        ++ ` Please complete the previous edit before starting a new one.`

let infoAboutGettingCompletedProof = `In order to show a completed proof do the following: ` 
                ++ `1) Make sure the step you want to show a completed proof for is marked with a green chekmark. ` 
                ++ `If it is not, try to "unify all"; 2) Select the step you want to show a completed proof for; ` 
                ++ `3) Select "Show completed proof" menu item.`

let infoAboutInliningTheorems = `In order to inline theorems do the following: ` 
                ++ `1) Make sure all steps you want to inline the proof for are marked with a green check mark. `
                ++ `If they are not, try to "unify all"; 2) Select "Inline theorems" menu item.`

@react.component
let make = (
    ~modalRef:modalRef, 
    ~editorId:int,
    ~preCtxData:preCtxData,
    ~top:int,
    ~reloadCtx: React.ref<option<MM_cmp_context_selector.reloadCtxFunc>>,
    ~addAsrtByLabel: ref<option<string=>promise<result<unit,string>>>>,
    ~updateTabTitle: ref<option<string=>unit>>,
    ~initialStateLocStor:option<editorStateLocStor>,
    ~toggleCtxSelector:React.ref<Nullable.t<unit=>unit>>,
    ~ctxSelectorIsExpanded:bool,
    ~showTabs:bool,
    ~setShowTabs:bool=>unit,
    ~openFrameExplorer:string=>unit,
    ~onTabTitleChange:string=>unit,
) => {
    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Nullable.null)
    let (contIsHidden, setContIsHidden) = React.useState(_ => false)
    let (showBkmOnly, setShowBkmOnly) = React.useState(_ => false)
    let showBkmOnlyRef:React.ref<bool> = React.useRef(showBkmOnly)
    showBkmOnlyRef.current = showBkmOnly
    let onOpenSubstitutionDialogRef:React.ref<unit=>unit> = React.useRef(()=>())

    let (showCheckbox, setShowCheckbox) = useStateFromLocalStorageBool(
        ~key="editor-showCheckbox", ~default=true,
    )
    let (showLabel, setShowLabel) = useStateFromLocalStorageBool(
        ~key="editor-showLabel", ~default=true,
    )
    let (showType, setShowType) = useStateFromLocalStorageBool(
        ~key="editor-showType", ~default=true,
    )
    let (showJstf, setShowJstf) = useStateFromLocalStorageBool(
        ~key="editor-showJstf", ~default=true,
    )
    let (inlineMode, setInlineMode) = useStateFromLocalStorageBool(
        ~key="editor-inlineMode", ~default=false,
    )
    let (smallBtns, setSmallBtns) = useStateFromLocalStorageBool(
        ~key="editor-smallBtns", ~default=false,
    )
    let (stepsPerPage, setStepsPerPage) = useStateFromLocalStorageInt(
        ~key="editor-steps-per-page", ~default=100,
    )
    let (parenAc, setParenAc) = useStateFromLocalStorageBool(
        ~key="paren-autocomplete", ~default=true,
    )

    let (state, setStatePriv) = React.useState(_ => {
        loadEditorInitialStateFromLocStor( ~editorId, ~preCtxData, ~initialStateLocStor, )
    })
    let (hist, setHistPriv) = React.useState(() => {
        switch histReadFromLocStor(~editorId) {
            | None => editorHistMake(~initState=state, ~maxLength=preCtxData.settingsV.val.editorHistMaxLength)
            | Some(hist) => hist->editorHistAddSnapshot(state)
        }
    })

    let stmtsToShow =
        if (showBkmOnly) {
            state.stmts->Array.filter(stmt => {
                stmt.typ == E || stmt.isGoal || stmt.isBkm
                || stmt->userStmtHasAnyErrors
                || stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode
            })
        } else {
            state.stmts
        }
    let maxStepsPerPage = 300
    let stepsPerPage = Math.Int.max(1, Math.Int.min(stepsPerPage, maxStepsPerPage))
    let numOfPages = (stmtsToShow->Array.length->Belt_Int.toFloat /. stepsPerPage->Belt.Int.toFloat)
                        ->Math.ceil->Belt.Float.toInt
    let minPageIdx = 0
    let maxPageIdx = numOfPages - 1
    let (pageIdx, setPageIdx) = React.useState(() => 0)
    let pageIdx = Math.Int.max(minPageIdx, Math.Int.min(pageIdx, maxPageIdx))
    let stmtBeginIdx = pageIdx * stepsPerPage
    let stmtEndIdx = stmtBeginIdx + stepsPerPage - 1

    let actSetStepsPerPage = (newStepsPerPage) => {
        if (1 <= newStepsPerPage && newStepsPerPage <= maxStepsPerPage) {
            setStepsPerPage(_ => newStepsPerPage)
        }
    }

    let actGoToPage = (pageIdx) => {
        setPageIdx(_ => pageIdx)
    }

    let actResetPageIdx = () => {
        actGoToPage(0)
    }

    let setHist = (update:editorHistory=>editorHistory):unit => {
        setHistPriv(ht => {
            let ht = update(ht)
            histSaveToLocStor(ht, ~editorId)
            ht
        })
    }

    let scheduleUnifyAllIfAllowed = (st:editorState):editorState => {
        if (st.preCtxData.settingsV.val.autoUnifyAll) {
            switch st.nextAction {
                | Some(_) => st
                | None => {
                    let editIsActive = st->isEditMode
                    let thereAreCriticalErrorsInEditor = st->editorStateHasCriticalErrors
                    let atLeastOneStmtIsChecked = st.checkedStmtIds->Array.length != 0
                    let proofStatusIsMissing = st.stmts->Array.some(stmt => {
                        stmt.typ == P && stmt.proofStatus->Belt_Option.isNone
                    })
                    if (
                        !editIsActive && !thereAreCriticalErrorsInEditor 
                        && !atLeastOneStmtIsChecked && proofStatusIsMissing
                    ) {
                        st->setNextAction(Some(UnifyAll({nextAction:()=>()})))
                    } else {
                        st
                    }
                }
            }
        } else {
            st
        }
    }

    let saveStateToLocStorAndMakeHistSnapshot = (st:editorState):editorState => {
        editorSaveStateToLocStor(st, ~editorId)
        setHist(ht => ht->editorHistAddSnapshot(st))
        st
    }

    let setState = (update:editorState=>editorState) => {
        setStatePriv(st => {
            let st = st->update
            let st = st->verifyEditorState
            let st = st->saveStateToLocStorAndMakeHistSnapshot
            let st = st->scheduleUnifyAllIfAllowed
            st
        })
    }

    let actUnifyAllResultsAreReady = (proofTreeDto:result<proofTreeDto,string>, nextAction: unit=>unit) => {
        switch proofTreeDto {
            | Ok(proofTreeDto) => {
                setStatePriv(st => {
                    let st = st->applyUnifyAllResults(proofTreeDto)
                    let st = st->saveStateToLocStorAndMakeHistSnapshot
                    let st = st->setNextAction(Some(Action(nextAction)))
                    st
                })
            }
            | Error(msg) => {
                openInfoDialog(
                    ~modalRef, 
                    ~title="Error in UnifyAll",
                    ~content={
                        <Col>
                            <pre style=ReactDOM.Style.make(~color="red", ())> {React.string("Error:")} </pre>
                            <pre> {React.string(msg)} </pre>
                        </Col>
                    }, 
                    ~onOk=()=>{
                        setStatePriv(st => {
                            let st = st->setNextAction(Some(Action(nextAction)))
                            st
                        })
                    }, 
                )
            }
        }
    }

    let showInfoMsg = (~title:option<string>=?, ~text:string) => {
        openInfoDialog( ~modalRef, ~title?, ~text )
    }
    
    let showErrMsg = (~title:option<string>=?, ~text:string) => {
        openInfoDialog( ~modalRef, ~title?, ~text,
            ~icon=
                <span style=ReactDOM.Style.make(~color="red", () ) >
                    <MM_Icons.PriorityHigh/>
                </span>
        )
    }

    let editIsActive = state->isEditMode
    let thereAreCriticalErrorsInEditor = editorStateHasCriticalErrors(state)
    let thereAreAnyErrorsInEditor = editorStateHasAnyErrors(state)
    let atLeastOneStmtIsChecked = state.checkedStmtIds->Array.length != 0
    let atLeastOneStmtHasSelectedText = state.stmts
        ->Array.find(stmt => stmt.cont->hasSelectedText)
        ->Belt.Option.isSome
    let checkedStmtIds = state.checkedStmtIds->Array.map(((stmtId,_)) => stmtId)
    let allCheckedStmtsAreBookmarked = state.stmts->Array.every(stmt => {
        !(checkedStmtIds->Array.includes(stmt.id)) || stmt.isBkm
    })
    let allCheckedStmtsAreUnbookmarked = state.stmts->Array.every(stmt => {
        !(checkedStmtIds->Array.includes(stmt.id)) || !stmt.isBkm
    })
    let mainCheckboxState = {
        let atLeastOneStmtIsNotChecked = state.stmts->Array.length != state.checkedStmtIds->Array.length
        if ((atLeastOneStmtIsChecked || atLeastOneStmtHasSelectedText) && atLeastOneStmtIsNotChecked) {
            None
        } else if (atLeastOneStmtIsChecked && !atLeastOneStmtIsNotChecked) {
            Some(true)
        } else {
            Some(false)
        }
    }

    let generalModificationActionIsEnabled = !editIsActive && !thereAreCriticalErrorsInEditor
    let numOfCheckedStmts = state.checkedStmtIds->Array.length
    let thereIsDuplicatedStmt = state->editorStateHasDuplicatedStmts

    let actPreCtxDataUpdated = () => {
        setState(st => {
            let st = st->setPreCtxData(preCtxData)
            let st = st->setNextAction(Some(Action(()=>()))) //this prevents automatic unification
            st
        })
        setHist(editorHistSetMaxLength(_, preCtxData.settingsV.val.editorHistMaxLength))
    }

    React.useEffect1(() => {
        actPreCtxDataUpdated()
        None
    }, [preCtxData.settingsV.ver, preCtxData.ctxV.ver])

    let actOpenMainMenu = () => {
        setMainMenuIsOpened(_ => true)
    }

    let actCloseMainMenu = () => {
        setMainMenuIsOpened(_ => false)
    }

    let actAddNewStmt = () => {
        setState(st => {
            let (st, _) = addNewStmt(st, ~isBkm=showBkmOnly)
            st
        })
    }
    let actDeleteCheckedStmts = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        {
                            let numOfSelectedStmts = state.checkedStmtIds->Array.length
                            if (numOfSelectedStmts == state.stmts->Array.length) {
                                React.string("Delete all steps?")
                            } else if (numOfSelectedStmts == 1) {
                                React.string("Delete the selected step?")
                            } else {
                                React.string(`Delete ${state.checkedStmtIds->Array.length->Belt.Int.toString}` 
                                                ++ ` selected steps?`)
                            }
                        }
                        <Row>
                            <Button onClick={_=>closeModal(modalRef, modalId)}> {React.string("Cancel")} </Button>
                            <Button
                                onClick={_=>{
                                    closeModal(modalRef, modalId)
                                    setState(deleteCheckedStmts)
                                }}
                                variant=#contained
                            > 
                                {React.string("Delete")} 
                            </Button>
                        </Row>
                    </Col>
                </Paper>
            })
        })->ignore
    }
    let actToggleStmtChecked = (~id:stmtId, ~shift:bool) => {
        if (shift) {
            setStatePriv(st => toggleStmtCheckedWithShift(st, id, ~showBkmOnly=showBkmOnlyRef.current))
        } else {
            setStatePriv(st => toggleStmtChecked(st, id))
        }
    }
    let actToggleMainCheckbox = () => {
        let action = switch mainCheckboxState {
            | Some(true) | None => uncheckAllStmts
            | Some(false) => checkAllStmts
        }
        setStatePriv(st => {
            let st = action(st)
            st->scheduleUnifyAllIfAllowed
        })
    }
    let actMoveCheckedStmtsUp = () => {
        if (showBkmOnly) {
            setState(moveCheckedBookmarkedStmts(_, true))
        } else {
            setState(moveCheckedStmts(_, true))
        }
    }
    let actMoveCheckedStmtsDown = () => {
        if (showBkmOnly) {
            setState(moveCheckedBookmarkedStmts(_, false))
        } else {
            setState(moveCheckedStmts(_, false))
        }
    }
    let actBookmarkCheckedStmts = () => {
        setStatePriv(st => {
            let st = st->bookmarkCheckedStmts(true)
            let st = st->saveStateToLocStorAndMakeHistSnapshot
            st
        })
    }
    let actUnbookmarkCheckedStmts = () => {
        setStatePriv(st => {
            let st = st->bookmarkCheckedStmts(false)
            let st = st->saveStateToLocStorAndMakeHistSnapshot
            st
        })
    }
    let actDuplicateStmt = (top:bool) => setState(st => {
        let st = duplicateCheckedStmt(st,top)
        let st = uncheckAllStmts(st)
        st
    })
    let addStmtAbove = (st:editorState, ~id:stmtId, ~text:string, ~isBkm:bool):editorState => {
        let st = uncheckAllStmts(st)
        let st = toggleStmtChecked(st,id)
        let (st, newId) = addNewStmt(st, ~isBkm)
        let st = setStmtCont(st, newId, text->strToCont)
        st
    }
    let actAddStmtAbove = (~id:stmtId, ~text:string, ~isBkm:bool):unit => {
        setState(st => {
            let st = addStmtAbove(st, ~id, ~text, ~isBkm)
            let st = uncheckAllStmts(st)
            st
        })
    }
    let actAddStmtBelow = (~id:stmtId, ~text:string, ~isBkm:bool):unit => {
        setState(st => {
            let st = addStmtAbove(st, ~id, ~text, ~isBkm)
            let st = moveCheckedStmts(st, true)
            let st = uncheckAllStmts(st)
            st
        })
    }

    let actBeginEdit0 = (setter:editorState=>editorState) => {
        if (!editIsActive) {
            setState(setter)
        } else {
            openInfoDialog(
                ~modalRef, 
                ~title=previousEditingIsNotCompletedTitle,
                ~text=previousEditingIsNotCompletedText
            )
        }
    }
    let actBeginEdit = (setter:(editorState,stmtId)=>editorState, stmtId:string) => {
        if (!editIsActive) {
            setState(setter(_,stmtId))
        } else {
            openInfoDialog(
                ~modalRef, 
                ~title=previousEditingIsNotCompletedTitle,
                ~text=previousEditingIsNotCompletedText
            )
        }
    }
    let actCompleteEdit = (setter:editorState=>editorState) => {
        setState(setter)
    }

    let actDescrEditComplete = newText => {
        setState(completeDescrEditMode(_,newText))
    }

    let actVarsEditComplete = newText => {
        setState(completeVarsEditMode(_,newText))
    }

    let actDisjEditComplete = newText => {
        setState(completeDisjEditMode(_,newText))
    }

    let actSyntaxTreeUpdatedWithoutContentChange = (setter:editorState=>editorState) => {
        setStatePriv(setter)
    }

    let actCompleteEditLabel = (stmtId, newLabel):unit => {
        let newLabel = newLabel->String.trim
        switch state->renameStmt(stmtId, newLabel) {
            | Error(msg) => openInfoDialog( ~modalRef, ~text=`Cannot rename this step: ${msg}` )
            | Ok(st) => setState(_ => completeLabelEditMode(st,stmtId,newLabel))
        }
    }

    let actCancelEditLabel = (stmtId, newLabel):unit => {
        switch state->editorGetStmtById(stmtId) {
            | None => ()
            | Some(stmt) => {
                let textOld = stmt.label
                let textNew = newLabel
                if (textOld == textNew || textNew == "") {
                    setState(completeLabelEditMode(_,stmtId,textOld))
                } else {
                    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                        updateModal(modalRef, modalId, () => {
                            <MM_cmp_save_or_discard
                                contOld={React.string(textOld)}
                                contNew={React.string(textNew)}
                                onDiscard={() => {
                                    closeModal(modalRef, modalId)
                                    setState(completeLabelEditMode(_,stmtId,textOld))
                                }}
                                onSave={() => {
                                    closeModal(modalRef, modalId)
                                    actCompleteEditLabel(stmtId, textNew)
                                }}
                                onContinueEditing={() => {
                                    closeModal(modalRef, modalId)
                                }}
                            />
                        })
                    })->ignore
                }
            }
        }
    }

    let actCancelEditCont = (stmtId, newContText):unit => {
        switch state->editorGetStmtById(stmtId) {
            | None => ()
            | Some(stmt) => {
                let contOld = stmt.cont
                let contNew = newContText->strToCont
                let textOld = contOld->contToStr
                let textNew = contNew->contToStr
                if (
                    textOld == textNew 
                    || (textOld == "" && textNew == state.preCtxData.settingsV.val.defaultStmtType->String.trim)
                ) {
                    if (textOld == "") {
                        setState(deleteStmt(_,stmtId))
                    } else {
                        setState(completeContEditMode(_,stmtId,textOld))
                    }
                } else {
                    if (textNew == "") {
                        setState(completeContEditMode(_,stmtId,textOld))
                    } else {
                        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <MM_cmp_save_or_discard
                                    removeStmt={textOld == ""}
                                    contOld={MM_cmp_user_stmt.rndContText(~stmtCont=contOld)}
                                    contNew={MM_cmp_user_stmt.rndContText(~stmtCont=contNew)}
                                    onDiscard={() => {
                                        closeModal(modalRef, modalId)
                                        if (textOld == "") {
                                            setState(deleteStmt(_,stmtId))
                                        } else {
                                            setState(completeContEditMode(_,stmtId,textOld))
                                        }
                                    }}
                                    onSave={() => {
                                        closeModal(modalRef, modalId)
                                        setState(completeContEditMode(_,stmtId,textNew))
                                    }}
                                    onContinueEditing={() => {
                                        closeModal(modalRef, modalId)
                                    }}
                                />
                            })
                        })->ignore
                    }
                }
            }
        }
    }

    let actCancelEditJstf = (stmtId, newJstfText):unit => {
        switch state->editorGetStmtById(stmtId) {
            | None => ()
            | Some(stmt) => {
                let textOld = switch stmt.typ {
                    | E => defaultJstfForHyp
                    | P => stmt.jstfText->String.trim
                }
                let newJstfTextTrim = newJstfText->String.trim
                let textNew = switch stmt.typ {
                    | P => newJstfTextTrim
                    | E => {
                        if (defaultJstfForHyp == newJstfTextTrim->String.toUpperCase) {
                            defaultJstfForHyp
                        } else {
                            newJstfTextTrim
                        }
                    }
                }
                let nothingChanged = textOld == textNew
                if (nothingChanged) {
                    setState(completeJstfEditMode(_,stmtId,textOld))
                } else {
                    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                        updateModal(modalRef, modalId, () => {
                            <MM_cmp_save_or_discard
                                contOld={React.string(textOld)}
                                contNew={React.string(textNew)}
                                onDiscard={() => {
                                    closeModal(modalRef, modalId)
                                    setState(completeJstfEditMode(_,stmtId,textOld))
                                }}
                                onSave={() => {
                                    closeModal(modalRef, modalId)
                                    setState(completeJstfEditMode(_,stmtId,textNew))
                                }}
                                onContinueEditing={() => {
                                    closeModal(modalRef, modalId)
                                }}
                            />
                        })
                    })->ignore
                }
            }
        }
    }

    let actCancelEditDescr = (newText):unit => {
        let textOld = state.descr->String.trim
        let textNew = newText->String.trim
        if (textOld == textNew || textNew == "") {
            setState(completeDescrEditMode(_,textOld))
        } else {
            openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                updateModal(modalRef, modalId, () => {
                    <MM_cmp_save_or_discard
                        contOld={<pre>{React.string(textOld)}</pre>}
                        contNew={<pre>{React.string(textNew)}</pre>}
                        onDiscard={() => {
                            closeModal(modalRef, modalId)
                            setState(completeDescrEditMode(_,textOld))
                        }}
                        onSave={() => {
                            closeModal(modalRef, modalId)
                            setState(completeDescrEditMode(_,textNew))
                        }}
                        onContinueEditing={() => {
                            closeModal(modalRef, modalId)
                        }}
                    />
                })
            })->ignore
        }
    }

    let actCancelEditVars = (newText):unit => {
        let textOld = state.varsText->String.trim
        let textNew = newText->String.trim
        if (textOld == textNew || textNew == "") {
            setState(completeVarsEditMode(_,textOld))
        } else {
            openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                updateModal(modalRef, modalId, () => {
                    <MM_cmp_save_or_discard
                        contOld={<pre>{React.string(textOld)}</pre>}
                        contNew={<pre>{React.string(textNew)}</pre>}
                        onDiscard={() => {
                            closeModal(modalRef, modalId)
                            setState(completeVarsEditMode(_,textOld))
                        }}
                        onSave={() => {
                            closeModal(modalRef, modalId)
                            setState(completeVarsEditMode(_,textNew))
                        }}
                        onContinueEditing={() => {
                            closeModal(modalRef, modalId)
                        }}
                    />
                })
            })->ignore
        }
    }

    let actCancelEditDisj = (newText):unit => {
        let textOld = state.disjText->String.trim
        let textNew = newText->String.trim
        if (textOld == textNew || textNew == "") {
            setState(completeDisjEditMode(_,textOld))
        } else {
            openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                updateModal(modalRef, modalId, () => {
                    <MM_cmp_save_or_discard
                        contOld={<pre>{React.string(textOld)}</pre>}
                        contNew={<pre>{React.string(textNew)}</pre>}
                        onDiscard={() => {
                            closeModal(modalRef, modalId)
                            setState(completeDisjEditMode(_,textOld))
                        }}
                        onSave={() => {
                            closeModal(modalRef, modalId)
                            setState(completeDisjEditMode(_,textNew))
                        }}
                        onContinueEditing={() => {
                            closeModal(modalRef, modalId)
                        }}
                    />
                })
            })->ignore
        }
    }

    let actToggleParenAc = () => {
        setParenAc(prev => !prev)
    }

    let addNewStatementsPriv = (st:editorState, stmtsDto:stmtsDto):editorState => {
        st->addNewStatements(stmtsDto, ~isBkm=showBkmOnlyRef.current)
    }

    let actAddAsrtByLabel = (asrtLabel:string):promise<result<unit,string>> => {
        Promise.make((resolve, _) => {
            setState(st => {
                switch st.wrkCtx {
                    | None => {
                        resolve(Error("MM context is not loaded."))
                        st
                    }
                    | Some(wrkCtx) => {
                        switch wrkCtx->getFrame(asrtLabel) {
                            | None => {
                                resolve(Error(`Cannot find a frame by name '${asrtLabel}'`))
                                st
                            }
                            | Some(frame) => {
                                let st = addNewStatementsPriv(st,MM_wrk_search_asrt.frameToStmtsDto(~wrkCtx,~frame))
                                resolve(Ok(()))
                                st
                            }
                        }
                    }
                }
            })
        })
    }

    let actAsrtSearchResultsSelected = (selectedLabels:array<string>):unit => {
        switch state.wrkCtx {
            | None => showErrMsg(~text="MM context is not loaded.")
            | Some(wrkCtx) => {
                switch selectedLabels->Array.reduce(Ok([]), (res,label) => {
                    switch res {
                        | Error(_) => res
                        | Ok(selectedFrames) => {
                            switch wrkCtx->getFrame(label) {
                                | None => Error(`Internal error: cannot find a frame by label '${label}'`)
                                | Some(frame) => {
                                    selectedFrames->Array.push(frame)
                                    Ok(selectedFrames)
                                }
                            }
                        }
                    }
                }) {
                    | Error(msg) => showErrMsg(~text=msg)
                    | Ok(selectedFrames) => {
                        setState(st => {
                            selectedFrames->Array.reduce(
                                st,
                                (st,frame) => {
                                    addNewStatementsPriv(st,MM_wrk_search_asrt.frameToStmtsDto(~wrkCtx, ~frame))
                                }
                            )
                        })
                    }
                }
            }
        }
    }

    React.useEffect0(() => {
        addAsrtByLabel.contents = Some(actAddAsrtByLabel)
        updateTabTitle.contents = Some(newTabTitle => {
            setStatePriv(st => {
                let st = {...st, tabTitle:newTabTitle}
                editorSaveStateToLocStor(st, ~editorId)
                st
            })
        })
        None
    })

    let actWrkSubsSelected = wrkSubs => {
        setState(st => st->applySubstitutionForEditor(wrkSubs))
    }

    let actOnMergeStmtsSelected = (~stmtToUse:userStmt,~stmtToRemove:userStmt,~continueMergingStmts:bool) => {
        setState(st => {
            switch st->mergeStmts(stmtToUse.id, stmtToRemove.id) {
                | Ok(st) => {
                    let st = st->uncheckAllStmts
                    if (continueMergingStmts) {
                        let st = st->verifyEditorState
                        if (st->editorStateHasDuplicatedStmts) {
                            st->setNextAction(Some(MergeNextDuplicate))
                        } else {
                            st
                        }
                    } else {
                        st
                    }
                }
                | Error(msg) => {
                    openInfoDialog(~modalRef, ~text=msg)
                    st
                }
            }
        })
    }

    let actRestorePrevState = (histIdx:int):unit => {
        switch state->restoreEditorStateFromSnapshot(hist, histIdx) {
            | Error(msg) => openInfoDialog( ~modalRef, ~title="Could not restore editor state", ~text=msg )
            | Ok(editorState) => setState(_ => editorState->recalcWrkColors)
        }
    }

    let viewOptions = { 
        MM_cmp_user_stmt.showCheckbox:showCheckbox, 
        showLabel, showType, showJstf, inlineMode, 
        smallBtns, 
    }

    let actOpenRestorePrevStateDialog = () => {
        openModalFullScreen(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_editor_hist 
                    modalRef
                    editorState={state->removeAllTempData}
                    hist 
                    onClose={_=>closeModal(modalRef, modalId)} 
                    viewOptions
                    stepsPerPage
                    onRestore={histIdx => {
                        actRestorePrevState(histIdx)
                        closeModal(modalRef, modalId)
                    }}
                />
            })
        })->ignore
    }

    let actMergeStmts = () => {
        switch state->findStmtsToMerge {
            | Error(msg) => openInfoDialog( ~modalRef, ~text=msg )
            | Ok((stmt1,stmt2)) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_merge_two_stmts
                            stmt1
                            stmt2
                            onStmtSelected={(stmtToUse,stmtToRemove)=>{
                                closeModal(modalRef, modalId)
                                actOnMergeStmtsSelected(
                                    ~stmtToUse, ~stmtToRemove, ~continueMergingStmts = numOfCheckedStmts==0
                                )
                            }}
                            onCancel={()=>closeModal(modalRef, modalId)}
                        />
                    })
                })->ignore
            }
        }
    }

    let actSearchAsrt = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_search_asrt
                            modalRef
                            preCtxData=state.preCtxData
                            wrkCtx
                            initialTyp={getLastUsedTyp(state.preCtxData.ctxV.val.min)}
                            onTypChange={saveLastUsedTyp(state.preCtxData.ctxV.val.min, _)}
                            onCanceled={()=>closeModal(modalRef, modalId)}
                            onResultsSelected={selectedLabels=>{
                                closeModal(modalRef, modalId)
                                actAsrtSearchResultsSelected(selectedLabels)
                            }}
                        />
                    })
                })->ignore
            }
        }
    }

    let getSelectionText = (stmt:userStmt):option<(string,Date.t)> => {
        switch stmt.cont {
            | Text(_) => None
            | Tree({clickedNodeId}) => {
                switch clickedNodeId {
                    | None => None
                    | Some((_,time)) => stmt.cont->getSelectedText->Belt.Option.map(str => (str,time))
                }
            }
        }
    }

    let getStmtTextIfChecked = (st:editorState,stmt:userStmt):option<(string,Date.t)> => {
        st.checkedStmtIds->Array.find(((id,_)) => id == stmt.id)
            ->Belt_Option.map(((_,time)) => (stmt.cont->contToStr, time))
    }

    let getSelectedExpr = (st:editorState,stmt:userStmt):option<(string,Date.t)> => {
        switch getSelectionText(stmt) {
            | Some(res) => Some(res)
            | None => getStmtTextIfChecked(st,stmt)
        }
    }

    let getExprsToSubstitute = (st:editorState):(option<string>,option<string>) => {
        let selections = st.stmts->Array.map(stmt => getSelectedExpr(st,stmt))
            ->Array.filter(Belt_Option.isSome(_))
            ->Array.map(Belt_Option.getExn(_))
            ->Expln_utils_common.sortInPlaceWith(((_,time1),(_,time2)) => compareDates(time1,time2) )
        let len = selections->Array.length
        let sel1 = selections->Belt_Array.get(len-2)->Belt.Option.map(((str,_)) => str)
        let sel2 = selections->Belt_Array.get(len-1)->Belt.Option.map(((str,_)) => str)
        switch len {
            | 0 => (None, None)
            | 1 => (sel2, None)
            | _ => (sel1, sel2)
        }
    }

    let actSubstitute = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                let (expr1Init,expr2Init) = getExprsToSubstitute(state)
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_substitution
                            modalRef
                            editorState=state
                            expr1Init
                            expr2Init
                            wrkCtx
                            onCanceled={()=>closeModal(modalRef, modalId)}
                            onSubstitutionSelected={wrkSubs=>{
                                closeModal(modalRef, modalId)
                                actWrkSubsSelected(wrkSubs)
                            }}
                        />
                    })
                })->ignore
            }
        }
    }
    onOpenSubstitutionDialogRef.current = actSubstitute

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let prepareArgs0 = (argLabels:array<string>, rootStmts:array<rootStmt>):array<expr> => {
        rootStmts
            ->Array.filter(stmt => argLabels->Array.includes(stmt.label))
            ->Array.map(stmt => stmt.expr)
    }

    let getArgs0AndAsrtLabel = (checkedStmts:array<userStmt>, rootStmts:array<rootStmt>):option<(array<expr>,option<string>)> => {
        if (checkedStmts->Array.length == 0 || (checkedStmts->Array.getUnsafe(checkedStmts->Array.length-1)).typ != P) {
            None
        } else {
            let stmtToProve = checkedStmts->Array.getUnsafe(checkedStmts->Array.length-1)
            switch stmtToProve.jstfText->parseJstf {
                | Ok(Some({args:argLabels, label})) => Some((prepareArgs0(argLabels, rootStmts), Some(label)))
                | Error(_) | Ok(None) => {
                    if (checkedStmts->Array.length == 1) {
                        None
                    } else {
                        Some((
                            prepareArgs0(
                                checkedStmts->Array.slice(~start=0,~end=checkedStmts->Array.length - 1)
                                    ->Array.map(stmt => stmt.label), 
                                rootStmts
                            ), 
                            None
                        ))
                    }
                }
            }
        }
    }

    let shouldCloseBottomUpProverDialog = (
        ~isApiCall:bool,
        ~selectFirstFoundProof:option<bool>,
        ~selectedResult:option<result<stmtsDto,string>>,
    ):bool => {
        !isApiCall 
        || selectFirstFoundProof->Option.getOr(false) 
        || (
            selectFirstFoundProof->Option.isNone 
            &&
            switch selectedResult {
                | Some(Ok(_)) => true
                | None | Some(Error(_)) => false
            }
        )
    }

    let rec actUnify = (
        ~stmtId:option<stmtId>=?,
        ~params:option<bottomUpProverParams>=?,
        ~initialDebugLevel:option<int>=?,
        ~isApiCall:bool=false,
        ~delayBeforeStartMs:int=0,
        ~selectFirstFoundProof:option<bool>=?,
        ~bottomUpProofResultConsumer:option<result<stmtsDto,string>>=>unit = _ => (),
        ~nextAction: unit=>unit = ()=>()
    ) => {
        if (thereAreCriticalErrorsInEditor) {
            ()
        } else {
            switch state.wrkCtx {
                | None => ()
                | Some(wrkCtx) => {
                    let varsText=state.varsText
                    let disjText=state.disjText
                    let state = switch stmtId {
                        | None => state
                        | Some(stmtId) => {
                            let state = state->uncheckAllStmts
                            let state = state->toggleStmtChecked(stmtId)
                            state
                        }
                    }
                    let rootUserStmts = state->getRootStmtsForUnification
                    let rootStmts = rootUserStmts->Array.map(userStmtToRootStmt)
                    let checkedStmtIds = state.checkedStmtIds->Array.map(((stmtId,_)) => stmtId)
                        ->Belt_HashSetString.fromArray
                    let checkedStmts = state.stmts
                        ->Array.filter(stmt => checkedStmtIds->Belt_HashSetString.has(stmt.id))
                    let settings = state.preCtxData.settingsV.val
                    if (checkedStmts->Array.length > 0 && (checkedStmts->Array.getUnsafe(checkedStmts->Array.length-1)).typ == P) {
                        let initialParams = switch params {
                            | Some(_) => params
                            | None => {
                                switch getArgs0AndAsrtLabel(checkedStmts, rootStmts) {
                                    | None => None
                                    | Some((deriveFromOnLevel0,asrtLabel)) => {
                                        let bottomUpProverDefaults = preCtxData.settingsV.val.bottomUpProverDefaults
                                        Some(
                                            bottomUpProverParamsMakeDefault(
                                                ~asrtLabel?, 
                                                ~deriveFromOnLevel0, 
                                                ~maxSearchDepth=bottomUpProverDefaults.searchDepth,
                                                ~lengthRestrict=bottomUpProverDefaults.lengthRestrict
                                                    ->lengthRestrictFromStr->Option.getOr(Less),
                                                ~allowNewDisjForExistingVars=
                                                    bottomUpProverDefaults.allowNewDisjForExistingVars,
                                                ~allowNewStmts=bottomUpProverDefaults.allowNewStmts,
                                                ~allowNewVars=bottomUpProverDefaults.allowNewVars,
                                            )
                                        )
                                    }
                                }
                            }
                        }
                        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <MM_cmp_unify_bottom_up
                                    modalRef
                                    settingsVer=state.preCtxData.settingsV.ver
                                    settings
                                    preCtxVer=state.preCtxData.ctxV.ver
                                    preCtx=state.preCtxData.ctxV.val.min
                                    frms=state.preCtxData.frms parenCnt=state.preCtxData.parenCnt
                                    varsText disjText wrkCtx
                                    rootStmts=rootUserStmts
                                    reservedLabels={state.stmts->Array.map(stmt => stmt.label)}
                                    typeToPrefix = {
                                        Belt_MapString.fromArray(
                                            settings.typeSettings->Array.map(ts => (ts.typ, ts.prefix))
                                        )
                                    }
                                    initialParams=?initialParams
                                    apiCallStartTime={if (isApiCall) {Some(Date.make())} else {None} }
                                    delayBeforeStartMs
                                    initialDebugLevel=?initialDebugLevel
                                    selectFirstFoundProof
                                    onResultSelected={newStmtsDto => {
                                        actBottomUpResultSelected( 
                                            ~selectedResult=newStmtsDto,
                                            ~bottomUpProofResultConsumer,
                                        )
                                        if (
                                            shouldCloseBottomUpProverDialog(
                                                ~isApiCall, 
                                                ~selectFirstFoundProof, 
                                                ~selectedResult=newStmtsDto,
                                            )
                                        ) {
                                            closeModal(modalRef, modalId)
                                        }
                                    }}
                                    onCancel={() => closeModal(modalRef, modalId)}
                                />
                            })
                        })->ignore
                    } else {
                        openModal(modalRef, () => rndProgress(~text="Unifying all", ~pct=0.))->promiseMap(modalId => {
                            let onTerminate = makeActTerminate(modalId)
                            updateModal( 
                                modalRef, modalId, () => rndProgress(
                                    ~text="Unifying all", ~pct=0., ~onTerminate
                                )
                            )
                            let rootStmts = rootUserStmts->Array.map(userStmtToRootStmt)
                            unify(
                                ~settingsVer=state.preCtxData.settingsV.ver,
                                ~settings,
                                ~preCtxVer=state.preCtxData.ctxV.ver,
                                ~preCtx=state.preCtxData.ctxV.val.min,
                                ~varsText,
                                ~disjText,
                                ~rootStmts,
                                ~bottomUpProverParams=None,
                                ~allowedFrms=settings.allowedFrms,
                                ~syntaxTypes=Some(state.preCtxData.syntaxTypes),
                                ~exprsToSyntaxCheck=
                                    if (settings.checkSyntax) {
                                        Some(state->getAllExprsToSyntaxCheck(rootStmts))
                                    } else {
                                        None
                                    },
                                ~debugLevel=0,
                                ~onProgress = msg => updateModal(
                                    modalRef, modalId, () => rndProgress( ~text=msg, ~onTerminate )
                                )
                            )->promiseMap(proofTreeDto => {
                                actUnifyAllResultsAreReady(proofTreeDto, nextAction)
                                closeModal(modalRef, modalId)
                            })
                        })->ignore
                    }
                }
            }
        }
    } and let actBottomUpResultSelected = (
        ~selectedResult:option<result<stmtsDto,string>>,
        ~bottomUpProofResultConsumer:option<result<stmtsDto,string>>=>unit,
    ) => {
        switch selectedResult {
            | None => bottomUpProofResultConsumer(None)
            | Some(selectedResult) => {
                switch selectedResult {
                    | Ok(selectedResult) => {
                        setState(st => {
                            let st = st->addNewStatements(selectedResult, ~isBkm = showBkmOnly)
                            let st = st->uncheckAllStmts
                            let st = st->setNextAction(Some(
                                UnifyAll({nextAction:() => bottomUpProofResultConsumer(Some(Ok(selectedResult)))})
                            ))
                            st
                        })
                    }
                    | Error(msg) => bottomUpProofResultConsumer(Some(Error(msg)))
                }
            }
        }
    }

    React.useEffect1(() => {
        switch state.nextAction {
            | None => ()
            | Some(action) => {
                setStatePriv(setNextAction(_,None))
                switch action {
                    | UnifyAll({nextAction}) => actUnify(~nextAction)
                    | MergeNextDuplicate => {
                        if (state->editorStateHasDuplicatedStmts) {
                            actMergeStmts()
                        }
                    }
                    | Action(action) => action()
                }
            }
        }
        None
    }, [state.nextAction])

    let actExportProof = (stmtId) => {
        switch generateCompressedProof(state, stmtId, ~useAllLocalEHyps=false) {
            | None => ()
            | Some((proofTextReqHyps,proofTableWithTypes,proofTableWithoutTypes)) => {
                switch generateCompressedProof(state, stmtId, ~useAllLocalEHyps=true) {
                    | None => ()
                    | Some((proofTextAllHyps,_,_)) => {
                        openModal(modalRef, () => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <MM_cmp_export_proof 
                                    proofTextReqHyps proofTextAllHyps proofTableWithTypes proofTableWithoutTypes 
                                    onClose={_=>closeModal(modalRef, modalId)} 
                                />
                            })
                        })->ignore
                    }
                }
            }
        }
    }

    let currGoalStmtStatus:option<(stmtId,string,option<proofStatus>)> = state.stmts
        ->Array.find(stmt => stmt.isGoal)
        ->Belt.Option.map(stmt => (stmt.id, stmt.cont->contToStr, stmt.proofStatus))
    let prevGoalStmtStatus:React.ref<(string,proofStatus)> = React.useRef(("",NoJstf))
    React.useEffect1(() => {
        switch state.nextAction {
            | Some(_) => ()
            | None => {
                switch currGoalStmtStatus {
                    | None => ()
                    | Some((curGoalId,curGoalText,currGoalStatus)) => {
                        switch currGoalStatus {
                            | None => prevGoalStmtStatus.current = (curGoalText,NoJstf)
                            | Some(currGoalStatus) => {
                                let (prevGoalText,prevGoalStatus) = prevGoalStmtStatus.current
                                if (currGoalStatus == Ready && (prevGoalStatus != Ready || curGoalText != prevGoalText)) {
                                    actExportProof(curGoalId)
                                }
                                prevGoalStmtStatus.current = (curGoalText,currGoalStatus)
                            }
                        }
                    }
                }
            }
        }
        None
    }, [currGoalStmtStatus])

    let actExportToJson = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_export_state_to_json 
                    jsonStr=Expln_utils_common.stringify(state->editorStateToEditorStateLocStor)
                    onClose={_=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    let actExportToUrl = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_export_state_to_url 
                    editorStateBase64=strToSafeBase64(Expln_utils_common.stringify(state->editorStateToEditorStateLocStor))
                    onClose={_=>closeModal(modalRef, modalId)} 
                />
            })
        })->ignore
    }

    let actLoadEditorState = (stateLocStor:editorStateLocStor):unit => {
        actResetPageIdx()
        let nextStmtId = hist->editorHistGetMaxIntStmtId->Option.map(i => i+1)->Option.getOr(0)
        let newState = createInitialEditorState( ~preCtxData, ~stateLocStor=Some(stateLocStor), ~nextStmtId, )
            ->setNextAction(Some(Action(()=>())))
        setState(_ => newState)
        onTabTitleChange(newState.tabTitle)
        reloadCtx.current->Option.forEach(reloadCtx => {
            reloadCtx(~srcs=stateLocStor.srcs, ~settings=state.preCtxData.settingsV.val)->ignore
        })
    }

    let actImportFromJson = (jsonStr:string):bool => {
        switch readEditorStateFromJsonStr(jsonStr) {
            | Error(errorMsg) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                            <Col spacing=1.>
                                { React.string(errorMsg) }
                                <Button onClick={_ => closeModal(modalRef, modalId) } variant=#contained> 
                                    {React.string("Ok")} 
                                </Button>
                            </Col>
                        </Paper>
                    })
                })->ignore
                false
            }
            | Ok(stateLocStor) => {
                actLoadEditorState(stateLocStor)
                true
            }
        }
    }

    let actOpenImportFromJsonDialog = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_import_from_json
                    onImport={text=>{
                        if (actImportFromJson(text)) {
                            closeModal(modalRef, modalId)
                        }
                    }}
                    onCancel={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    let actShowCompletedProof = () => {
        switch state->getTheOnlyCheckedStmt {
            | Some(stmt) if stmt.typ == P => {
                switch stmt.proofStatus {
                    | Some(Ready) => actExportProof(stmt.id)
                    | Some(Waiting) | Some(NoJstf) | Some(JstfIsIncorrect) | None => 
                        showInfoMsg(~title=`A proof is not available`, ~text=infoAboutGettingCompletedProof)
                }
            }
            | _ => showInfoMsg(~title=`A proof is not available`, ~text=infoAboutGettingCompletedProof)
        }
    }

    let makeFrameProofDataForAssertions = (
        ~preCtxData:preCtxData,
        ~labels:Belt_SetString.t,
        ~onProgress:float=>unit,
    ):promise<result<Belt_MapString.t<frameProofData>,string>> => {
        if (labels->Belt_SetString.size == 0) {
            Promise.resolve(Ok(Belt_MapString.empty))
        } else {
            let pctPerFrame = 1.0 /. labels->Belt_SetString.size->Int.toFloat
            let rec go = async (
                ~labelsToProcess:Belt_SetString.t, 
                ~res:result<Belt_MapString.t<frameProofData>,string>,
            ):result<Belt_MapString.t<frameProofData>,string> => {
                switch res {
                    | Error(_) => res
                    | Ok(proofDatas) => {
                        if (labelsToProcess->Belt_SetString.size == 0) {
                            res
                        } else {
                            let curLabel = labelsToProcess->Belt_SetString.toArray->Array.getUnsafe(0)
                            switch await MM_cmp_pe_frame_full.makeFrameProofData(
                                ~preCtxData,
                                ~label=curLabel,
                                ~onProgress = pct => {
                                    let processedPct = proofDatas->Belt_MapString.size->Int.toFloat *. pctPerFrame
                                    let curPct = processedPct +. pctPerFrame *. pct
                                    onProgress(curPct)
                                }
                            ) {
                                | Error(msg) => {
                                    let errMsg = `An error occured while getting proof for ${curLabel}: ${msg}`
                                    await go(~labelsToProcess=Belt_SetString.empty, ~res=Error(errMsg))
                                }
                                | Ok(proofData) => {
                                    await go(
                                        ~labelsToProcess=labelsToProcess->Belt_SetString.remove(curLabel),
                                        ~res=Ok(proofDatas->Belt_MapString.set(curLabel, proofData))
                                    )
                                }
                            }
                        }
                    }
                }
            }
            go(~labelsToProcess=labels, ~res=Ok(Belt_MapString.empty))
        }
    }

    let inlineTheoremForStep = (
        ~state:editorState,
        ~stmtId:stmtId,
        ~proofData:Belt_MapString.t<frameProofData>,
    ):result<editorState,string> => {
        let state = state->uncheckAllStmts->toggleStmtChecked(stmtId)
        switch state->getTheOnlyCheckedStmt {
            | None => Error("Internal error: cannot find the step to inline the proof for.")
            | Some(stmt) => {
                switch stmt.src {
                    | None | Some(VarType) | Some(Hypothesis(_)) | Some(AssertionWithErr(_)) => {
                        Error("Not a provable step.")
                    }
                    | Some(Assertion({args, label})) => {
                        switch stmt.proofTreeDto {
                            | None => Error("Internal error: proofTreeDto is not set.")
                            | Some(proofTreeDto) => {
                                switch state.wrkCtx {
                                    | None => Error("Internal error: wrkCtx is not set.")
                                    | Some(wrkCtx) => {
                                        switch proofData->Belt_MapString.get(label) {
                                            | None => Error(`Internal error: no proof data for label ${label}.`)
                                            | Some(frameProofData) => {
                                                switch MM_cmp_pe_frame_full.frameProofDataToStmtsDto(
                                                    ~preCtxData, ~wrkCtx, 
                                                    ~proofTreeDto, ~args, ~frameProofData
                                                ) {
                                                    | Error(msg) => Error(msg)
                                                    | Ok(stmtsDto) => Ok(state->addNewStatements(stmtsDto))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let findStepToInlineProofFor = (
        ~state:editorState,
        ~proofData:Belt_MapString.t<frameProofData>,
        ~updatedLabels:Belt_HashSetString.t,
    ):option<userStmt> => {
        state.stmts->Array.find(stmt => {
            !(updatedLabels->Belt_HashSetString.has(stmt.label))
            && switch stmt.src {
                | None | Some(VarType) | Some(Hypothesis(_)) | Some(AssertionWithErr(_)) => false
                | Some(Assertion({label})) => proofData->Belt_MapString.has(label) && stmt.proofTreeDto->Option.isSome
            }
        })
    }

    let inlineTheoremForSteps = (
        ~state:editorState,
        ~proofData:Belt_MapString.t<frameProofData>,
        ~onProcessedStepCntChange:int=>unit,
    ):(editorState,option<string>) => {
        let state = ref(state)
        let errMsg = ref(None)
        let stepsInlinedCnt = ref(0)
        onProcessedStepCntChange(stepsInlinedCnt.contents)
        let updatedLabels = Belt_HashSetString.make(~hintSize=10)
        let getNextStmt = ():option<userStmt> => {
            switch findStepToInlineProofFor( ~state=state.contents, ~proofData, ~updatedLabels) {
                | None => None
                | Some(stmt) => {
                    updatedLabels->Belt_HashSetString.add(stmt.label)
                    Some(stmt)
                }
            }
        }
        let stmtToInlineRef = ref(getNextStmt())
        while (stmtToInlineRef.contents->Option.isSome && errMsg.contents->Option.isNone) {
            let stmtToInline = stmtToInlineRef.contents
                ->Option.getExn(~message="inlineTheoremForSteps: stmtToInlineRef.contents is None.")
            switch inlineTheoremForStep(
                ~state=state.contents,
                ~stmtId=stmtToInline.id,
                ~proofData,
            ) {
                | Error(msg) => errMsg := Some(`An error happened while inlining step ${stmtToInline.label}: ${msg}`)
                | Ok(updatedState) => {
                    state := updatedState
                    stepsInlinedCnt := stepsInlinedCnt.contents + 1
                    onProcessedStepCntChange(stepsInlinedCnt.contents)
                    stmtToInlineRef := getNextStmt()
                }
            }
        }
        (state.contents, errMsg.contents)
    }

    let inlineTheorems = (labels:array<string>):unit => {
        openModal(modalRef, () => rndProgress(~text="Collecting proof data", ~pct=0.))->Promise.thenResolve(modalId => {
            let onTerminate = makeActTerminate(modalId)
            updateModal( modalRef, modalId, () => rndProgress( ~text="Collecting proof data", ~pct=0., ~onTerminate ) )
            makeFrameProofDataForAssertions(
                ~preCtxData,
                ~labels=Belt_SetString.fromArray(labels),
                ~onProgress = pct=>updateModal(
                    modalRef, modalId, () => rndProgress( ~text="Collecting proof data", ~pct, ~onTerminate )
                ),
            )->Promise.thenResolve(proofData => {
                switch proofData {
                    | Error(msg) => {
                        closeModal(modalRef, modalId)
                        showInfoMsg(~title=`Cannot inline theorems`, ~text=msg)
                    }
                    | Ok(proofData) => {
                        updateModal( modalRef, modalId, () => rndProgress( ~text="Inlining theorems" ) )
                        let (newState,errMsg) = inlineTheoremForSteps(
                            ~state, 
                            ~proofData,
                            ~onProcessedStepCntChange = cnt => updateModal( 
                                modalRef, modalId, 
                                () => rndProgress(~text=`Inlining theorems: ${cnt->Int.toString} steps updated.`) 
                            )
                        )
                        closeModal(modalRef, modalId)
                        setState(_ => {
                            {
                                ...newState,
                                nextAction: errMsg->Option.map(errMsg => {
                                    Action(() => showErrMsg(~title="Inline theorems", ~text=errMsg))
                                })
                            }
                        })
                    }
                }
            })
        })->ignore
    }

    let actInlineTheorems = () => {
        switch state.wrkCtx {
            | None => showErrMsg(~title="actInlineTheorems internal error", ~text="wrkCtx is not set.")
            | Some(wrkCtx) => {
                let theoremLabels = state.stmts->Array.map(stmt => {
                    switch stmt.src {
                        | None | Some(VarType) | Some(Hypothesis(_)) | Some(AssertionWithErr(_)) => None
                        | Some(Assertion({label})) => Some((label, wrkCtx->getTokenType(label)))
                    }
                })
                    ->Array.map(labelData => {
                        switch labelData {
                            | Some((label,Some(P))) => Some(label)
                            | None | Some((_,None)) | Some((_,Some(_))) => None
                        }
                    })
                    ->Array.filter(Option.isSome(_))
                    ->Array.map(opt => {
                        opt->Option.getExn(
                            ~message="actInlineTheorems internal error: cannot map to label."
                        )
                    })
                    ->Belt_SetString.fromArray
                    ->Belt_SetString.toArray
                    ->Array.toSorted(String.compare)
                if (theoremLabels->Array.length == 0) {
                    showInfoMsg(~title=`Cannot inline theorems`, ~text=infoAboutInliningTheorems)
                } else {
                    openModalPaneWithTitle(
                        ~modalRef, 
                        ~content = (~close:unit=>unit) => {
                            <MM_cmp_editor_select_theorems
                                modalRef
                                labels=theoremLabels
                                onOk={selectedLabels => {
                                    close()
                                    inlineTheorems(selectedLabels)
                                }}
                                onCancel={() => close()}
                            />
                        },
                    )
                }
            }
        }
    }

    let actDeleteUnrelatedSteps = (~deleteHyps:bool) => {
        switch state->deleteUnrelatedSteps(
            ~stepIdsToKeep=state.checkedStmtIds->Array.map(((id,_)) => id),
            ~deleteHyps
        ) {
            | Ok(state) => setState(_ => state)
            | Error(msg) => openInfoDialog( ~modalRef, ~text=msg )
        }
    }

    let actRenumberSteps = () => {
        switch state->renumberProvableSteps {
            | Ok(state) => setState(_ => state)
            | Error(msg) => openInfoDialog( ~modalRef, ~text=msg )
        }
    }

    let actRenameHypotheses = () => {
        switch state.stmts->Array.find(stmt => stmt.isGoal) {
            | None => {
                openInfoDialog( 
                    ~modalRef, 
                    ~title="Cannot rename hypotheses",
                    ~text=`The goal step is not set. Please mark one of the steps as the goal step.` 
                )
            }
            | Some(goalStmt) => {
                switch state->renumberHypothesisSteps(~goalLabel=goalStmt.label) {
                    | Ok(state) => setState(_ => state)
                    | Error(msg) => openInfoDialog( ~modalRef, ~text=msg )
                }
            }
        }
    }

    let actReorderSteps = () => {
        setState(reorderSteps)
    }

    let actOpenMoveStepsDialog = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col>
                        <ListCmp disablePadding=true>
                            <ListItem disablePadding=true >
                                <ListItemButton 
                                    onClick={_=>{
                                        closeModal(modalRef, modalId)
                                        setState(moveCheckedStmtsToTop)
                                    }}
                                >
                                    <ListItemText>
                                        {React.string("Move selected steps to the top")}
                                    </ListItemText>
                                </ListItemButton>
                            </ListItem>
                            <ListItem disablePadding=true >
                                <ListItemButton 
                                    onClick={_=>{
                                        closeModal(modalRef, modalId)
                                        setState(moveCheckedStmtsToBottom)
                                    }}
                                >
                                    <ListItemText>
                                        {React.string("Move selected steps to the bottom")}
                                    </ListItemText>
                                </ListItemButton>
                            </ListItem>
                        </ListCmp>
                        <Row>
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                                {React.string("Cancel")}
                            </Button>
                            {
                                rndHiddenTextField(
                                    ~onKeyDown=kbrdHnds([
                                        kbrdClbkMake(~key=keyEsc, ~act=()=>closeModal(modalRef, modalId)),
                                    ])
                                )
                            }
                        </Row>
                    </Col>
                </Paper>
            })
        })->ignore
    }

    let actDebugUnifyAll = (stmtId) => {
        let st = state
        let st = st->uncheckAllStmts
        let st = st->toggleStmtChecked(stmtId)
        let singleProvableChecked = switch getTheOnlyCheckedStmt(st) {
            | Some(stmt) if stmt.typ == P => Some(stmt)
            | _ => None
        }
        switch singleProvableChecked {
            | None => ()
            | Some(singleProvableChecked) => {
                let rootUserStmts = st->getRootStmtsForUnification
                let rootStmts = rootUserStmts->Array.map(userStmtToRootStmt)
                let (params,debugLevel) = switch getArgs0AndAsrtLabel([singleProvableChecked], rootStmts) {
                    | Some((deriveFromOnLevel0,asrtLabel)) => {
                        (
                            bottomUpProverParamsMakeDefault(
                                ~deriveFromOnLevel0, 
                                ~deriveFromOnLevel1=[],
                                ~asrtLabel?, 
                                ~maxSearchDepth=1,
                                ~lengthRestrict=Less,
                                ~allowNewDisjForExistingVars=false,
                                ~allowNewStmts=false,
                                ~allowNewVars=false,
                                ~maxNumberOfBranches=?None
                            ),
                            2
                        )
                    }
                    | None => {
                        (
                            bottomUpProverParamsMakeDefault(
                                ~deriveFromOnLevel0=rootStmts->Array.map(stmt => stmt.expr), 
                                ~deriveFromOnLevel1=[],
                                ~asrtLabel=?None, 
                                ~maxSearchDepth=1,
                                ~lengthRestrict=No,
                                ~allowNewDisjForExistingVars=false,
                                ~allowNewStmts=false,
                                ~allowNewVars=false,
                                ~maxNumberOfBranches=?None
                            ),
                            1
                        )
                    }
                }
                actUnify(
                    ~stmtId, 
                    ~params, 
                    ~initialDebugLevel = debugLevel
                )
            }
        }
    }

    let actOpenViewOptionsDialog = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_editor_view_options
                    onClose={()=>closeModal(modalRef, modalId)}
                    showCheckbox onShowCheckboxChange = {b => setShowCheckbox(_ => b) }
                    showLabel onShowLabelChange = {b => setShowLabel(_ => b) }
                    showType onShowTypeChange = {b => setShowType(_ => b) }
                    showJstf onShowJstfChange = {b => setShowJstf(_ => b) }
                    inlineMode onInlineModeChange = {b => setInlineMode(_ => b) }
                    smallBtns onSmallBtnsChange = {b => setSmallBtns(_ => b) }
                    stepsPerPage onStepsPerPageChange=actSetStepsPerPage
                />
            })
        })->ignore
    }

    let actRenameThisTab = () => {
        openModalPaneWithTitle(
            ~modalRef,
            ~title="Rename tab",
            ~content = (~close) => {
                <MM_cmp_rename_tab 
                    initName=state.tabTitle
                    onOk={newName => {
                        close()
                        onTabTitleChange(newName)
                    }}
                    onCancel=close
                />
            }
        )
    }

    let actResetEditorContent = () => {
        setState(resetEditorContent)
    }

    let rndError = (msgOpt,color) => {
        switch msgOpt {
            | None => <></>
            | Some(msg) => <pre style=ReactDOM.Style.make(~color, ~whiteSpace="pre-wrap", ())>{React.string(msg)}</pre>
        }
    }

    let rndMainMenu = () => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Nullable.toOption {
                | None => React.null
                | Some(mainMenuButtonRef) => {
                    <Menu
                        opn=true
                        anchorEl=mainMenuButtonRef
                        onClose=actCloseMainMenu
                    >
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                setShowTabs(!showTabs)
                            }}
                        >
                            {React.string(if (showTabs) {"Hide tabs"} else {"Show tabs"})}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                toggleCtxSelector.current->Nullable.toOption
                                    ->Belt.Option.forEach(toggleCtxSelector => toggleCtxSelector())
                            }}
                        >
                            {React.string(if ctxSelectorIsExpanded {"Hide context"} else {"Show context"})}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actRenameThisTab()
                            }}
                        >
                            {React.string("Rename this tab")}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actOpenViewOptionsDialog()
                            }}
                        >
                            {React.string("View options")}
                        </MenuItem>
                        <MenuItem 
                            onClick={() => {
                                actCloseMainMenu()
                                actExportToUrl()
                            }}
                        >
                            {"Export to URL"->React.string}
                        </MenuItem>
                        <MenuItem 
                            onClick={() => {
                                actCloseMainMenu()
                                actExportToJson()
                            }}
                        >
                            {"Export to JSON"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actOpenImportFromJsonDialog()
                            }}
                        >
                            {"Import from JSON ..."->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actResetEditorContent()
                            }}
                        >
                            {"Reset editor content"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actShowCompletedProof()
                            }}
                        >
                            {"Show completed proof"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actInlineTheorems()
                            }}
                            disabled={state.wrkCtx->Option.isNone}
                        >
                            {"Inline theorems"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actDeleteUnrelatedSteps(~deleteHyps=false)
                            }}
                        >
                            {"Delete unrelated steps"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actDeleteUnrelatedSteps(~deleteHyps=true)
                            }}
                        >
                            {"Delete unrelated steps and hypotheses"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actOpenMoveStepsDialog()
                            }}
                        >
                            {"Move steps"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actReorderSteps()
                            }}
                        >
                            {"Reorder steps"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actRenameHypotheses()
                            }}
                        >
                            {"Rename hypotheses"->React.string}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actRenumberSteps()
                            }}
                        >
                            {"Renumber steps"->React.string}
                        </MenuItem>
                    </Menu>
                }
            }
        } else {
            React.null
        }
    }

    let actToggleShowBkmOnly = () => {
        setShowBkmOnly(prev => !prev)
        setStatePriv(uncheckAllStmts)
    }

    let actOpenMacros = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                let closeDialog = ()=>closeModal(modalRef, modalId)
                <MM_cmp_macros
                    modalRef
                    onClose=closeDialog
                />
            })
        })->ignore
    }

    let rndButtons = () => {
        <Paper>
            <Row
                spacing = 0.
                childXsOffset = {idx => {
                    switch idx {
                        | 16 => Some(JSON.Encode.string("auto"))
                        | _ => None
                    }
                }}
            >
                <Checkbox
                    disabled=editIsActive
                    indeterminate={ mainCheckboxState->Belt_Option.isNone }
                    checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                    onChange={_ => actToggleMainCheckbox()}
                    style=?{
                        if (smallBtns) {Some(ReactDOM.Style.make(~padding="2px", ()))} else {None}
                    }
                />
                {rndIconButton(~icon=<MM_Icons.BookmarkAddOutlined/>, ~onClick=actBookmarkCheckedStmts, 
                    ~active= !editIsActive && atLeastOneStmtIsChecked && !allCheckedStmtsAreBookmarked,
                    ~title="Bookmark selected steps", ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.BookmarkRemoveOutlined/>, ~onClick=actUnbookmarkCheckedStmts, 
                    ~active= !editIsActive && atLeastOneStmtIsChecked && !allCheckedStmtsAreUnbookmarked,
                    ~title="Unbookmark selected steps", ~smallBtns, )}
                {rndIconButton(
                    ~icon=if (showBkmOnly){<MM_Icons.Bookmark/>}else{<MM_Icons.BookmarkBorder/>}, 
                    ~onClick=actToggleShowBkmOnly, ~active=true,
                    ~title="Show bookmarked steps only / show all steps", ~smallBtns)}
                {rndIconButton(~icon=<MM_Icons.ArrowDownward/>, ~onClick=actMoveCheckedStmtsDown, 
                    ~active= !editIsActive && canMoveCheckedStmts(state,false),
                    ~title="Move selected steps down", ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.ArrowUpward/>, ~onClick=actMoveCheckedStmtsUp, 
                    ~active= !editIsActive && canMoveCheckedStmts(state,true),
                    ~title="Move selected steps up", ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.Add/>, ~onClick=actAddNewStmt, ~active= !editIsActive,
                    ~title="Add new step (and place before selected steps if any)", ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.DeleteForever/>, ~onClick=actDeleteCheckedStmts,
                    ~active= !editIsActive && atLeastOneStmtIsChecked, ~title="Delete selected steps", ~smallBtns
                )}
                {rndIconButton(~icon=<MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />, 
                    ~onClick=()=>actDuplicateStmt(true), 
                    ~active= !editIsActive && isSingleStmtChecked(state), ~title="Duplicate selected step up", 
                    ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(+90deg)", ()) />, 
                    ~onClick=()=>actDuplicateStmt(false), 
                    ~active= !editIsActive && isSingleStmtChecked(state), ~title="Duplicate selected step down", 
                    ~smallBtns, )}
                {rndIconButton(~icon=<MM_Icons.Restore/>, 
                    ~active= !editIsActive, ~onClick=actOpenRestorePrevStateDialog,
                    ~title="Restore previous state", ~smallBtns)}
                {rndIconButton(~icon=<MM_Icons.MergeType style=ReactDOM.Style.make(~transform="rotate(180deg)", ())/>, 
                    ~onClick=actMergeStmts,
                    ~active= numOfCheckedStmts==1 || thereIsDuplicatedStmt, 
                    ~title="Merge two similar steps", ~smallBtns)}
                { 
                    rndIconButton(~icon=<MM_Icons.Search/>, ~onClick=actSearchAsrt,
                        ~active=generalModificationActionIsEnabled 
                            && state.preCtxData.frms->MM_substitution.frmsSize > 0,
                        ~title="Add new steps from existing assertions (and place before selected steps if any)", 
                        ~smallBtns
                    ) 
                }
                { rndIconButton(~icon=<MM_Icons.TextRotationNone/>, ~onClick=actSubstitute,
                    ~active=generalModificationActionIsEnabled,
                    ~title="Apply a substitution to all steps", ~smallBtns ) }
                { 
                    rndIconButton(~icon=<MM_Icons.Hub/>, ~onClick={() => actUnify(())},
                        ~active=generalModificationActionIsEnabled 
                                    && state.stmts->Array.length > 0, 
                        ~title="Unify all steps or unify selected provable bottom-up", ~smallBtns )
                }
                { 
                    rndIconButton(~icon=<MM_Icons.PlayArrow/>, ~onClick=actOpenMacros,
                        ~active=!editIsActive, 
                        ~title="Run a macro", ~smallBtns )
                }
                { 
                    rndIconButton(~icon=<MM_Icons.Menu/>, ~onClick=actOpenMainMenu, ~active={!editIsActive}, 
                        ~ref=ReactDOM.Ref.domRef(mainMenuButtonRef),
                        ~title="Additional actions", ~smallBtns )
                }
            </Row>
        </Paper>
    }

    let rndErrors = (stmt:userStmt):reElem => {
        if (stmt.stmtErr->Belt_Option.isSome 
            || stmt.syntaxErr->Belt_Option.isSome 
            || stmt.unifErr->Belt_Option.isSome) {
            <Col style=ReactDOM.Style.make(~marginLeft="10px", ())>
                {rndError(stmt.stmtErr->Belt.Option.map(err => err.msg),"red")}
                {
                    rndError(
                        stmt.syntaxErr->Belt.Option.map(msg => {
                            if (msg == "") {
                                "Syntax error."
                            } else {
                                `Syntax error - ${msg}.`
                            }
                        }),
                        "red"
                    )
                }
                {rndError(stmt.unifErr,"darkgrey")}
            </Col>
        } else {
            <></>
        }
    }

    let rndStmt = (stmt:userStmt):reElem => {
        let settings = state.preCtxData.settingsV.val
        <MM_cmp_user_stmt
            modalRef
            settingsVer=state.preCtxData.settingsV.ver
            settings
            preCtxVer=state.preCtxData.ctxV.ver
            varsText=state.varsText
            wrkCtx=state.wrkCtx
            frms=state.preCtxData.frms
            parenCnt=state.preCtxData.parenCnt
            syntaxTypes=state.preCtxData.syntaxTypes
            parensMap=state.preCtxData.parensMap
            stmt
            typeColors=state.preCtxData.typeColors
            preCtxColors=state.preCtxData.symColors
            wrkCtxColors=state.wrkCtxColors
            viewOptions
            readOnly=false
            parenAc
            toggleParenAc=actToggleParenAc
            editStmtsByLeftClick=settings.editStmtsByLeftClick
            longClickEnabled=settings.longClickEnabled
            longClickDelayMs=settings.longClickDelayMs
            defaultStmtType=settings.defaultStmtType
            showVisByDefault=settings.showVisByDefault

            onLabelEditRequested={() => actBeginEdit(setLabelEditMode,stmt.id)}
            onLabelEditDone={newLabel => actCompleteEditLabel(stmt.id,newLabel)}
            onLabelEditCancel={newLabel => actCancelEditLabel(stmt.id,newLabel)}

            onTypEditRequested={() => actBeginEdit(setTypEditMode,stmt.id)}
            onTypEditDone={(newTyp,newIsGoal) => actCompleteEdit(
                completeTypEditMode(_,stmt.id,newTyp,newIsGoal)
            )}

            onContEditRequested={() => actBeginEdit(setContEditMode,stmt.id)}
            onContEditDone={newContText => actCompleteEdit(completeContEditMode(_,stmt.id,newContText))}
            onContEditCancel={newContText => actCancelEditCont(stmt.id,newContText)}
            onSyntaxTreeUpdatedWithoutContentChange=
                {newStmtCont => actSyntaxTreeUpdatedWithoutContentChange(setStmtCont(_,stmt.id,newStmtCont))}
            
            onJstfEditRequested={() => actBeginEdit(setJstfEditMode,stmt.id)}
            onJstfEditDone={newJstf => actCompleteEdit(completeJstfEditMode(_,stmt.id,newJstf))}
            onJstfEditCancel={newJstf => actCancelEditJstf(stmt.id,newJstf)}

            checkboxDisabled=editIsActive
            checkboxChecked={state->isStmtChecked(stmt.id)}
            checkboxOnChange={(~checked as _, ~shift) => actToggleStmtChecked(~id=stmt.id, ~shift)}

            onGenerateProof={()=>actExportProof(stmt.id)}
            onDebug={() => actDebugUnifyAll(stmt.id)}
            onOpenSubstitutionDialog=Some(onOpenSubstitutionDialogRef)

            addStmtAbove=
                {text => actAddStmtAbove(~id=stmt.id, ~text, 
                    ~isBkm = stmt.isBkm || showBkmOnly && (stmt.typ == E || stmt.isGoal))}
            addStmtBelow=
                {text => actAddStmtBelow(~id=stmt.id, ~text, 
                    ~isBkm = stmt.isBkm || showBkmOnly && (stmt.typ == E || stmt.isGoal))}
            setShowTabs
            openFrameExplorer
        />
    }

    let rndStmtAndErrors = (stmt:userStmt) => {
        <Col key=stmt.id spacing=0.>
            {rndStmt(stmt)}
            {rndErrors(stmt)}
        </Col>
    }

    let rndDescr = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="12px", ())>
            <span onClick={_=>actBeginEdit0(setDescrEditMode)} style=ReactDOM.Style.make(~cursor="pointer", ())>
                {React.string("Description")}
            </span>
            <Col>
                <MM_cmp_multiline_text
                    text=state.descr
                    editMode=state.descrEditMode
                    editByClick=false
                    editByAltClick=true
                    longClickEnabled=state.preCtxData.settingsV.val.longClickEnabled
                    longClickDelayMs=state.preCtxData.settingsV.val.longClickDelayMs
                    onEditRequested={() => actBeginEdit0(setDescrEditMode)}
                    onEditDone=actDescrEditComplete
                    onEditCancel={newText => actCancelEditDescr(newText)}
                    renderer={str => {
                        <Static_XML_to_HTML xmlStr=str />
                    }}
                    fullWidth=true
                    onHelp={() => {
                        openModal(modalRef, () => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <XML_formatting_help onClose={_=>closeModal(modalRef, modalId)} />
                            })
                        })->ignore
                    }}
                    onDelete={() => {
                        actDescrEditComplete("")
                    }}
                />
            </Col>
        </Row>
    }

    let rndVars = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="3px", ())>
            <span onClick={_=>actBeginEdit0(setVarsEditMode)} style=ReactDOM.Style.make(~cursor="pointer", ())>
                {React.string("Variables")}
            </span>
            <Col>
                <MM_cmp_multiline_text
                    text=state.varsText
                    editMode=state.varsEditMode
                    editByClick=true
                    editByAltClick=true
                    longClickEnabled=state.preCtxData.settingsV.val.longClickEnabled
                    longClickDelayMs=state.preCtxData.settingsV.val.longClickDelayMs
                    onEditRequested={() => actBeginEdit0(setVarsEditMode)}
                    onEditDone=actVarsEditComplete
                    onEditCancel={newText => actCancelEditVars(newText)}
                    onDelete={() => {
                        actVarsEditComplete("")
                    }}
                />
                {rndError(state.varsErr,"red")}
            </Col>
        </Row>
    }

    let rndDisj = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ())>
            <span onClick={_=>actBeginEdit0(setDisjEditMode)} style=ReactDOM.Style.make(~cursor="pointer", ())>
                {React.string("Disjoints")}
            </span>
            <Col>
                <MM_cmp_multiline_text
                    text=state.disjText
                    editMode=state.disjEditMode
                    editByClick=true
                    editByAltClick=true
                    longClickEnabled=state.preCtxData.settingsV.val.longClickEnabled
                    longClickDelayMs=state.preCtxData.settingsV.val.longClickDelayMs
                    onEditRequested={() => actBeginEdit0(setDisjEditMode)}
                    onEditDone=actDisjEditComplete
                    onEditCancel={newText => actCancelEditDisj(newText)}
                    onDelete={() => {
                        actDisjEditComplete("")
                    }}
                />
                {rndError(state.disjErr,"red")}
            </Col>
        </Row>
    }

    let getPagesWithErrors = () => {
        let pageHasErrors = pageIdx => {
            let minIdx = pageIdx * stepsPerPage
            let maxIdx = minIdx + stepsPerPage - 1
            stmtsToShow
                ->Array.findIndexWithIndex((stmt,i) => minIdx <= i && i <= maxIdx && stmt->userStmtHasAnyErrors) >= 0
        }
        Belt_Array.range(minPageIdx, maxPageIdx)->Array.filter(pageHasErrors)
    }

    let actGoToNextPageWithErrors = () => {
        let pagesWithErrors = getPagesWithErrors()
        if (pagesWithErrors->Array.length > 0) {
            switch getPagesWithErrors()->Array.find(i => pageIdx < i) {
                | Some(i) => actGoToPage(i)
                | None => actGoToPage(pagesWithErrors->Array.getUnsafe(0))
            }
        }
    }

    let actGoToPrevPageWithErrors = () => {
        let pagesWithErrors = getPagesWithErrors()
        if (pagesWithErrors->Array.length > 0) {
            switch getPagesWithErrors()->Array.find(i => i < pageIdx) {
                | Some(i) => actGoToPage(i)
                | None => actGoToPage(pagesWithErrors->Array.getUnsafe(pagesWithErrors->Array.length-1))
            }
        }
    }

    let paginationIsRequired = stmtsToShow->Array.length > stepsPerPage

    let rndPagination = () => {
        if (paginationIsRequired) {
            <div style=ReactDOM.Style.make(~padding="5px", ())>
                <PaginationCmp
                    numOfPages
                    pageIdx
                    siblingCount=1000
                    showGoToPage=false
                    onPageIdxChange=actGoToPage
                    itemsPerPage=stepsPerPage
                    showItemsPerPage=false
                />
            </div>
        } else {
            React.null
        }
    }

    let rndGoToNextPageWithErrorsBtn = () => {
        if (thereAreAnyErrorsInEditor && paginationIsRequired) {
            <Row style=ReactDOM.Style.make(~padding="5px", ())>
                {
                    rndSmallTextBtn( 
                        ~text="< Go to previos page with errors", ~color="red", 
                        ~onClick=actGoToPrevPageWithErrors 
                    )
                }
                {
                    rndSmallTextBtn( 
                        ~text="Go to next page with errors >", ~color="red", 
                        ~onClick=actGoToNextPageWithErrors 
                    )
                }
            </Row>
        } else {
            React.null
        }
    }

    let rndStmts = () => {
        <Col spacing=0.>
            {rndPagination()}
            {rndGoToNextPageWithErrorsBtn()}
            { 
                stmtsToShow
                    ->Array.filterWithIndex((_,i) => stmtBeginIdx <= i && i <= stmtEndIdx)
                    ->Array.map(rndStmtAndErrors)->React.array 
            }
            {rndGoToNextPageWithErrorsBtn()}
            {rndPagination()}
        </Col>
    }

    let rndFooter = () => {
        Belt_Array.range(1,12)->Array.map(i => {
            <span key={i->Belt_Int.toString} style=ReactDOM.Style.make(~fontSize="20px", ())>{nbsp->React.string}</span>
        })->React.array
    }

    let rndShowContentBtn = () => {
        <Col spacing=1.0 style=ReactDOM.Style.make(~padding="20px", ())>
            {"Editor content is hidden."->React.string}
            <Button onClick={_=>{ setContIsHidden(_=>false) }} variant=#contained >
                {React.string("Show editor content")}
            </Button>
        </Col>
    }

    let actSetStateFromApi = (update:editorState=>result<(editorState,JSON.t),string>):promise<result<JSON.t,string>> => {
        promise(resolve => {
            setState(st => {
                switch update(st) {
                    | Ok((st,json)) => st->setNextAction(Some(Action(() => resolve(Ok(json)))))
                    | Error(msg) => st->setNextAction(Some(Action(() => resolve(Error(msg)))))
                }
            })
        })
    }

    let actBuildSyntaxTrees = (exprs:array<string>):result<array<result<MM_syntax_tree.syntaxTreeNode,string>>,string> => {
        switch state.wrkCtx {
            | None => Error(`There are errors in the editor.`)
            | Some(wrkCtx) => {
                let syms = exprs->Array.map(getSpaceSeparatedValuesAsArray)
                let unrecognizedSymbol = syms->Expln_utils_common.arrFlatMap(a => a)
                    ->Belt_HashSetString.fromArray
                    ->Belt_HashSetString.toArray
                    ->Array.find(sym => wrkCtx->ctxSymToInt(sym)->Belt_Option.isNone)
                switch unrecognizedSymbol {
                    | Some(sym) => Error(`Unrecognized symbol '${sym}'`)
                    | None => {
                        textToSyntaxTree( 
                            ~wrkCtx, ~syms, 
                            ~syntaxTypes=state.preCtxData.syntaxTypes, ~frms=state.preCtxData.frms, 
                            ~frameRestrict=state.preCtxData.settingsV.val.allowedFrms.inSyntax,
                            ~parenCnt=state.preCtxData.parenCnt,
                            ~lastSyntaxType=None,
                            ~onLastSyntaxTypeChange=_=>(),
                        )
                    }
                }
            }
        }
    }

    let actSetEditorContIsHidden = (contIsHidden:bool):promise<unit> => {
        setContIsHidden(_ => contIsHidden)
        promiseResolved(())
    }

    let getAsrtSyntaxTrees = ():promise<Belt_HashMapString.t<MM_syntax_tree.syntaxTreeNode>> => {
        switch preCtxData.asrtSyntaxTrees {
            | Some(asrtSyntaxTrees) => Promise.resolve(asrtSyntaxTrees)
            | None => {
                openModal(modalRef, () => rndProgress(~text="Building syntax trees for all assertions", ~pct=0.))
                    ->Promise.then(modalId => {
                        updateModal( 
                            modalRef, modalId, () => rndProgress(
                                ~text="Building syntax trees for all assertions", 
                                ~pct=0., ~onTerminate=makeActTerminate(modalId)
                            )
                        )
                        MM_wrk_syntax_tree.buildSyntaxTreesForAllAssertions(
                            ~settingsV=preCtxData.settingsV,
                            ~preCtxVer=preCtxData.ctxV.ver,
                            ~preCtx=preCtxData.ctxV.val.min,
                            ~onProgress = pct => updateModal(
                                modalRef, modalId, () => rndProgress(
                                    ~text="Building syntax trees for all assertions", 
                                    ~pct, ~onTerminate=makeActTerminate(modalId)
                                )
                            )
                        )->Promise.thenResolve(asrtSyntaxTreesArr => {
                            preCtxData.asrtSyntaxTrees = Some(Belt_HashMapString.fromArray(asrtSyntaxTreesArr))
                            closeModal(modalRef, modalId)
                            preCtxData.asrtSyntaxTrees->Option.getExn(~message="MM_cmp_editor.getAsrtSyntaxTrees.1")
                        })
                    })
            }
        }
    }

    MM_api_editor.updateEditorData(
        ~editorId,
        ~unifMetavarPrefix=preCtxData.settingsV.val.unifMetavarPrefix,
        ~state,
        ~setState=actSetStateFromApi,
        ~setEditorContIsHidden=actSetEditorContIsHidden,
        ~canStartProvingBottomUp=generalModificationActionIsEnabled,
        ~startProvingBottomUp = (params) => {
            promise(resolve => {
                actUnify(
                    ~stmtId=params.stmtId,
                    ~params=params.bottomUpProverParams,
                    ~initialDebugLevel=params.debugLevel,
                    ~isApiCall=true,
                    ~delayBeforeStartMs=params.delayBeforeStartMs,
                    ~selectFirstFoundProof=?params.selectFirstFoundProof,
                    ~bottomUpProofResultConsumer = stmtsDto => {
                        switch stmtsDto {
                            | None => resolve(Ok(false))
                            | Some(Error(msg)) => resolve(Error(msg))
                            | Some(Ok(_)) => resolve(Ok(true))
                        }
                    }
                )
            })
        },
        ~canStartUnifyAll=generalModificationActionIsEnabled,
        ~startUnifyAll = () => {
            promise(resolve => {
                setState(st => {
                    let st = st->uncheckAllStmts
                    let st = st->setNextAction(Some(UnifyAll({nextAction: () => resolve(())})))
                    st
                })
            })
        },
        ~buildSyntaxTrees=actBuildSyntaxTrees,
        ~getAsrtSyntaxTrees,
        ~addAsrtByLabel=actAddAsrtByLabel,
    )

    <Expln_React_ContentWithStickyHeader
        top
        header={rndButtons()}
        content={_ => {
            <Col spacing=0. >
                {rndMainMenu()}
                {
                    if (contIsHidden) {
                        rndShowContentBtn()
                    } else {
                        <Col spacing=0. >
                            {rndDescr()}
                            {rndVars()}
                            {rndDisj()}
                            {rndStmts()}
                            {rndFooter()}
                        </Col>
                    }
                }
            </Col>
        }}
    />
}
