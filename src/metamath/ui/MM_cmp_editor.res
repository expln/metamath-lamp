open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open MM_provers
open Local_storage_utils


let unifyAllIsRequiredCnt = ref(0)

let editorSaveStateToLocStor = (state:editorState, key:string):unit => {
    locStorWriteString(key, Expln_utils_common.stringify(state->editorStateToEditorStateLocStor))
}

let readEditorStateFromLocStor = (key:string):option<editorStateLocStor> => {
    switch locStorReadString(key) {
        | None => None
        | Some(stateLocStorStr) => 
            readEditorStateFromJsonStr(stateLocStorStr)->Belt.Result.mapWithDefault(None, state => Some(state))
    }
}

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool, 
    ~ref:option<ReactDOM.domRef>=?,
    ~title:option<string>=?, 
    ()
) => {
    <span ?ref ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
    </span>
}

let stateLocStorKey = "editor-state"
let appendTimestampLocStorKey = "export-to-json-append-timestamp"

let lastUsedAsrtSearchTypLocStorKey = "search-asrt-typ"

let saveLastUsedTyp = (ctx,typInt) => {
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

@val external navigator: {..} = "navigator"

@react.component
let make = (
    ~modalRef:modalRef, 
    ~settingsV:int, 
    ~settings:settings, 
    ~srcs:array<mmCtxSrcDto>, 
    ~preCtxV:int, 
    ~preCtx:mmContext, 
    ~top:int,
    ~reloadCtx: React.ref<Js.Nullable.t<array<mmCtxSrcDto> => promise<result<unit,string>>>>,
) => {
    let (jsonExportAppendTimestamp, setJsonExportAppendTimestampPriv) = React.useState(_ => {
        locStorReadBool(appendTimestampLocStorKey)->Belt_Option.getWithDefault(false)
    })

    let setJsonExportAppendTimestamp = (appendTimestamp:bool):unit => {
        locStorWriteBool(appendTimestampLocStorKey, appendTimestamp)
        setJsonExportAppendTimestampPriv(_ => appendTimestamp)
    }

    let (state, setStatePriv) = React.useState(_ => createInitialEditorState(
        ~settingsV, ~settings, ~preCtxV, ~preCtx, ~stateLocStor=readEditorStateFromLocStor(stateLocStorKey)
    ))

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Js.Nullable.null)

    let setState = (update:editorState=>editorState) => {
        setStatePriv(st => {
            let st = updateEditorStateWithPostupdateActions(st, update)
            editorSaveStateToLocStor(st, stateLocStorKey)
            st
        })
    }

    let editIsActive = 
        state.varsEditMode || state.disjEditMode ||
        state.stmts->Js.Array2.some(stmt => stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode )

    let thereAreSyntaxErrors = editorStateHasErrors(state)
    let mainCheckboxState = {
        let atLeastOneStmtIsChecked = state.checkedStmtIds->Js.Array2.length != 0
        let atLeastOneStmtIsNotChecked = state.stmts->Js.Array2.length != state.checkedStmtIds->Js.Array2.length
        if (atLeastOneStmtIsChecked && atLeastOneStmtIsNotChecked) {
            None
        } else if (atLeastOneStmtIsChecked && !atLeastOneStmtIsNotChecked) {
            Some(true)
        } else {
            Some(false)
        }
    }

    let generalModificationActionIsEnabled = !editIsActive && !thereAreSyntaxErrors
    let atLeastOneStmtIsSelected = mainCheckboxState->Belt_Option.getWithDefault(true)
    let singleProvableSelected = switch getTheOnlySelectedStmt(state) {
        | Some(stmt) if stmt.typ == P => Some(stmt)
        | _ => None
    }
    let oneStatementIsSelected = state.checkedStmtIds->Js.Array2.length == 1

    let actSettingsUpdated = (settingsV, settings) => {
        setState(setSettings(_, settingsV, settings))
    }

    React.useEffect1(() => {
        actSettingsUpdated(settingsV, settings)
        None
    }, [settingsV])

    let actPreCtxUpdated = (srcs, preCtxV, preCtx) => {
        setState(setPreCtx(_, srcs, preCtxV, preCtx))
    }

    React.useEffect1(() => {
        actPreCtxUpdated(srcs, preCtxV, preCtx)
        None
    }, [preCtxV])

    let actOpenMainMenu = () => {
        setMainMenuIsOpened(_ => true)
    }

    let actCloseMainMenu = () => {
        setMainMenuIsOpened(_ => false)
    }

    let actAddNewStmt = () => setState(st => {
        let (st, _) = addNewStmt(st)
        st
    })
    let actDeleteCheckedStmts = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        {
                            let numOfSelectedStmts = state.checkedStmtIds->Js.Array2.length
                            if (numOfSelectedStmts == state.stmts->Js.Array2.length) {
                                React.string("Delete all statements?")
                            } else if (numOfSelectedStmts == 1) {
                                React.string("Delete the selected statement?")
                            } else {
                                React.string(`Delete ${state.checkedStmtIds->Js.Array2.length->Belt.Int.toString}` 
                                                ++ ` selected statements?`)
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
    let actToggleStmtChecked = id => {
        setStatePriv(st => {
            let st = toggleStmtChecked(st,id)
            editorSaveStateToLocStor(st, stateLocStorKey)
            st
        })
    }
    let actToggleMainCheckbox = () => {
        let action = switch mainCheckboxState {
            | Some(true) | None => uncheckAllStmts
            | Some(false) => checkAllStmts
        }
        setStatePriv(st => {
            let st = action(st)
            editorSaveStateToLocStor(st, stateLocStorKey)
            st
        })
    }
    let actMoveCheckedStmtsUp = () => setState(moveCheckedStmts(_, true))
    let actMoveCheckedStmtsDown = () => setState(moveCheckedStmts(_, false))
    let actDuplicateStmt = () => setState(st => {
        let st = duplicateCheckedStmt(st)
        let st = uncheckAllStmts(st)
        st
    })
    let actBeginEdit0 = (setter:editorState=>editorState) => {
        if (!editIsActive) {
            setState(setter)
        }
    }
    let actBeginEdit = (setter:(editorState,string)=>editorState, stmtId:string) => {
        if (!editIsActive) {
            setState(setter(_,stmtId))
        }
    }
    let actCompleteEdit = (setter:editorState=>editorState) => {
        setState(setter)
    }

    let actCompleteEditLabel = (stmtId, newLabel):unit => {
        let newLabel = newLabel->Js_string2.trim
        switch state->renameStmt(stmtId, newLabel) {
            | Error(msg) => openInfoDialog( ~modalRef, ~text=`Cannot rename this statement: ${msg}`, () )
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
                let contNew = newContText->strToCont(())
                let textOld = contOld->contToStr
                let textNew = contNew->contToStr
                if (textOld == textNew) {
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
                                    contOld={MM_cmp_user_stmt.rndContText(contOld)}
                                    contNew={MM_cmp_user_stmt.rndContText(contNew)}
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
                let textOld = stmt.jstfText->Js_string2.trim
                let textNew = newJstfText->Js_string2.trim
                if (textOld == textNew || textNew == "") {
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

    let actCancelEditVars = (newText):unit => {
        let textOld = state.varsText->Js_string2.trim
        let textNew = newText->Js_string2.trim
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
        let textOld = state.disjText->Js_string2.trim
        let textNew = newText->Js_string2.trim
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

    let actAsrtSearchResultsSelected = selectedResults => {
        setState(st => selectedResults->Js_array2.reduce( addNewStatements, st ))
    }

    let actBottomUpResultSelected = (selectedResult:stmtsDto) => {
        setState(st => {
            let st = st->addNewStatements(selectedResult)
            let st = st->uncheckAllStmts
            let st = st->incUnifyAllIsRequiredCnt
            st
        })
    }

    let actWrkSubsSelected = wrkSubs => {
        setState(st => st->applySubstitutionForEditor(wrkSubs))
    }

    let actOnMergeStmtsSelected = (stmtToUse:userStmt,stmtToRemove:userStmt) => {
        setState(st => {
            switch st->mergeStmts(stmtToUse.id, stmtToRemove.id) {
                | Ok(st) => st->uncheckAllStmts
                | Error(msg) => {
                    openInfoDialog(~modalRef, ~text=msg, ())
                    st
                }
            }
        })
    }

    let actMergeTwoStmts = () => {
        switch state->findStmtsToMerge {
            | Error(msg) => {
                openInfoDialog(
                    ~modalRef, 
                    ~text=msg,
                    ()
                )
            }
            | Ok((stmt1,stmt2)) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_merge_two_stmts
                            stmt1
                            stmt2
                            onStmtSelected={(stmtToUse,stmtToRemove)=>{
                                closeModal(modalRef, modalId)
                                actOnMergeStmtsSelected(stmtToUse,stmtToRemove)
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
                            settingsVer=state.settingsV
                            settings=state.settings
                            preCtxVer=state.preCtxV
                            preCtx=state.preCtx
                            varsText=state.varsText
                            disjText=state.disjText
                            wrkCtx
                            frms=state.frms
                            initialTyp={getLastUsedTyp(state.preCtx)}
                            onTypChange={saveLastUsedTyp(state.preCtx, _)}
                            onCanceled={()=>closeModal(modalRef, modalId)}
                            onResultsSelected={selectedResults=>{
                                closeModal(modalRef, modalId)
                                actAsrtSearchResultsSelected(selectedResults)
                            }}
                        />
                    })
                })->ignore
            }
        }
    }

    let actSubstitute = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                let expr1Init = if (state.checkedStmtIds->Js.Array2.length >= 1) {
                    let id = state.checkedStmtIds[0]
                    state.stmts->Js_array2.find(stmt => stmt.id == id)->Belt_Option.map(stmt => stmt.cont->contToStr)
                } else {
                    None
                }
                let expr2Init = if (state.checkedStmtIds->Js.Array2.length >= 2) {
                    let id = state.checkedStmtIds[1]
                    state.stmts->Js_array2.find(stmt => stmt.id == id)->Belt_Option.map(stmt => stmt.cont->contToStr)
                } else {
                    None
                }
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

    let actUnifyAllResultsAreReady = proofTreeDto => {
        setStatePriv(st => {
            let st = applyUnifyAllResults(st, proofTreeDto)
            editorSaveStateToLocStor(st, stateLocStorKey)
            st
        })
    }

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let prepareArgs0 = (argLabels:array<string>, rootStmts:array<rootStmt>):array<expr> => {
        rootStmts
            ->Js_array2.filter(stmt => argLabels->Js_array2.includes(stmt.label))
            ->Js_array2.map(stmt => stmt.expr)
    }

    let getArgs0AndAsrtLabel = (jstfText:string, rootStmts:array<rootStmt>):option<(array<expr>,string)> => {
        switch jstfText->parseJstf {
            | Error(_) | Ok(None) => None
            | Ok(Some({args:argLabels, label})) => Some((prepareArgs0(argLabels, rootStmts), label))
        }
    }

    let actUnify = (
        ~stmtId:option<stmtId>=?,
        ~params:option<bottomUpProverParams>=?,
        ~initialDebugLevel:option<int>=?,
        ()
    ) => {
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
                let rootStmts = rootUserStmts->Js.Array2.map(userStmtToRootStmt)
                let singleProvableSelected = switch state->getTheOnlySelectedStmt {
                    | Some(stmt) if stmt.typ == P => Some(stmt)
                    | _ => None
                }
                switch singleProvableSelected {
                    | Some(singleProvableSelected) => {
                        let initialParams = switch params {
                            | Some(_) => params
                            | None => {
                                switch getArgs0AndAsrtLabel(singleProvableSelected.jstfText, rootStmts) {
                                    | None => None
                                    | Some((args0,asrtLabel)) => Some(bottomUpProverParamsMake(~asrtLabel, ~args0, ()))
                                }
                            }
                        }
                        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <MM_cmp_unify_bottom_up
                                    modalRef
                                    settingsVer=state.settingsV
                                    settings=state.settings
                                    preCtxVer=state.preCtxV
                                    preCtx=state.preCtx
                                    frms=state.frms parenCnt=state.parenCnt
                                    varsText disjText wrkCtx
                                    rootStmts=rootUserStmts
                                    reservedLabels={state.stmts->Js_array2.map(stmt => stmt.label)}
                                    typeToPrefix = {
                                        Belt_MapString.fromArray(
                                            state.settings.typeSettings->Js_array2.map(ts => (ts.typ, ts.prefix))
                                        )
                                    }
                                    initialParams=?initialParams
                                    initialDebugLevel=?initialDebugLevel
                                    onResultSelected={newStmtsDto => {
                                        closeModal(modalRef, modalId)
                                        actBottomUpResultSelected(newStmtsDto)
                                    }}
                                    onCancel={() => closeModal(modalRef, modalId)}
                                />
                            })
                        })->ignore
                    }
                    | None => {
                        openModal(modalRef, () => rndProgress(~text="Unifying all", ~pct=0., ()))->promiseMap(modalId => {
                            updateModal( 
                                modalRef, modalId, () => rndProgress(
                                    ~text="Unifying all", ~pct=0., ~onTerminate=makeActTerminate(modalId), ()
                                )
                            )
                            unify(
                                ~settingsVer=state.settingsV,
                                ~settings=state.settings,
                                ~preCtxVer=state.preCtxV,
                                ~preCtx=state.preCtx,
                                ~varsText,
                                ~disjText,
                                ~rootStmts={rootUserStmts->Js_array2.map(userStmtToRootStmt)},
                                ~bottomUpProverParams=None,
                                ~debugLevel=0,
                                ~onProgress = msg => updateModal(
                                    modalRef, modalId, () => rndProgress(
                                        ~text=msg, ~onTerminate=makeActTerminate(modalId), ()
                                    )
                                )
                            )->promiseMap(proofTreeDto => {
                                closeModal(modalRef, modalId)
                                actUnifyAllResultsAreReady(proofTreeDto)
                            })
                        })->ignore
                    }
                }
            }
        }
    }

    React.useEffect1(() => {
        if (unifyAllIsRequiredCnt.contents < state.unifyAllIsRequiredCnt) {
            unifyAllIsRequiredCnt.contents = state.unifyAllIsRequiredCnt
            if (!editorStateHasErrors(state)) {
                actUnify(())
            }
        }
        None
    }, [state.unifyAllIsRequiredCnt])

    let rndExportedProof = (modalId, proofText:string, proofTableWithTypes:string, proofTableWithoutTypes:string) => {
        <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
            <Col spacing=1.>
                <pre style=ReactDOM.Style.make(~overflowWrap="break-word", ~whiteSpace="pre-wrap", ())>
                    {React.string(proofText)}
                </pre>
                <pre>
                    {React.string(proofTableWithTypes)}
                </pre>
                <pre>
                    {React.string(proofTableWithoutTypes)}
                </pre>
                <Button onClick={_=>closeModal(modalRef, modalId)}> {React.string("Close")} </Button>
            </Col>
        </Paper>
    }

    let actExportProof = (stmtId) => {
        switch generateCompressedProof(state, stmtId) {
            | None => ()
            | Some((proofText,proofTableWithTypes,proofTableWithoutTypes)) => {
                openModal(modalRef, () => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        rndExportedProof(modalId, proofText, proofTableWithTypes, proofTableWithoutTypes)
                    })
                })->ignore
            }
        }
    }

    let copyToClipboard = (text:string) => {
        navigator["clipboard"]["writeText"](. text)
    }

    let rec rndExportedJson = (modalId,json:string,appendTimestamp:bool):unit => {
        updateModal(modalRef, modalId, () => {
            let timestampStr = if (appendTimestamp) {
                Common.currTimeStr() ++ " "
            } else {
                ""
            }
            let textToShow = timestampStr ++ json
            <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
                <Col>
                    <Row spacing=1.>
                        <FormControlLabel
                            control={
                                <Checkbox
                                    checked=appendTimestamp
                                    onChange={evt2bool(appendTimestamp => {
                                        setJsonExportAppendTimestamp(appendTimestamp)
                                        rndExportedJson(modalId,json,appendTimestamp)
                                    })}
                                />
                            }
                            label="append timestamp"
                        />
                        <Button onClick={_=>copyToClipboard(textToShow)} variant=#contained > {React.string("Copy")} </Button>
                        <Button onClick={_=>closeModal(modalRef, modalId)} > {React.string("Close")} </Button>
                    </Row>
                    <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(textToShow)}</pre>
                </Col>
            </Paper>
        })
    }

    let actExportToJson = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            rndExportedJson(
                modalId,
                Expln_utils_common.stringify(state->editorStateToEditorStateLocStor),
                jsonExportAppendTimestamp
            )
        })->ignore
    }

    let rndSrcDtos = (srcs:array<mmCtxSrcDto>):reElem => {
        <Col>
        {
            srcs->Js_array2.mapi((src,i) => {
                <Paper key={i->Belt.Int.toString} style=ReactDOM.Style.make(~padding="3px", ())>
                    <Col>
                        {src.url->React.string}
                        {
                            let readInstr = src.readInstr->readInstrFromStr
                            if (readInstr == ReadAll) {
                                `read all`->React.string
                            } else {
                                let readInstrStr = if (readInstr == StopBefore) {"stop before"} else {"stop after"}
                                `${readInstrStr}: ${src.label}`->React.string
                            }
                        }
                    </Col>
                </Paper>
            })->React.array
        }
        </Col>
    }

    let actImportFromJson = (jsonStr):bool => {
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
                setState(_ => createInitialEditorState(
                    ~settingsV, ~settings, ~preCtxV, ~preCtx, ~stateLocStor=Some(stateLocStor)
                ))
                reloadCtx.current->Js.Nullable.toOption
                    ->Belt.Option.map(reloadCtx => {
                        reloadCtx(stateLocStor.srcs)->promiseMap(res => {
                            switch res {
                                | Ok(_) => ()
                                | Error(msg) => {
                                    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                                        updateModal(modalRef, modalId, () => {
                                            <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                                                <Col spacing=1.>
                                                    <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                                                        { React.string(`Could not realod the context because of the error:`) }
                                                    </span>
                                                    <span style=ReactDOM.Style.make(~color="red", ())>
                                                        { React.string(msg) }
                                                    </span>
                                                    <span>
                                                        { React.string(`This error happened when loading the context:`) }
                                                    </span>
                                                    {rndSrcDtos(stateLocStor.srcs)}
                                                    <Button onClick={_ => closeModal(modalRef, modalId) } variant=#contained> 
                                                        {React.string("Ok")} 
                                                    </Button>
                                                </Col>
                                            </Paper>
                                        })
                                    })->ignore
                                }
                            }
                        })
                    })->ignore
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

    let actDebugUnifyAll = (stmtId) => {
        let st = state
        let st = st->uncheckAllStmts
        let st = st->toggleStmtChecked(stmtId)
        let provableSelected = switch getTheOnlySelectedStmt(st) {
            | Some(stmt) if stmt.typ == P => Some(stmt)
            | _ => None
        }
        switch provableSelected {
            | None => ()
            | Some(provableSelected) => {
                let rootUserStmts = st->getRootStmtsForUnification
                let rootStmts = rootUserStmts->Js.Array2.map(userStmtToRootStmt)
                let params = switch getArgs0AndAsrtLabel(provableSelected.jstfText, rootStmts) {
                    | Some((args0,asrtLabel)) => {
                        bottomUpProverParamsMake(
                            ~args0, 
                            ~args1=[],
                            ~asrtLabel, 
                            ~maxSearchDepth=1,
                            ~lengthRestrict=Less,
                            ~allowNewDisjForExistingVars=false,
                            ~allowNewStmts=false,
                            ~allowNewVars=false,
                            ~maxNumberOfBranches=?None,
                            ()
                        )
                    }
                    | None => {
                        bottomUpProverParamsMake(
                            ~args0=rootStmts->Js_array2.map(stmt => stmt.expr), 
                            ~args1=[],
                            ~asrtLabel=?None, 
                            ~maxSearchDepth=1,
                            ~lengthRestrict=Less,
                            ~allowNewDisjForExistingVars=false,
                            ~allowNewStmts=false,
                            ~allowNewVars=false,
                            ~maxNumberOfBranches=?None,
                            ()
                        )
                    }
                }
                actUnify(
                    ~stmtId, 
                    ~params, 
                    ~initialDebugLevel = if (params.asrtLabel->Belt_Option.isSome) {2} else {1}, 
                    ()
                )
            }
        }
    }

    let rndError = (msgOpt,color) => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color, ())>{React.string(msg)}</pre>
        }
    }

    let rndMainMenu = () => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Js.Nullable.toOption {
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
                    </Menu>
                }
            }
        } else {
            React.null
        }
    }

    let rndButtons = () => {
        <Paper>
            <Row
                childXsOffset = {idx => {
                    switch idx {
                        | 10 => Some(Js.Json.string("auto"))
                        | _ => None
                    }
                }}
            >
                <Checkbox
                    disabled=editIsActive
                    indeterminate={mainCheckboxState->Belt_Option.isNone}
                    checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                    onChange={_ => actToggleMainCheckbox()}
                />
                {rndIconButton(~icon=<MM_Icons.ArrowDownward/>, ~onClick=actMoveCheckedStmtsDown, ~active= !editIsActive && canMoveCheckedStmts(state,false),
                    ~title="Move selected statements down", ())}
                {rndIconButton(~icon=<MM_Icons.ArrowUpward/>, ~onClick=actMoveCheckedStmtsUp, ~active= !editIsActive && canMoveCheckedStmts(state,true),
                    ~title="Move selected statements up", ())}
                {rndIconButton(~icon=<MM_Icons.Add/>, ~onClick=actAddNewStmt, ~active= !editIsActive,
                    ~title="Add new statement (and place before selected statements if any)", ())}
                {rndIconButton(~icon=<MM_Icons.DeleteForever/>, ~onClick=actDeleteCheckedStmts,
                    ~active= !editIsActive && atLeastOneStmtIsSelected, ~title="Delete selected statements", ()
                )}
                {rndIconButton(~icon=<MM_Icons.ControlPointDuplicate/>, ~onClick=actDuplicateStmt, 
                    ~active= !editIsActive && isSingleStmtChecked(state), ~title="Duplicate selected statement", ())}
                {rndIconButton(~icon=<MM_Icons.MergeType style=ReactDOM.Style.make(~transform="rotate(180deg)", ())/>, 
                    ~onClick=actMergeTwoStmts,
                    ~active=oneStatementIsSelected, ~title="Merge two similar statements", ())}
                { 
                    rndIconButton(~icon=<MM_Icons.Search/>, ~onClick=actSearchAsrt,
                        ~active=generalModificationActionIsEnabled && state.frms->Belt_MapString.size > 0,
                        ~title="Add new statements from existing assertions (and place before selected statements if any)", ()
                    ) 
                }
                { rndIconButton(~icon=<MM_Icons.TextRotationNone/>, ~onClick=actSubstitute, 
                    ~active=generalModificationActionIsEnabled && state.checkedStmtIds->Js.Array2.length <= 2,
                    ~title="Apply a substitution to all statements", () ) }
                { 
                    rndIconButton(~icon=<MM_Icons.Hub/>, ~onClick={() => actUnify(())},
                        ~active=generalModificationActionIsEnabled 
                                    && (!atLeastOneStmtIsSelected || singleProvableSelected->Belt.Option.isSome)
                                    && state.stmts->Js_array2.length > 0, 
                        ~title="Unify all statements or unify selected provable bottom-up", () )
                }
                { 
                    rndIconButton(~icon=<MM_Icons.Menu/>, ~onClick=actOpenMainMenu, ~active={!editIsActive}, 
                        ~ref=ReactDOM.Ref.domRef(mainMenuButtonRef),
                        ~title="Additional actions", () )
                }
            </Row>
        </Paper>
    }

    let rndStmt = (stmt:userStmt) => {
        <tr key=stmt.id >
            <td style=ReactDOM.Style.make(~verticalAlign="top", ())>
                <Checkbox
                    disabled=editIsActive
                    checked={state->isStmtChecked(stmt.id)}
                    onChange={_ => actToggleStmtChecked(stmt.id)}
                />
            </td>
            <td>
                <MM_cmp_user_stmt
                    stmt

                    onLabelEditRequested={() => actBeginEdit(setLabelEditMode,stmt.id)}
                    onLabelEditDone={newLabel => actCompleteEditLabel(stmt.id,newLabel)}
                    onLabelEditCancel={newLabel => actCancelEditLabel(stmt.id,newLabel)}

                    onTypEditRequested={() => actBeginEdit(setTypEditMode,stmt.id)}
                    onTypEditDone={newTyp => actCompleteEdit(completeTypEditMode(_,stmt.id,newTyp))}

                    onContEditRequested={() => actBeginEdit(setContEditMode,stmt.id)}
                    onContEditDone={newContText => actCompleteEdit(completeContEditMode(_,stmt.id,newContText))}
                    onContEditCancel={newContText => actCancelEditCont(stmt.id,newContText)}
                    
                    onJstfEditRequested={() => actBeginEdit(setJstfEditMode,stmt.id)}
                    onJstfEditDone={newJstf => actCompleteEdit(completeJstfEditMode(_,stmt.id,newJstf))}
                    onJstfEditCancel={newJstf => actCancelEditJstf(stmt.id,newJstf)}

                    onGenerateProof={()=>actExportProof(stmt.id)}
                    onDebug={()=>actDebugUnifyAll(stmt.id)}
                />
                {rndError(stmt.stmtErr,"red")}
                {rndError(stmt.unifErr,"darkgrey")}
            </td>
        </tr>
    }

    let rndVars = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Variables")}
            <Col>
                <MM_cmp_multiline_text
                    text=state.varsText
                    editMode=state.varsEditMode
                    onEditRequested={() => actBeginEdit0(setVarsEditMode)}
                    onEditDone={newText => actCompleteEdit(completeVarsEditMode(_,newText))}
                    onEditCancel={newText => actCancelEditVars(newText)}
                />
                {rndError(state.varsErr,"red")}
            </Col>
        </Row>
    }

    let rndDisj = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Disjoints")}
            <Col>
                <MM_cmp_multiline_text
                    text=state.disjText
                    editMode=state.disjEditMode
                    onEditRequested={() => actBeginEdit0(setDisjEditMode)}
                    onEditDone={newText => actCompleteEdit(completeDisjEditMode(_,newText))}
                    onEditCancel={newText => actCancelEditDisj(newText)}
                />
                {rndError(state.disjErr,"red")}
            </Col>
        </Row>
    }

    let rndStmts = () => {
        <table>
            <tbody>
                { state.stmts->Js_array2.map(rndStmt)->React.array }
            </tbody>
        </table>
    }

    <Expln_React_ContentWithStickyHeader
        top
        header={rndButtons()}
        content={_ => {
            <Col>
                {rndMainMenu()}
                {rndVars()}
                {rndDisj()}
                {rndStmts()}
            </Col>
        }}
    />
}