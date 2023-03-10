open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_ctx
open MM_wrk_unify
open MM_substitution
open Expln_utils_promise
open MM_react_common
open MM_parenCounter
open MM_statements_dto

type userStmtLocStor = {
    label: string,
    typ: string,
    cont: string,
    jstfText: string,
}   

type editorStateLocStor = {
    varsText: string,
    disjText: string,
    stmts: array<userStmtLocStor>,
}

let userStmtLocStorToUserStmt = (userStmtLocStor:userStmtLocStor):userStmt => {
    {
        id: "",

        label: userStmtLocStor.label,
        labelEditMode: false,
        typ: userStmtTypeFromStr(userStmtLocStor.typ),
        typEditMode: false,
        cont: strToCont(userStmtLocStor.cont, ()),
        contEditMode: false,

        jstfText: userStmtLocStor.jstfText,
        jstfEditMode: false,

        stmtErr: None,

        expr: None,
        jstf: None,
        proof: None,
        proofStatus: None,
    }
}

let unifyAllIsRequiredCnt = ref(0)

let createInitialEditorState = (settingsV, settings, preCtxV, preCtx, stateLocStor:option<editorStateLocStor>) => {
    let st = {
        settingsV,
        settings,
        typeColors: Belt_HashMapString.make(~hintSize=0),

        preCtxV,
        preCtx,
        frms: prepareFrmSubsData(preCtx),
        parenCnt: parenCntMake(prepareParenInts(preCtx, settings.parens), ()),
        preCtxColors: Belt_HashMapString.make(~hintSize=0),

        varsText: stateLocStor->Belt.Option.map(obj => obj.varsText)->Belt.Option.getWithDefault(""),
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: stateLocStor->Belt.Option.map(obj => obj.disjText)->Belt.Option.getWithDefault(""),
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: stateLocStor
            ->Belt.Option.map(stateLocStor => stateLocStor.stmts->Js_array2.length)
            ->Belt.Option.getWithDefault(0),
        stmts: 
            stateLocStor
                ->Belt.Option.map(obj => obj.stmts->Js_array2.mapi((stmtLocStor,i) => {
                    {
                        ...userStmtLocStorToUserStmt(stmtLocStor),
                        id: i->Belt_Int.toString
                    }
                }))
                ->Belt.Option.getWithDefault([]),
        checkedStmtIds: [],

        unifyAllIsRequiredCnt: unifyAllIsRequiredCnt.contents,
    }
    let st = recalcAllColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let editorStateToEditorStateLocStor = (state:editorState):editorStateLocStor => {
    {
        varsText: state.varsText,
        disjText: state.disjText,
        stmts: state.stmts->Js_array2.map(stmt => {
            {
                label: stmt.label,
                typ: (stmt.typ :> string),
                cont: contToStr(stmt.cont),
                jstfText: stmt.jstfText,
            }
        }),
    }
}

let editorSaveStateToLocStor = (state:editorState, key:string):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, Expln_utils_common.stringify(state->editorStateToEditorStateLocStor))
}

let readEditorStateFromLocStor = (key:string):option<editorStateLocStor> => {
    switch Dom_storage2.localStorage->Dom_storage2.getItem(key) {
        | None => None
        | Some(stateLocStorStr) => {
            open Expln_utils_jsonParse
            let parseResult = parseJson(stateLocStorStr, asObj(_, d=>{
                {
                    varsText: d->str("varsText", ~default=()=>"", ()),
                    disjText: d->str("disjText", ~default=()=>"", ()),
                    stmts: d->arr("stmts", asObj(_, d=>{
                        {
                            label: d->str("label", ()),
                            typ: d->str("typ", ()),
                            cont: d->str("cont", ()),
                            jstfText: d->str("jstfText", ())
                        }
                    }, ()), ())
                }
            }, ()), ())
            switch parseResult {
                | Error(_) => None
                | Ok(res) => Some(res)
            }
        }
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool, ~title:option<string>=?, ()) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
    </span>
}

let stateLocStorKey = "editor-state"

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

@react.component
let make = (~modalRef:modalRef, ~settingsV:int, ~settings:settings, ~preCtxV:int, ~preCtx:mmContext, ~top:int) => {
    let (state, setStatePriv) = React.useState(_ => createInitialEditorState(
        settingsV, settings, preCtxV, preCtx, readEditorStateFromLocStor(stateLocStorKey)
    ))

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
        | Some(stmt) if stmt.typ == #p => Some(stmt)
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

    let actPreCtxUpdated = (preCtxV, preCtx) => {
        setState(setPreCtx(_, preCtxV, preCtx))
    }

    React.useEffect1(() => {
        actPreCtxUpdated(preCtxV, preCtx)
        None
    }, [preCtxV])

    let actAddNewStmt = () => setState(st => {
        let (st, _) = addNewStmt(st)
        st
    })
    let actDeleteCheckedStmts = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        {React.string("Delete selected statements?")}
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

    let actBottomUpResultSelected = (selectedResult:newStmtsDto) => {
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
        if (state.checkedStmtIds->Js.Array2.length == 1) {
            switch state->editorGetStmtById(state.checkedStmtIds[0]) {
                | None => ()
                | Some(stmt1) => {
                    let contStr = stmt1.cont->contToStr
                    switch state.stmts->Js.Array2.find(stmt => stmt.id != stmt1.id && stmt.cont->contToStr == contStr) {
                        | None => {
                            openInfoDialog(
                                ~modalRef, 
                                ~text="Cannot find another statement to merge with.",
                                ()
                            )
                        }
                        | Some(stmt2) => {
                            if (stmt1.cont->contToStr != stmt2.cont->contToStr) {
                                openInfoDialog(
                                    ~modalRef, 
                                    ~text="Statements to merge must have identical expressions.",
                                    ()
                                )
                            } else {
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
                }
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
                            preCtxVer=state.preCtxV
                            preCtx=state.preCtx
                            parenStr=state.settings.parens
                            varsText=state.varsText
                            disjText=state.disjText
                            hyps={
                                state.stmts
                                    ->Js_array2.filter(stmt => stmt.typ == #e)
                                    ->Js_array2.map(stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
                            }
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

    let actUnify = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                let preCtxVer=state.preCtxV
                let preCtx=state.preCtx
                let parenStr=state.settings.parens
                let varsText=state.varsText
                let disjText=state.disjText
                let hyps={
                    state.stmts
                        ->Js_array2.filter(stmt => stmt.typ == #e)
                        ->Js_array2.map(stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
                }
                let stmts={state->getStmtsForUnification}
                switch singleProvableSelected {
                    | Some(singleProvableSelected) => {
                        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                            updateModal(modalRef, modalId, () => {
                                <MM_cmp_unify_bottom_up
                                    modalRef preCtxVer preCtx wrkCtx parenStr varsText disjText hyps stmts
                                    userStmtToProve=singleProvableSelected
                                    frms=state.frms parenCnt=state.parenCnt
                                    onResultSelected={newStmtsDto => {
                                        closeModal(modalRef, modalId)
                                        actBottomUpResultSelected(newStmtsDto)
                                    }}
                                    onCancel={() => closeModal(modalRef, modalId)}
                                    typeToPrefix = {
                                        Belt_MapString.fromArray(
                                            state.settings.typeSettings->Js_array2.map(ts => (ts.typ, ts.prefix))
                                        )
                                    }
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
                                ~preCtxVer, ~preCtx, ~parenStr, ~varsText, ~disjText, ~hyps, ~stmts,
                                ~bottomUpProverParams=None,
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
                actUnify()
            }
        }
        None
    }, [state.unifyAllIsRequiredCnt])

    let rndExportedProof = (proofStr, modalId) => {
        <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
            <Col>
                <pre style=ReactDOM.Style.make(~overflowWrap="break-word", ~whiteSpace="pre-wrap", ())>{React.string(proofStr)}</pre>
                <Button onClick={_=>closeModal(modalRef, modalId)}> {React.string("Close")} </Button>
            </Col>
        </Paper>
    }

    let actExportProof = (stmtId) => {
        switch generateCompressedProof(state, stmtId) {
            | None => ()
            | Some(proofText) => {
                openModal(modalRef, () => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        rndExportedProof(proofText, modalId)
                    })
                })->ignore
            }
        }
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }

    let rndButtons = () => {
        <Paper>
            <Row>
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
                    rndIconButton(~icon=<MM_Icons.Hub/>, ~onClick=actUnify,
                        ~active=generalModificationActionIsEnabled 
                                    && (!atLeastOneStmtIsSelected || singleProvableSelected->Belt.Option.isSome)
                                    && state.stmts->Js_array2.length > 0, 
                        ~title="Unify all statements or unify selected provable bottom-up", () )
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
                />
                {rndError(stmt.stmtErr)}
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
                {rndError(state.varsErr)}
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
                {rndError(state.disjErr)}
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
                {rndVars()}
                {rndDisj()}
                {rndStmts()}
            </Col>
        }}
    />
}