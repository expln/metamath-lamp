open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_wrk_pre_ctx_data
open MM_wrk_editor_json
open Expln_React_Modal
open MM_cmp_user_stmt
open MM_react_common
open Local_storage_utils
open Expln_utils_promise

let defaultTestStmt = "|- ( ( a + b ) + ( c + d ) ) = 0"

let putSingleStatementToEditor = (st:editorState, stmt:string):editorState => {
    let st = st->completeDescrEditMode("")
    let st = st->completeVarsEditMode("")
    let st = st->completeDisjEditMode("")
    let st = st->checkAllStmts
    let st = st->deleteCheckedStmts
    let (st,stmtId) = st->addNewStmt
    let st = st->completeContEditMode(stmtId, stmt)
    st->verifyEditorState
}

let prepareTestEditorState = (~preCtxData:preCtxData, ~testStmt:string):editorState => {
    createInitialEditorState(~preCtxData, ~stateLocStor=None)->putSingleStatementToEditor(testStmt)
}

module CustomJsWarning = {
    let warningText = `
        Please be careful with what JavaScript code you use for the custom transforms. 
        This code will be executed by your browser "as is" meaning no safety measures will be taken to protect your browser from hurmful code.
        Before putting any code into this dialog please make sure you understand what that code does or make sure the code is not harmful.
    `
    @react.component
    let make = (
        ~hideInit:bool,
        ~onHideChange:bool=>unit,
        ~onClose:unit=>unit,
    ) => {
        let (hide, setHide) = React.useState(() => hideInit)

        let actHideChange = newHide => {
            setHide(_ => newHide)
            onHideChange(newHide)
        }

        <Paper style=ReactDOM.Style.make(~padding="10px", ())>
            <Col spacing=1.>
                <span style=ReactDOM.Style.make( ~fontWeight="bold", ~color="#FF7900", () ) >
                    {"Security warning"->React.string}
                </span>
                <table>
                    <tbody>
                        <tr>
                            <td>
                                <span style=ReactDOM.Style.make(~color="#FF7900", () ) >
                                    <MM_Icons.Warning/>
                                </span>
                            </td>
                            <td style=ReactDOM.Style.make(~padding="10px", () )>
                                {warningText->React.string}
                            </td>
                        </tr>
                    </tbody>
                </table>
                <Row alignItems=#center>
                    <Button onClick={_=>onClose()} variant=#contained >
                        {React.string("Ok")}
                    </Button>
                    <FormControlLabel
                        control={ <Checkbox checked=hide onChange=evt2bool(actHideChange) /> }
                        label="Don't show again"
                    />
                </Row>
            </Col>
        </Paper>
    }
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~preCtxData:preCtxData,
    ~readOnly:bool,
    ~title:string,
    ~transformsText:string,
    ~isCustom:bool,
    ~onSave:option<string=>unit>=?,
    ~onCancel:unit=>unit,
) => {
    let (testStmt, setTestStmt) = useStateFromLocalStorageStr(
        ~key="transform-editor-test-stmt", ~default=defaultTestStmt
    )
    let (editorState, setEditorState) = React.useState(() => {
        prepareTestEditorState(~preCtxData, ~testStmt)
    })
    let (parenAc, setParenAc) = React.useState(() => true)
    let (transformsText, setTransformsText) = React.useState(() => transformsText)
    let (hideWarning, setHideWarning) = useStateFromLocalStorageBool(
        ~key="custom-transforms-js-warning-hide", ~default=false
    )

    let actShowWarning = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <CustomJsWarning
                    hideInit=hideWarning
                    onHideChange = {b => setHideWarning(_ => b)}
                    onClose={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    React.useEffect0(() => {
        if (!readOnly && !hideWarning) {
            actShowWarning()
        }
        None
    })

    let actUpdateEditorState = (update:editorState=>editorState):unit => {
        setEditorState(st => {
            let st = st->update->verifyEditorState
            st.stmts->Belt_Array.get(0)->Belt.Option.forEach(stmt => {
                setTestStmt(_ => stmt.cont->contToStr)
            })
            st
        })
    }

    let testStmtText:option<string> = editorState.stmts->Belt_Array.get(0)->Belt.Option.map(stmt => stmt.cont->contToStr)
    React.useEffect1(() => {
        switch testStmtText {
            | None | Some("") => actUpdateEditorState(putSingleStatementToEditor(_, defaultTestStmt))
            | Some(_) => ()
        }
        None
    }, [testStmtText])

    let actSyntaxTreeUpdated = (stmtId, newStmtCont) => {
        actUpdateEditorState(setStmtCont(_, stmtId, newStmtCont))
    }

    let actTransformsTextUpdated = (newTransformsText) => {
        setTransformsText(_ => newTransformsText)
    }
    
    let rndTransformsText = () => {
        <table>
            <tbody>
                <tr>
                    {
                        if (readOnly) {
                            React.null
                        } else {
                            <td>
                                <IconButton title="Security warning" onClick={_=>actShowWarning()} color="orange" >
                                    <MM_Icons.Warning/>
                                </IconButton>
                            </td>
                        }
                    }
                    <td style=ReactDOM.Style.make(~paddingLeft="5px", () )>
                        <TextField
                            label=title
                            size=#small
                            style=ReactDOM.Style.make(~width="800px", ())
                            autoFocus=true
                            multiline=true
                            maxRows=10
                            value=transformsText
                            onChange=evt2str(actTransformsTextUpdated)
                            onKeyDown=kbrdHnd(~key=keyEsc, ~act=onCancel, ())
                            disabled=readOnly
                        />
                    </td>
                </tr>
            </tbody>
        </table>
    }

    let rndButtons = () => {
        <Row alignItems=#center style=ReactDOM.Style.make(~padding="4px", ())>
            {
                if (readOnly) {
                    React.null
                } else {
                    <Button onClick={_=>onSave->Belt_Option.forEach(onSave => onSave(transformsText))} variant=#contained > 
                        {React.string("Save")} 
                    </Button>
                }
            }
            <Button onClick={_=>onCancel()} > {React.string(if (readOnly) {"Close"} else {"Cancel"})} </Button>
        </Row>
    }

    let actToggleParenAc = () => {
        setParenAc(prev => !prev)
    }

    let rndStmt = (stmt:userStmt):reElem => {
        <MM_cmp_user_stmt
            modalRef
            settingsVer=editorState.settingsV
            settings={
                if (isCustom) {
                    {
                        ...editorState.settings,
                        customTransforms: transformsText,
                    }
                } else {
                    editorState.settings
                }
            }
            preCtxVer=editorState.preCtxV
            varsText=editorState.varsText
            wrkCtx=editorState.wrkCtx
            frms=editorState.frms
            parenCnt=editorState.parenCnt
            syntaxTypes=editorState.syntaxTypes
            parensMap=editorState.parensMap
            stmt
            typeColors=editorState.typeColors
            preCtxColors=editorState.preCtxColors
            wrkCtxColors=editorState.wrkCtxColors
            viewOptions={
                showCheckbox:false,
                showLabel:false,
                showType:false,
                showJstf:false,
                inlineMode:false,
                smallBtns:false,
            }
            readOnly=false
            parenAc
            toggleParenAc=actToggleParenAc
            editStmtsByLeftClick=editorState.settings.editStmtsByLeftClick
            longClickEnabled=editorState.settings.longClickEnabled
            longClickDelayMs=editorState.settings.longClickDelayMs
            defaultStmtType=editorState.settings.defaultStmtType
            showVisByDefault=editorState.settings.showVisByDefault

            onLabelEditRequested={() => ()}
            onLabelEditDone={_ => ()}
            onLabelEditCancel={_ => ()}

            onTypEditRequested={() => ()}
            onTypEditDone={(_,_) => ()}

            onContEditRequested={() => actUpdateEditorState(setContEditMode(_,stmt.id))}
            onContEditDone={newContText => actUpdateEditorState(completeContEditMode(_,stmt.id,newContText))}
            onContEditCancel={_ => ()}
            onSyntaxTreeUpdatedWithoutContentChange={newStmtCont => actSyntaxTreeUpdated(stmt.id,newStmtCont)}
            
            onJstfEditRequested={() => ()}
            onJstfEditDone={_ => ()}
            onJstfEditCancel={_ => ()}

            checkboxDisabled=true
            checkboxChecked={false}
            checkboxOnChange={_ => ()}

            onGenerateProof={()=>()}
            onDebug={() => ()}

            addStmtAbove={_ => ()}
            addStmtBelow={_ => ()}
            setShowTabs={_ => ()}
            openFrameExplorer={_ => ()}
        />
    }
    
    let rndError = (msgOpt,color) => {
        switch msgOpt {
            | None => <></>
            | Some(msg) => <pre style=ReactDOM.Style.make(~color, ())>{React.string(msg)}</pre>
        }
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

    let rndStmtAndErrors = (stmt:userStmt) => {
        <Col key=stmt.id spacing=0.>
            {rndStmt(stmt)}
            {rndErrors(stmt)}
        </Col>
    }

    let rndStmts = (editorState:editorState) => {
        <Col spacing=0.>
            { editorState.stmts->Js_array2.map(rndStmtAndErrors)->React.array }
        </Col>
    }

    let rndEditorState = () => {
        <Col spacing=0. style=ReactDOM.Style.make(~marginBottom="10px", ())>
            {rndStmts(editorState)}
        </Col>
    }

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            {React.string("Test statement:")}
            {rndEditorState()}
            {rndTransformsText()}
            {rndButtons()}
        </Col>
    </Paper>
}