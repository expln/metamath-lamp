open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open Expln_React_Modal
open MM_cmp_user_stmt

let prepareTestEditorState = (st:editorState):editorState => {
    let st = st->completeDescrEditMode("")
    let st = st->completeVarsEditMode("")
    let st = st->completeDisjEditMode("")
    let st = st->checkAllStmts
    let st = st->deleteCheckedStmts
    let (st,stmtId) = st->addNewStmt
    let st = st->completeContEditMode(stmtId,"write a test statement here")
    st->updateEditorStateWithPostupdateActions(st => st)
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~initEditorState:editorState,
    ~onClose:unit=>unit,
    ~viewOptions:viewOptions,
) => {
    let (editorState, setEditorState) = React.useState(() => prepareTestEditorState(initEditorState))
    let (parenAc, setParenAc) = React.useState(() => true)
    let (transformsText, setTransformsText) = React.useState(() => "")

    let updateEditorState = (update:editorState=>editorState):unit => {
        setEditorState(updateEditorStateWithPostupdateActions(_, update))
    }

    let actSyntaxTreeUpdated = (stmtId, newStmtCont) => {
        updateEditorState(setStmtCont(_, stmtId, newStmtCont))
    }

    let rndButtons = () => {
        <Row alignItems=#center style=ReactDOM.Style.make(~padding="4px", ())>
            <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
        </Row>
    }

    let actToggleParenAc = () => {
        setParenAc(prev => !prev)
    }

    let rndStmt = (stmt:userStmt):reElem => {
        <MM_cmp_user_stmt
            modalRef
            settingsVer=editorState.settingsV
            settings=editorState.settings
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
                {...viewOptions, showCheckbox:false}
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

            onContEditRequested={() => updateEditorState(setContEditMode(_,stmt.id))}
            onContEditDone={newContText => updateEditorState(completeContEditMode(_,stmt.id,newContText))}
            onContEditCancel={_ => ()}
            onSyntaxTreeUpdated={newStmtCont => actSyntaxTreeUpdated(stmt.id,newStmtCont)}
            
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
        <Col spacing=0. >
            {rndStmts(editorState)}
        </Col>
    }

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        {rndEditorState()}
        {rndButtons()}
    </Paper>
}