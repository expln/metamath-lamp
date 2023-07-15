open Expln_React_common
open Expln_React_Mui
open MM_editor_history
open MM_wrk_editor
open Expln_React_Modal
open MM_cmp_user_stmt

@react.component
let make = (
    ~modalRef:modalRef, 
    ~editorState:editorState,
    ~hist:editorHistory,
    ~onClose:unit=>unit,
    ~viewOptions:viewOptions,
    ~onRestore:int=>unit,
) => {
    let (curIdx, setCurIdx) = React.useState(() => if (hist->editorHistLength > 0) {0} else {-1})
    let (curEditorState, setCurEditorState) = React.useState(() => hist->editorHistGetSnapshotPreview(curIdx,editorState))

    let actChangeCurIdx = (newCurIdx:int) => {
        if (-1 <= newCurIdx && newCurIdx <= hist->editorHistLength - 1) {
            setCurIdx(_ => newCurIdx)
            setCurEditorState(_ => hist->editorHistGetSnapshotPreview(newCurIdx,editorState))
        }
    }

    let actPrev = () => actChangeCurIdx(curIdx+1)
    let actNext = () => actChangeCurIdx(curIdx-1)

    let updateCurEditorState = (update:editorState=>editorState):unit => {
        setCurEditorState(stRes => {
            switch stRes {
                | Error(_) => stRes
                | Ok(st) => Ok(update(st))
            }
        })
    }

    let actSyntaxTreeUpdated = (stmtId, newStmtCont) => {
        updateCurEditorState(setStmtCont(_, stmtId, newStmtCont))
    }

    let prevIsDisabled = curIdx == hist->editorHistLength - 1
    let nextIsDisabled = curIdx == -1
    let restoreThisIsDisabled = nextIsDisabled

    let rndButtons = () => {
        <Row alignItems=#center style=ReactDOM.Style.make(~padding="4px", ())>
            <Button onClick={_=>onRestore(curIdx)} disabled=restoreThisIsDisabled variant=#contained color="grey" > 
                {React.string("Restore this")} 
            </Button>
            <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            <Row spacing=0. alignItems=#center>
                <Button onClick={_=>actPrev()} disabled=prevIsDisabled > {React.string("< PREV")} </Button>
                <span> {(curIdx+1)->Belt_Int.toString->React.string} </span>
                <Button onClick={_=>actNext()} disabled=nextIsDisabled > {React.string("NEXT >")} </Button>
            </Row>
        </Row>
    }

    let rndMultilineText = (text:string, renderer:option<string=>reElem>) => {
        let style = if (text->Js.String2.trim == "") {
            ReactDOM.Style.make(~padding="4px", ())
        } else {
            ReactDOM.Style.make(~padding="0px", ())
        }
        <Paper variant=#outlined style >
            {
                if (text->Js.String2.trim == "" || renderer->Belt.Option.isNone) {
                    <pre>
                        {React.string(text)}
                    </pre>
                } else {
                    (renderer->Belt.Option.getExn)(text)
                }
            }
        </Paper>
    }

    let rndDescr = (editorState:editorState) => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="12px", ())>
            <span>
                {React.string("Description")}
            </span>
            {rndMultilineText(editorState.descr, Some(str => <Static_XML_to_HTML xmlStr=str />))}
        </Row>
    }

    let rndVars = (editorState:editorState) => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="3px", ())>
            <span >
                {React.string("Variables")}
            </span>
            {rndMultilineText(editorState.varsText, None)}
        </Row>
    }

    let rndDisj = (editorState:editorState) => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="3px", ())>
            <span >
                {React.string("Disjoints")}
            </span>
            {rndMultilineText(editorState.disjText, None)}
        </Row>
    }

    let rndStmt = (editorState:editorState, stmt:userStmt):reElem => {
        let state = editorState
        <MM_cmp_user_stmt
            modalRef
            settingsVer=state.settingsV
            preCtxVer=state.preCtxV
            varsText=state.varsText
            wrkCtx=state.wrkCtx
            frms=state.frms
            parenCnt=state.parenCnt
            syntaxTypes=state.syntaxTypes
            parensMap=state.parensMap
            stmt
            typeColors=state.typeColors
            preCtxColors=state.preCtxColors
            wrkCtxColors=state.wrkCtxColors
            viewOptions={
                {...viewOptions, showCheckbox:false}
            }
            readOnly=true
            editStmtsByLeftClick=state.settings.editStmtsByLeftClick
            longClickEnabled=state.settings.longClickEnabled
            longClickDelayMs=state.settings.longClickDelayMs
            defaultStmtType=state.settings.defaultStmtType
            showVisByDefault=state.settings.showVisByDefault

            onLabelEditRequested={() => ()}
            onLabelEditDone={_ => ()}
            onLabelEditCancel={_ => ()}

            onTypEditRequested={() => ()}
            onTypEditDone={(_,_) => ()}

            onContEditRequested={() => ()}
            onContEditDone={_ => ()}
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

    let rndStmts = (editorState:editorState) => {
        <Col spacing=0.>
            { editorState.stmts->Js_array2.map(rndStmt(editorState, _))->React.array }
        </Col>
    }

    let rndEditorState = () => {
        switch curEditorState {
            | Error(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string(`Error reading editor history: ${msg}`)}
                </pre>
            }
            | Ok(editorState) => {
                <Col spacing=0. >
                    {rndDescr(editorState)}
                    {rndVars(editorState)}
                    {rndDisj(editorState)}
                    {rndStmts(editorState)}
                </Col>
            }
        }
    }

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <AppBar position=#sticky color="white">
            {rndButtons()}
        </AppBar>
        {rndEditorState()}
    </Paper>
}