open Expln_React_common
open Expln_React_Mui
open MM_editor_history
open MM_wrk_editor
open Expln_React_Modal
open MM_cmp_user_stmt
open Local_storage_utils

type paginationData = {
    numOfPages:int,
    stepsPerPage:int,
    pageIdx:int,
    stmtsToShow:array<userStmt>,
    thereAreBkmSteps:bool,
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~editorState:editorState,
    ~hist:editorHistory,
    ~onClose:unit=>unit,
    ~viewOptions:viewOptions,
    ~stepsPerPage:int,
    ~onRestore:int=>unit,
) => {
    let (curIdx, setCurIdx) = React.useState(() => if (hist->editorHistLength > 0) {0} else {-1})
    let (curEditorState, setCurEditorState) = React.useState(() => hist->editorHistGetSnapshotPreview(curIdx,editorState))
    let (showBkmOnly, setShowBkmOnly) = useStateFromLocalStorageBool(
        ~key="editor-hist-showBkmOnly", ~default=false,
    )

    let (pageIdx, setPageIdx) = React.useState(() => 0)

    let paginationData:option<paginationData> = switch curEditorState {
        | Error(_) => None
        | Ok(editorState) => {
            let thereAreBkmSteps = editorState.stmts
                ->Array.some(stmt => stmt.typ != E && !stmt.isGoal && stmt.isBkm)
            
            let stmtsToShow = if (thereAreBkmSteps && showBkmOnly) {
                editorState.stmts->Array.filter(stmt => stmt.typ == E || stmt.isGoal || stmt.isBkm)
            } else {
                editorState.stmts
            }
            let numOfPages = (stmtsToShow->Array.length->Belt_Int.toFloat /. stepsPerPage->Belt.Int.toFloat)
                ->Math.ceil->Belt.Float.toInt
            let minPageIdx = 0
            let maxPageIdx = numOfPages - 1
            let pageIdx = Math.Int.max(minPageIdx, Math.Int.min(pageIdx, maxPageIdx))

            Some({ numOfPages, stepsPerPage, pageIdx, stmtsToShow, thereAreBkmSteps, })
        }
    }

    let actGoToPage = (pageIdx) => {
        setPageIdx(_ => pageIdx)
    }

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

    let actToggleShowBkmOnly = () => {
        setShowBkmOnly(prev => !prev)
    }

    let prevIsDisabled = curIdx == hist->editorHistLength - 1
    let nextIsDisabled = curIdx == -1
    let restoreThisIsDisabled = nextIsDisabled

    let rndButtons = () => {
        let thereAreBkmSteps = paginationData->Option.map(data => data.thereAreBkmSteps)->Option.getOr(false)
        <Row alignItems=#center style=ReactDOM.Style.make(~padding="4px", ())>
            <Button onClick={_=>onRestore(curIdx)} disabled=restoreThisIsDisabled variant=#contained color="grey" > 
                {React.string("Restore this")} 
            </Button>
            <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            <span title="Show bookmarked steps only / show all steps">
                <IconButton 
                    onClick={_ => actToggleShowBkmOnly()}
                    color="primary"
                    disabled={!thereAreBkmSteps}
                > 
                    {if (thereAreBkmSteps && showBkmOnly){<MM_Icons.Bookmark/>}else{<MM_Icons.BookmarkBorder/>}}
                </IconButton>
            </span>
            <Row spacing=0. alignItems=#center>
                <Button onClick={_=>actPrev()} disabled=prevIsDisabled > {React.string("< PREV")} </Button>
                <span> {(curIdx+1)->Belt_Int.toString->React.string} </span>
                <Button onClick={_=>actNext()} disabled=nextIsDisabled > {React.string("NEXT >")} </Button>
            </Row>
        </Row>
    }

    let rndMultilineText = (text:string, renderer:option<string=>reElem>) => {
        let style = if (text->String.trim == "") {
            ReactDOM.Style.make(~padding="4px", ())
        } else {
            ReactDOM.Style.make(~padding="0px", ())
        }
        <Paper variant=#outlined style >
            {
                if (text->String.trim == "" || renderer->Belt.Option.isNone) {
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
            viewOptions={
                {...viewOptions, showCheckbox:false}
            }
            readOnly=true
            parenAc=false
            toggleParenAc={()=>()}
            editStmtsByLeftClick=settings.editStmtsByLeftClick
            longClickEnabled=settings.longClickEnabled
            longClickDelayMs=settings.longClickDelayMs
            defaultStmtType=settings.defaultStmtType
            showVisByDefault=settings.showVisByDefault

            onLabelEditRequested={() => ()}
            onLabelEditDone={_ => ()}
            onLabelEditCancel={_ => ()}

            onTypEditRequested={() => ()}
            onTypEditDone={(_,_) => ()}

            onContEditRequested={() => ()}
            onContEditDone={_ => ()}
            onContEditCancel={_ => ()}
            onSyntaxTreeUpdatedWithoutContentChange={newStmtCont => actSyntaxTreeUpdated(stmt.id,newStmtCont)}
            
            onJstfEditRequested={() => ()}
            onJstfEditDone={_ => ()}
            onJstfEditCancel={_ => ()}

            checkboxDisabled=true
            checkboxChecked={false}
            checkboxOnChange={(~checked as _, ~shift as _) => ()}

            onGenerateProof={()=>()}
            onDebug={() => ()}
            onOpenSubstitutionDialog=None

            addStmtAbove={_ => ()}
            addStmtBelow={_ => ()}
            setShowTabs={_ => ()}
            openFrameExplorer={_ => ()}
        />
    }

    let rndPagination = () => {
        switch paginationData {
            | None => React.null
            | Some(paginationData) => {
                let paginationIsRequired = paginationData.stmtsToShow->Array.length > paginationData.stepsPerPage
                if (paginationIsRequired) {
                    <div style=ReactDOM.Style.make(~padding="5px", ())>
                        <PaginationCmp
                            numOfPages=paginationData.numOfPages
                            pageIdx=paginationData.pageIdx
                            siblingCount=1000
                            showGoToPage=false
                            onPageIdxChange=actGoToPage
                            itemsPerPage=paginationData.stepsPerPage
                            showItemsPerPage=false
                        />
                    </div>
                } else {
                    React.null
                }
            }
        }
    }

    let rndStmts = (editorState:editorState) => {
        switch paginationData {
            | None => React.null
            | Some(paginationData) => {
                let stmtBeginIdx = paginationData.pageIdx * paginationData.stepsPerPage
                let stmtEndIdx = stmtBeginIdx + paginationData.stepsPerPage - 1
                <Col spacing=0.>
                    { 
                        paginationData.stmtsToShow
                            ->Array.filterWithIndex((_,i) => stmtBeginIdx <= i && i <= stmtEndIdx)
                            ->Array.map(rndStmt(editorState, _))->React.array 
                    }
                </Col>
            }
        }
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
                    {rndPagination()}
                    {rndStmts(editorState)}
                    {rndPagination()}
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