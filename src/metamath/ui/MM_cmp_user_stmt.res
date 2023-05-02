open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_react_common
open MM_context

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool=true, 
    ~title:option<string>=?, 
    ~color:option<string>=Some("primary"),
    ()
) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} ?color> icon </IconButton>
    </span>
}

type state = {
    newText: string,
    infoExpanded: bool,
}

let makeInitialState = () => {
    {
        newText: "",
        infoExpanded: false
    }
}

let setNewText = (st,text):state => {
    {
        ...st,
        newText:text
    }
}

let setInfoExpanded = (st,infoExpanded):state => {
    {
        ...st,
        infoExpanded
    }
}

let leftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk, ifNot: ReactEvent.Mouse.t => unit) => {
    if (mouseEvt->ReactEvent.Mouse.button == 0) {
        clbk()
    } else {
        ifNot(mouseEvt)
    }
}

let altLeftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk, ifNot: ReactEvent.Mouse.t => unit) => {
    if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.altKey) {
        clbk()
    } else {
        ifNot(mouseEvt)
    }
}

let rndContText = (stmtCont) => {
    switch stmtCont {
        | Text(arr) => {
            arr->Js.Array2.map(stmtSym => {
                <React.Fragment key={stmtSym.id}>
                    {
                        switch stmtSym.color {
                            | None => (stmtSym.sym ++ " ")->React.string
                            | Some(color) => {
                                <span style=ReactDOM.Style.make(~color=color, ~fontWeight="500", ())>
                                    {(stmtSym.sym ++ " ")->React.string}
                                </span>
                            }
                        }
                    }
                </React.Fragment>
            })->React.array
        }
        | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
    }
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->Js_string2.replaceByRe(symbolsNotAllowedInLabelRegex, "")

let rndProofStatus = (
    ~proofStatus:option<proofStatus>,
    ~readyTooltip:option<string>=?,
    ~waitingTooltip:option<string>=?,
    ~noJstfTooltip:option<string>=?,
    ~jstfIsIncorrectTooltip:option<string>=?,
    ~onReadyIconClicked:option<unit=>unit>=?,
    ~onErrorIconClicked:option<unit=>unit>=?,
    ~onNoJstfIconClicked:option<unit=>unit>=?,
    ()
):React.element => {
    switch proofStatus {
        | None => React.null
        | Some(status) => {
            switch status {
                | Ready =>
                    <span 
                        title=?readyTooltip
                        style=ReactDOM.Style.make(
                            ~color="green", ~fontWeight="bold", 
                            ~cursor=if (onReadyIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onReadyIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >{React.string("\u2713")}</span>
                | Waiting =>
                    <span 
                        title=?waitingTooltip
                        style=ReactDOM.Style.make(~color="orange", ~fontWeight="bold", ())
                    >
                        {React.string("\u223F")}
                    </span>
                | NoJstf =>
                    <span 
                        title=?noJstfTooltip
                        style=ReactDOM.Style.make(
                            ~color="red", ~fontWeight="bold", 
                            ~cursor=if (onNoJstfIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onNoJstfIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >
                        {React.string("?")}
                    </span>
                | JstfIsIncorrect =>
                    <span 
                        title=?jstfIsIncorrectTooltip
                        style=ReactDOM.Style.make(
                            ~color="red", ~fontWeight="bold", 
                            ~cursor=if (onErrorIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onErrorIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >
                        {React.string("\u2717")}
                    </span>
            }
        }
    }
}

module VisualizedJstf = {
    type props = {
        wrkCtx:option<mmContext>,
        stmt:userStmt,
        typeColors:Belt_HashMapString.t<string>,
        preCtxColors:Belt_HashMapString.t<string>,
        wrkCtxColors:Belt_HashMapString.t<string>,
    }
    let make = React.memoCustomCompareProps( @react.component (props:props) => {
        switch (props.wrkCtx, props.stmt.proofTreeDto, props.stmt.jstf) {
            | (Some(ctx), Some(proofTreeDto), Some(jstf)) => {
                switch props.stmt.src {
                    | None | Some(VarType) | Some(Hypothesis(_)) | Some(AssertionWithErr(_)) => React.null
                    | Some(Assertion({args, label})) => {
                        switch ctx->getFrame(label) {
                            | None => React.null
                            | Some(frame) => {
                                let asrt = ctx->frmIntsToSymsExn(frame, frame.asrt)
                                let hyps = []
                                let subs = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                let frmColors = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                frame.hyps->Js.Array2.forEachi((hyp,i) => {
                                    if (hyp.typ == E) {
                                        hyps->Js.Array2.push(ctx->frmIntsToSymsExn(frame, hyp.expr))->ignore
                                    } else {
                                        let frmSym = ctx->frmIntToSymExn(frame, hyp.expr[1])
                                        subs->Belt_HashMapString.set(
                                            frmSym,
                                            ctx->ctxIntsToSymsExn( 
                                                proofTreeDto.nodes[args[i]].expr->Js_array2.sliceFrom(1) 
                                            )
                                        )
                                        let typeSym = ctx->ctxIntToSymExn(hyp.expr[0])
                                        switch props.typeColors->Belt_HashMapString.get(typeSym) {
                                            | None => ()
                                            | Some(color) => frmColors->Belt_HashMapString.set( frmSym, color )
                                        }
                                    }
                                })
                                <MM_cmp_jstf_to_svg
                                    hyps
                                    hypLabels=jstf.args
                                    asrt
                                    frmColors=Some(frmColors)
                                    ctxColors1=Some(props.preCtxColors)
                                    ctxColors2=Some(props.wrkCtxColors)
                                    subs
                                />
                            }
                        }
                    }
                }
            }
            | _ => React.null
        }
    }, (_,_) => true )
}

@val external window: {..} = "window"

@react.component
let make = (
    ~wrkCtx:option<mmContext>,
    ~stmt:userStmt, 
    ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit, ~onLabelEditCancel:string=>unit,
    ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~onContEditRequested:unit=>unit, ~onContEditDone:string=>unit, ~onContEditCancel:string=>unit,
    ~onJstfEditRequested:unit=>unit, ~onJstfEditDone:string=>unit, ~onJstfEditCancel:string=>unit,
    ~onGenerateProof:unit=>unit,
    ~onDebug:unit=>unit,
    ~typeColors:Belt_HashMapString.t<string>,
    ~preCtxColors:Belt_HashMapString.t<string>,
    ~wrkCtxColors:Belt_HashMapString.t<string>,
    ~visualizationIsOn:bool,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())
    let labelRef = React.useRef(Js.Nullable.null)

    React.useEffect1(() => {
        if (stmt.labelEditMode) {
            setState(setNewText(_,stmt.label))
        } else if (stmt.typEditMode) {
            setState(setNewText(_,stmt.typ->userStmtTypeToStr))
        } else if (stmt.contEditMode) {
            setState(setNewText(_,contToStr(stmt.cont)))
        } else if (stmt.jstfEditMode) {
            setState(setNewText(_,stmt.jstfText))
        }
        None
    }, [stmt.labelEditMode, stmt.typEditMode, stmt.contEditMode, stmt.jstfEditMode])

    let actToggleInfoExpanded = () => {
        setState(st => setInfoExpanded(st, !st.infoExpanded))
    }

    let actExpandProof = expanded => {
        setState(st => setInfoExpanded(st, expanded))
    }

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }
    
    let actLabelEditDone = () => {
        onLabelEditDone(state.newText->removeSymbolsNotAllowedInLabel)
    }
    
    let actLabelEditCancel = () => {
        onLabelEditCancel(state.newText->removeSymbolsNotAllowedInLabel)
    }
    
    let actTypEditDone = newTypStr => {
        onTypEditDone(userStmtTypeFromStr(newTypStr))
    }
    
    let actContEditDone = () => {
        onContEditDone(state.newText->Js_string2.trim)
    }
    
    let actContEditCancel = () => {
        onContEditCancel(state.newText->Js_string2.trim)
    }
    
    let actJstfEditDone = () => {
        actExpandProof(true)
        onJstfEditDone(state.newText->Js_string2.trim)
    }
    
    let actJstfEditCancel = () => {
        onJstfEditCancel(state.newText->Js_string2.trim)
    }
    
    let actJstfDeleted = () => {
        onJstfEditDone("")
    }

    let rndLabel = () => {
        if (stmt.labelEditMode) {
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width="100px", ())
                    autoFocus=true
                    value=state.newText
                    onChange=evt2str(str => actNewTextUpdated(str->removeSymbolsNotAllowedInLabel))
                    onKeyDown=kbrdHnd(~onEnter=actLabelEditDone, ~onEsc=actLabelEditCancel, ())
                    title="Enter to save, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  
                    ~onClick=actLabelEditDone, ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,
                    ~onClick=actLabelEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            <span 
                ref=ReactDOM.Ref.domRef(labelRef)
                onClick=leftClickHnd(_, onLabelEditRequested, _ => ()) 
                title="<left-click> to change"
                style=ReactDOM.Style.make(~overflowWrap="normal", ~whiteSpace="nowrap", ())
            >
                {React.string(stmt.label)}
            </span>
        }
    }

    let rndCont = () => {
        if (stmt.contEditMode) {
            let windowWidth = window["innerWidth"]
            let labelWidth = switch labelRef.current->Js.Nullable.toOption {
                | None => 0
                | Some(domElem) => ReactDOM.domElementToObj(domElem)["offsetWidth"]
            }
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(
                        ~width = (windowWidth - 200 - labelWidth)->Belt_Int.toString ++ "px", 
                        ()
                    )
                    inputProps={
                        "style": {"fontFamily": "monospace"}
                    }
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd(~onEnter=actContEditDone, ~onEsc=actContEditCancel, ())
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  
                    ~onClick=actContEditDone, ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,  
                    ~onClick=actContEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            <Paper 
                onClick=leftClickHnd(_, onContEditRequested, _ => ()) 
                style=ReactDOM.Style.make(
                    ~padding="1px 10px", 
                    ~backgroundColor="rgb(255,255,235)", 
                    ~fontFamily="monospace",
                    ~fontSize="1.3em",
                    ()
                ) 
                title="<left-click> to change"
            >
                {rndContText(stmt.cont)}
            </Paper>
        }
    }

    let rndTyp = () => {
        if (stmt.typEditMode) {
            <FormControl size=#small >
                <Select
                    value=""
                    onChange=evt2str(actTypEditDone)
                >
                    <MenuItem value="e">{React.string("H")}</MenuItem>
                    <MenuItem value="p">{React.string("P")}</MenuItem>
                </Select>
            </FormControl>
        } else {
            let typStr = switch stmt.typ {
                | E => "H"
                | P => "P"
            }
            <span 
                onClick=altLeftClickHnd(_, onTypEditRequested, _ => actToggleInfoExpanded()) 
                style=ReactDOM.Style.make(~cursor="pointer", ~fontWeight="bold", ())
                title="Alt+<left-click> to change statement type. Left-click to show/hide the justification for provable."
            >
                {React.string(typStr->Js_string2.toUpperCase)}
            </span>
        }
    }

    let rndJstf = () => {
        if (stmt.jstfEditMode) {
            <Row>
                <TextField
                    size=#small
                    label="Justification"
                    style=ReactDOM.Style.make(~width="600px", ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd(~onEnter=actJstfEditDone, ~onEsc=actJstfEditCancel, ())
                    title="Enter to save, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actJstfEditDone,
                    ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,  
                    ~onClick=actJstfEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            let jstfText = if (stmt.jstfText == "") { " " } else { stmt.jstfText }
            let padding = if (jstfText->Js_string2.trim == "") { "10px 30px" } else { "3px" }
            <Row >
                <Paper 
                    onClick=leftClickHnd(_, onJstfEditRequested, _ => ()) 
                    style=ReactDOM.Style.make( ~padding, ~marginTop="5px", () )
                    title="<left-click> to change"
                >
                    {React.string(jstfText)}
                </Paper>
                {
                    if (jstfText->Js_string2.trim == "") {
                        React.null
                    } else {
                        <span>
                            {rndIconButton(~icon=<MM_Icons.DeleteForever/>,
                                ~onClick=actJstfDeleted, ~title="Clear", ~color=None, ())}
                        </span>
                    }
                }
            </Row>
        }
    }

    let rndJstfVisualization = () => {
        if (visualizationIsOn) {
                switch stmt.src {
                    | None => React.null
                    | Some(_) => {
                        <VisualizedJstf
                            wrkCtx
                            stmt
                            typeColors
                            preCtxColors
                            wrkCtxColors
                        />
                    }
                }
        } else {
            React.null
        }
    }

    let rndInfoBody = () => {
        if (stmt.typ == P) {
            if (state.infoExpanded || stmt.jstfEditMode) {
                <Col>
                    {rndJstf()}
                    {rndJstfVisualization()}
                </Col>
            } else {
                React.null
            }
        } else {
            React.null
        }
    }

    <table style=ReactDOM.Style.make(~margin="-2px", ())>
        <tbody>
            <tr style=ReactDOM.Style.make(~verticalAlign="top", ())>
                <td>
                    {
                        rndProofStatus(
                            ~proofStatus=stmt.proofStatus, 
                            ~readyTooltip="Proof is ready, left-click to generate compressed proof",
                            ~waitingTooltip="Justification for this statement is correct",
                            ~noJstfTooltip="Justification cannot be determined automatically. Click to debug.",
                            ~jstfIsIncorrectTooltip="Justification is incorrect. Click to debug.",
                            ~onReadyIconClicked=onGenerateProof,
                            ~onErrorIconClicked=onDebug,
                            ~onNoJstfIconClicked=onDebug,
                            ()
                        )
                    }
                </td>
                <td>
                    {rndLabel()}
                </td>
                <td>
                    {rndTyp()}
                </td>
                <td>
                    {rndCont()}
                </td>
            </tr>
            <tr>
                <td>
                    {React.null}
                </td>
                <td>
                    {React.null}
                </td>
                <td>
                    {React.null}
                </td>
                <td>
                    {rndInfoBody()}
                </td>
            </tr>
        </tbody>
    </table>
}