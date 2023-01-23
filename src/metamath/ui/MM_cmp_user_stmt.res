open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool, ~title:option<string>=?, ()) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
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

@react.component
let make = (
    ~stmt:userStmt, 
    ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit,
    ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~onContEditRequested:unit=>unit, ~onContEditDone:string=>unit,
    ~onJstfEditRequested:unit=>unit, ~onJstfEditDone:string=>unit,
    ~onGenerateProof:unit=>unit,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (stmt.labelEditMode) {
            setState(setNewText(_,stmt.label))
        } else if (stmt.typEditMode) {
            setState(setNewText(_,stmt.typ :> string))
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
        onLabelEditDone(state.newText)
    }
    
    let actTypEditDone = newTypStr => {
        onTypEditDone(userStmtTypeFromStr(newTypStr))
    }
    
    let actContEditDone = () => {
        onContEditDone(state.newText)
    }
    
    let actJstfEditDone = () => {
        actExpandProof(true)
        onJstfEditDone(state.newText)
    }

    let ctrlEnterHnd = (kbrdEvt, clbk) => {
        if (kbrdEvt->ReactEvent.Keyboard.ctrlKey && kbrdEvt->ReactEvent.Keyboard.keyCode == 13) {
            clbk()
        }
    }

    let rndLabel = () => {
        if (stmt.labelEditMode) {
            <Row>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="100px", ())
                    autoFocus=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=ctrlEnterHnd(_, actLabelEditDone)
                    title="Ctrl+Enter to save"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  ~onClick=actLabelEditDone,
                    ~title="Save, Ctrl+Enter", ())}
            </Row>
        } else {
            <span 
                onClick=leftClickHnd(_, onLabelEditRequested, _ => ()) 
                title="<left-click> to change"
            >
                {React.string(stmt.label)}
            </span>
        }
    }

    let rndCont = () => {
        if (stmt.contEditMode) {
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width="600px", ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=ctrlEnterHnd(_, actContEditDone)
                    title="Ctrl+Enter to save"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  ~onClick=actContEditDone,
                    ~title="Save, Ctrl+Enter", ())}
            </Row>
        } else {
            <Paper 
                onClick=leftClickHnd(_, onContEditRequested, _ => ()) 
                style=ReactDOM.Style.make(~padding="1px 10px", ~backgroundColor="rgb(255,255,235)", ()) 
                title="<left-click> to change"
            >
            {
                switch stmt.cont {
                    | Text(arr) => React.string(arr->Js_array2.map(stmtSym => stmtSym.sym)->Js_array2.joinWith(" "))
                    | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
                }
            }
            </Paper>
        }
    }

    let rndProofStatus = () => {
        switch stmt.proofStatus {
            | None => React.null
            | Some(status) => {
                switch status {
                    | #ready => 
                        <span 
                            title="Proof is ready, left-click to generate compressed proof"
                            style=ReactDOM.Style.make(~color="green", ~fontWeight="bold", ~cursor="pointer", ())
                            onClick={_=>onGenerateProof()}
                        >{React.string("\u2713")}</span>
                    | #waiting => 
                        <span 
                            title="Justification for this statement is correct"
                            style=ReactDOM.Style.make(~color="orange", ~fontWeight="bold", ())
                        >
                            {React.string("\u223F")}
                        </span>
                    | #noJstf => 
                        <span 
                            title="Justification cannot be determined automatically"
                            style=ReactDOM.Style.make(~color="red", ~fontWeight="bold", ())
                        >
                            {React.string("?")}
                        </span>
                    | #jstfIsIncorrect => 
                        <span 
                            title="Justification is incorrect"
                            style=ReactDOM.Style.make(~color="red", ~fontWeight="bold", ())
                        >
                            {React.string("\u2717")}
                        </span>
                }
            }
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
                | #e => "H"
                | #p => "P"
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
                    onKeyDown=ctrlEnterHnd(_, actJstfEditDone)
                    title="Ctrl+Enter to save"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actJstfEditDone,
                    ~title="Save, Ctrl+Enter", ())}
            </Row>
        } else {
            <Paper 
                onClick=leftClickHnd(_, onJstfEditRequested, _ => ()) 
                style=ReactDOM.Style.make(~padding="3px", ())
                title="<left-click> to change"
            >
                {React.string("Justification: ")}
                {React.string(stmt.jstfText)}
            </Paper>
        }
    }

    let rndInfoBody = () => {
        if (stmt.typ == #p) {
            if (state.infoExpanded || stmt.jstfEditMode) {
                rndJstf()
            } else {
                React.null
            }
        } else {
            React.null
        }
    }

    <table>
        <tbody>
            <tr>
                <td>
                    {rndProofStatus()}
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