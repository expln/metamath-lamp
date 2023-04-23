open Expln_React_common
open Expln_React_Mui
open MM_react_common

type state = {
    newText: string,
}

let makeInitialState = () => {
    {
        newText: "",
    }
}

let setNewText = (st,text):state => {
    {
        ...st,
        newText:text
    }
}

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

@val external window: {..} = "window"

@react.component
let make = (
    ~text:string, 
    ~editMode:bool, 
    ~onEditRequested:unit=>unit, 
    ~onEditDone:string=>unit,
    ~onEditCancel:string=>unit,
    ~editByAltClick:bool=false,
    ~renderer:option<string=>reElem>=?,
    ~width:int=600,
    ~fullWidth:bool=false,
    ~buttonDirHor:bool=true,
    ~onHelp:option<unit=>unit>=?,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (editMode) {
            setState(setNewText(_,text))
        }
        None
    }, [editMode])

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }
    
    let actEditDone = () => {
        onEditDone(state.newText->Js_string2.trim)
    }
    
    let actEditCancel = () => {
        onEditCancel(state.newText->Js_string2.trim)
    }

    let leftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk) => {
        if (mouseEvt->ReactEvent.Mouse.button == 0) {
            clbk()
        }
    }

    let altLeftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk) => {
        if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.altKey) {
            clbk()
        }
    }

    let rndButtons = () => {
        let saveBtn = rndIconButton(
            ~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actEditDone, ~title="Save, Enter", ())
        let cancelBtn = rndIconButton(
            ~icon=<MM_Icons.CancelOutlined/>, ~onClick=actEditCancel, ~title="Cancel, Esc", ~color=None, ())
        let helpBtn = switch onHelp {
            | None => React.null
            | Some(onHelp) => {
                rndIconButton(
                    ~icon=<MM_Icons.HelpOutline/>, ~onClick=onHelp, ~title="Help", ~color=None, ())
            }
        }
        if (buttonDirHor) {
            <Row> saveBtn cancelBtn helpBtn </Row>
        } else {
            <Col> saveBtn cancelBtn helpBtn </Col>
        }
    }

    let rndText = () => {
        if (editMode) {
            let width = if (fullWidth) {
                let windowWidth = window["innerWidth"]
                windowWidth - 200
            } else {
                width
            }
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width = width->Belt_Int.toString ++ "px", ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd(~onEnter=actEditDone, ~onEsc=actEditCancel, ())
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                    minRows={if (buttonDirHor) {1} else if (onHelp->Belt.Option.isNone) {3} else {5} }
                />
                {rndButtons()}
            </Row>
        } else {
            let style = if (text->Js.String2.trim == "") {
                ReactDOM.Style.make(~padding="4px", ())
            } else {
                ReactDOM.Style.make(~padding="0px", ())
            }
            <Paper 
                variant=#outlined 
                onClick={
                    if (editByAltClick) {
                        altLeftClickHnd(_, onEditRequested)
                    } else {
                        leftClickHnd(_, onEditRequested)
                    }
                } 
                style 
                title={if (editByAltClick) {"Alt + <left-click> to change"} else {"<left-click> to change"}}
            >
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
    }

    rndText()
}