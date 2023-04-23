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

@react.component
let make = (
    ~text:string, 
    ~editMode:bool, 
    ~onEditRequested:unit=>unit, 
    ~onEditDone:string=>unit,
    ~onEditCancel:string=>unit,
    ~editByAltClick:bool=false,
    ~renderer:option<string=>reElem>=?,
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

    let rndText = () => {
        if (editMode) {
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width="600px", ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd(~onEnter=actEditDone, ~onEsc=actEditCancel, ())
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actEditDone, ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,
                    ~onClick=actEditCancel, ~title="Cancel, Esc", ~color=None, ())}
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