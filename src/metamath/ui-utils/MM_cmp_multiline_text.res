open Expln_React_common
open Expln_React_Mui

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
        newText:text
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool, ~title:option<string>=?, ()) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
    </span>
}

@react.component
let make = (
    ~text:string, 
    ~editMode:bool, ~onEditRequested:unit=>unit, ~onEditDone:string=>unit,
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
        onEditDone(state.newText)
    }

    let ctrlEnterHnd = (kbrdEvt, clbk) => {
        if (kbrdEvt->ReactEvent.Keyboard.ctrlKey && kbrdEvt->ReactEvent.Keyboard.keyCode == 13) {
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
                    onKeyDown=ctrlEnterHnd(_, actEditDone)
                    title="Ctrl+Enter to save"
                />
                {rndIconButton(~icon=<Icons2.Save/>, ~active=true,  ~onClick=actEditDone, ~title="Save, Ctrl+Enter", ())}
            </Row>
        } else {
            let style = if (text->Js.String2.trim == "") {
                ReactDOM.Style.make(~padding="4px", ())
            } else {
                ReactDOM.Style.make(~padding="0px", ())
            }
            <Paper 
                variant=#outlined 
                onClick=altLeftClickHnd(_, onEditRequested) 
                style 
                title="Alt+<left-click> to change"
            >
                <pre>
                    {React.string(text)}
                </pre>
            </Paper>
        }
    }

    rndText()
}