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

let setNewText = (_, text):state => {
    {
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
    ~editByClick:bool=true,
    ~editByAltClick:bool=false,
    ~longClickEnabled:bool=false,
    ~longClickDelayMs:int=10_000,
    ~renderer:option<string=>reElem>=?,
    ~width:int=600,
    ~fullWidth:bool=false,
    ~buttonDirHor:bool=true,
    ~onHelp:option<unit=>unit>=?,
    ~onDelete:option<unit=>unit>=?,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    let {
        onClick, 
        onMouseDown, onMouseUp, onMouseMove, onMouseLeave, onMouseOut,
        onTouchStart, onTouchEnd, onTouchMove, onTouchCancel, 
    } = UseLongClick.useLongClick(
        ~onClick = Some(
            if (editByClick) {
                if (editByAltClick) {
                    clickHnd2(
                        clickClbkMake(~act=onEditRequested, ()),
                        clickClbkMake(~alt=true, ~act=onEditRequested, ()),
                    )
                } else {
                    clickHnd(~act=onEditRequested, ())
                }
            } else if (editByAltClick) {
                clickHnd(~alt=true, ~act=onEditRequested, ())
            } else {
                _ => ()
            }
        ),
        ~longClickEnabled = longClickEnabled && editByAltClick,
        ~longClickDelayMs,
        ~onShortClick=Some(clickAttrs => {
            switch clickAttrs {
                | Some({alt}) => {
                    if (editByClick && !alt || editByAltClick && alt) {
                        onEditRequested()
                    }
                }
                | _ => ()
            }
        }),
        ~onLongClick=Some(onEditRequested),
    )

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

    let rndButtons = () => {
        let saveBtn = rndIconButton(
            ~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actEditDone, ~title="Save, Enter", ())
        let cancelBtn = rndIconButton(
            ~icon=<MM_Icons.CancelOutlined/>, ~onClick=actEditCancel, ~title="Cancel, Esc", ~color=None, ())
        let helpBtn = switch onHelp {
            | None => React.null
            | Some(onHelp) => {
                rndIconButton(
                    ~icon=<MM_Icons.HelpOutline/>, ~onClick=onHelp, ~title="Help", ~color=None, ()
                )
            }
        }
        let deleteBtn = switch onDelete {
            | None => React.null
            | Some(onDelete) => {
                rndIconButton(
                    ~icon=<MM_Icons.DeleteForever/>, ~onClick=onDelete, ~title="Clear", ~color=None, ()
                )
            }
        }
        if (buttonDirHor) {
            <Row spacing=0.> saveBtn cancelBtn helpBtn deleteBtn </Row>
        } else {
            <Col spacing=0.> saveBtn cancelBtn helpBtn deleteBtn </Col>
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
                    onKeyDown=kbrdHnd2(
                        kbrdClbkMake(~keyCode=keyCodeEnter, ~act=actEditDone, ()),
                        kbrdClbkMake(~keyCode=keyCodeEsc, ~act=actEditCancel, ()),
                    )
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                    minRows={if (buttonDirHor) {1} else {6} }
                />
                {rndButtons()}
            </Row>
        } else {
            let style = if (text->Js.String2.trim == "") {
                ReactDOM.Style.make(~padding="4px", ())
            } else {
                ReactDOM.Style.make(~padding="0px", ())
            }
            let title = if (editByAltClick) {
                if (longClickEnabled) {
                    "<long-click> (Alt + <left-click>) to change"
                } else {
                    "Alt + <left-click> to change"
                }
            } else {
                "<left-click> to change"
            }
            <Paper
                variant=#outlined 
                onClick
                onMouseDown onMouseUp onMouseMove onMouseLeave onMouseOut
                onTouchStart onTouchEnd onTouchMove onTouchCancel 
                style 
                title
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