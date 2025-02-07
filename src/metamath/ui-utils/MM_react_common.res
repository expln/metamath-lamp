open Expln_React_Mui
open Expln_React_Modal
open Expln_React_common
open Expln_utils_promise
open MM_context
open MM_wrk_settings

@val external navigator: {..} = "navigator"
@val external window: {..} = "window"

let getAvailWidth = ():int => {
    window["screen"]["availWidth"]
}

let backupClipboard = ref("")

let copyToClipboard = (text:string):promise<unit> => {
    backupClipboard := text
    navigator["clipboard"]["writeText"](. text)
}

let readFromClipboard = ():promise<string> => {
    // Firefox doesn't support readText. Implement a workaround so we
    // can readFromClipboard (paste) from within this application.
    switch navigator["clipboard"]["readText"]->Nullable.toOption {
        | None => promise(resolve => resolve(backupClipboard.contents))
        | Some(_) => navigator["clipboard"]["readText"](.)
    }
}

let removeQueryParamsFromUrl = (msg:string):unit => {
    let location = window["location"]
    window["history"]["replaceState"](. msg, "", location["origin"] ++ location["pathname"])
}

type mouseButton = Left | Middle | Right

type clickCallback = {
    btn:mouseButton,
    alt:bool,
    shift:bool,
    ctrl:bool,
    act:unit=>unit,
}

let mouseButtonToInt = (btn:mouseButton):int => {
    switch btn {
        | Left => 0
        | Middle => 1
        | Right => 2
    }
}

let clickClbkMake = (
    ~btn:mouseButton=Left,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit
) => {
    { btn, alt, shift, ctrl, act, }
}

let clickHnd = (
    ~btn:mouseButton=Left,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit
):(ReactEvent.Mouse.t => unit) => {
    evt => {
        if (
            evt->ReactEvent.Mouse.button === btn->mouseButtonToInt
            && evt->ReactEvent.Mouse.altKey === alt
            && evt->ReactEvent.Mouse.ctrlKey === ctrl
            && evt->ReactEvent.Mouse.shiftKey === shift
        ) {
            act()
        }
    }
}

let runClickCallback = (evt:ReactEvent.Mouse.t, clbk:clickCallback):unit => {
    if (
        evt->ReactEvent.Mouse.button === clbk.btn->mouseButtonToInt
        && evt->ReactEvent.Mouse.altKey === clbk.alt
        && evt->ReactEvent.Mouse.ctrlKey === clbk.ctrl
        && evt->ReactEvent.Mouse.shiftKey === clbk.shift
    ) {
        clbk.act()
    }
}

let clickHnd2 = ( clbk1:clickCallback, clbk2:clickCallback, ):(ReactEvent.Mouse.t => unit) => {
    evt => {
        runClickCallback(evt,clbk1)
        runClickCallback(evt,clbk2)
    }
}

let keyEnter = "Enter"
let keyEsc = "Escape"

type kbrdCallback = {
    key:string,
    alt:bool,
    shift:bool,
    ctrl:bool,
    act:unit=>unit,
}

let kbrdClbkMake = (
    ~key:string,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit
) => {
    { key:key->String.toLowerCase, alt, shift, ctrl, act, }
}

let kbrdHnd = (
    ~key:string,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit
):(ReactEvent.Keyboard.t => unit) => {
    let key = key->String.toLowerCase
    evt => {
        if (
            evt->ReactEvent.Keyboard.key->String.toLowerCase === key
            && evt->ReactEvent.Keyboard.altKey === alt
            && evt->ReactEvent.Keyboard.ctrlKey === ctrl
            && evt->ReactEvent.Keyboard.shiftKey === shift
        ) {
            act()
            evt->ReactEvent.Keyboard.stopPropagation
            evt->ReactEvent.Keyboard.preventDefault
        }
    }
}

let runKbrdCallback = (evt:ReactEvent.Keyboard.t, clbk:kbrdCallback):unit => {
    if (
        evt->ReactEvent.Keyboard.key->String.toLowerCase === clbk.key
        && evt->ReactEvent.Keyboard.altKey === clbk.alt
        && evt->ReactEvent.Keyboard.ctrlKey === clbk.ctrl
        && evt->ReactEvent.Keyboard.shiftKey === clbk.shift
    ) {
        clbk.act()
        evt->ReactEvent.Keyboard.stopPropagation
        evt->ReactEvent.Keyboard.preventDefault
    }
}

let kbrdHnd2 = ( clbk1:kbrdCallback, clbk2:kbrdCallback, ):(ReactEvent.Keyboard.t => unit) => {
    evt => {
        runKbrdCallback(evt,clbk1)
        runKbrdCallback(evt,clbk2)
    }
}

let kbrdHnd3 = ( clbk1:kbrdCallback, clbk2:kbrdCallback, clbk3:kbrdCallback, ):(ReactEvent.Keyboard.t => unit) => {
    evt => {
        runKbrdCallback(evt,clbk1)
        runKbrdCallback(evt,clbk2)
        runKbrdCallback(evt,clbk3)
    }
}

let kbrdHnds = ( clbks:array<kbrdCallback>):(ReactEvent.Keyboard.t => unit) => {
    evt => clbks->Array.forEach(runKbrdCallback(evt,_))
}

let rndProgress = (~text:string, ~pct:option<float>=?, ~onTerminate:option<unit=>unit>=?) => {
    <Paper style=ReactDOM.Style.make(~padding=onTerminate->Belt.Option.map(_=>"5px")->Belt.Option.getWithDefault("10px"), ())>
        <Row alignItems=#center spacing=1.>
            <span style=ReactDOM.Style.make(~paddingLeft="10px", ())>
                {
                    switch pct {
                        | Some(pct) => `${text}: ${(pct *. 100.)->Math.round->Belt.Float.toInt->Belt_Int.toString}%`
                        | None => text
                    }->React.string
                }
            </span>
            {
                switch onTerminate {
                    | None => React.null
                    | Some(onTerminate) => {
                        <IconButton onClick={_ => onTerminate()}>
                            <MM_Icons.CancelOutlined/>
                        </IconButton>
                    }
                }
            }
        </Row>
    </Paper>
}

let rndHiddenTextField = (~key:option<string>=?, ~onKeyDown:reKeyboardHnd):reElem => {
    <TextField 
        key=?key
        size=#small
        style=ReactDOM.Style.make(~width="0px", ~height="0px", ~opacity="0", ())
        onKeyDown
        autoFocus=true
        autoComplete="off"
    />
}

let rndModalPaneWithTitle = (
    ~title:option<string>=?,
    ~content:React.element, 
) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <span 
                style=ReactDOM.Style.make(
                    ~fontWeight="bold", 
                    ~display=?{if (title->Belt_Option.isNone) {Some("none")} else {None}}, 
                    ()
                )
            >
                {title->Belt_Option.getWithDefault("")->String.padEnd(35, Common.nbsp)->React.string}
            </span>
            content
        </Col>
    </Paper>
}

let openModalPane = (
    ~modalRef:modalRef, 
    ~content:(~close:unit=>unit) => React.element,
) => {
    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
        updateModal(modalRef, modalId, () => content(~close=()=>closeModal(modalRef, modalId)))
    })->ignore
}

let openModalPaneWithTitle = (
    ~modalRef:modalRef, 
    ~title:option<string>=?,
    ~content:(~close:unit=>unit) => React.element,
) => {
    openModalPane(
        ~modalRef,
        ~content=(~close)=>rndModalPaneWithTitle(
            ~title?,
            ~content=content(~close),
        )
    )
}

let rndDialogContent = (
    ~text:option<string>, 
    ~content:option<React.element>, 
) => {
    switch content {
        | Some(content) => content
        | None => text->Belt_Option.getWithDefault("")->React.string
    }
}

let rndInfoDialog = (
    ~title:option<string>=?,
    ~icon:option<React.element>=?,
    ~content:option<React.element>=?, 
    ~text:option<string>=?, 
    ~onOk:unit=>unit,
    ~okBtnText:option<string>=?,
    ~onCancel:option<unit=>unit>=?,
    ~cancelBtnText:option<string>=?,
) => {
    rndModalPaneWithTitle(
        ~title?,
        ~content={
            <Col>
                {
                    switch icon {
                        | None => {
                            <span>
                                {rndDialogContent(~text, ~content)}
                            </span>
                        }
                        | Some(icon) => {
                            <table>
                                <tbody>
                                    <tr>
                                        <td>
                                            icon
                                        </td>
                                        <td style=ReactDOM.Style.make(~paddingLeft="5px", () )>
                                            {rndDialogContent(~text, ~content)}
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        }
                    }
                }
                <Row>
                    <Button onClick={_=>onOk()} variant=#contained >
                        {okBtnText->Option.getOr("Ok")->React.string}
                    </Button>
                    {
                        switch onCancel {
                            | None => React.null
                            | Some(onCancel) => {
                                <Button onClick={_=>onCancel()} variant=#outlined >
                                    {cancelBtnText->Option.getOr("Cancel")->React.string}
                                </Button>
                            }
                        }
                    }
                    {
                        rndHiddenTextField(
                            ~onKeyDown=kbrdHnd2(
                                kbrdClbkMake(~key=keyEnter, ~act=onOk),
                                kbrdClbkMake(~key=keyEsc, ~act=onCancel->Option.getOr(onOk)),
                            )
                        )
                    }
                </Row>
            </Col>
        }
    )
}

let openInfoDialog = (
    ~modalRef:modalRef, 
    ~title:option<string>=?,
    ~icon:option<React.element>=?,
    ~content:option<React.element>=?, 
    ~text:option<string>=?, 
    ~onOk:option<unit=>unit>=?, 
    ~okBtnText:option<string>=?,
    ~onCancel:option<unit=>unit>=?,
    ~cancelBtnText:option<string>=?,
) => {
    openModalPane(
        ~modalRef,
        ~content=(~close)=>rndInfoDialog(
            ~title?,
            ~icon?,
            ~content?, 
            ~text?, 
            ~onOk = () => {
                close()
                onOk->Option.forEach(clbk => clbk())
            },
            ~okBtnText?,
            ~onCancel = ?(onCancel->Option.map(onCancel => {
                () => {
                    close()
                    onCancel()
                }
            })),
            ~cancelBtnText?,
        )
    )
}

let openOkCancelDialog = (
    ~modalRef:modalRef, 
    ~title:option<string>=?,
    ~icon:option<React.element>=?,
    ~content:option<React.element>=?, 
    ~text:option<string>=?, 
    ~okBtnText:option<string>=?,
    ~cancelBtnText:option<string>=?,
):promise<bool> => {
    promise(resolve => {
        openInfoDialog(
            ~modalRef, 
            ~title?,
            ~icon?,
            ~content?, 
            ~text?, 
            ~onOk=()=>resolve(true), 
            ~okBtnText?,
            ~onCancel=()=>resolve(false),
            ~cancelBtnText?,
        )
    })
}

let rndSmallTextBtn = ( ~onClick:unit=>unit, ~text:string, ~color:string="grey" ):React.element => {
    <span
        onClick={_=> onClick() }
        style=ReactDOM.Style.make( 
            ~cursor="pointer", 
            ~color, 
            ~fontSize="0.7em", 
            ~padding="2px",
            ~borderRadius="3px",
            () 
        )
        className="grey-bkg-on-hover"
    >
        {React.string(text)}
    </span>
}

let rndColorSelect = (
    ~availableColors:array<string>, 
    ~selectedColor:string, 
    ~onNewColorSelected:string=>unit,
    ~label:option<string>=?
):React.element => {
    <FormControl size=#small >
        {
            switch label {
                | Some(label) => <InputLabel id="label-for-color-select">label</InputLabel>
                | None => React.null
            }
        }
        <Select 
            labelId="label-for-color-select"
            ?label
            value=selectedColor
            onChange=evt2str(onNewColorSelected)
        >
            {
                React.array(availableColors->Array.map(color => {
                    <MenuItem key=color value=color>
                        <div style=ReactDOM.Style.make(~width="50px", ~height="20px", ~backgroundColor=color, ()) />
                    </MenuItem>
                }))
            }
        </Select>
    </FormControl>
}

let getFrmLabelBkgColor = (frame:frame, settings:settings):option<string> => {
    if (frame.isDisc) {
        Some(settings.discColor)
    } else if (frame.isDepr) {
        Some(settings.deprColor)
    } else if (frame.isTranDepr) {
        Some(settings.tranDeprColor)
    } else {
        None
    }
}
