open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise

@val external navigator: {..} = "navigator"
@val external window: {..} = "window"

let getAvailWidth = ():int => {
    window["screen"]["availWidth"]
}

let backupClipboard = ref("")

let copyToClipboard = (text:string) => {
    backupClipboard := text
    navigator["clipboard"]["writeText"](. text)
}

let readFromClipboard = ():promise<string> => {
    // Firefox doesn't support readText. Implement a workaround so we
    // can readFromClipboard (paste) from within this application.
    switch navigator["clipboard"]["readText"]->Js.Nullable.toOption {
        | None => promise(resolve => resolve(backupClipboard.contents))
        | Some(_) => navigator["clipboard"]["readText"](.)
    }
}

let rndProgress = (~text:string, ~pct:option<float>=?, ~onTerminate:option<unit=>unit>=?, ()) => {
    <Paper style=ReactDOM.Style.make(~padding=onTerminate->Belt.Option.map(_=>"5px")->Belt.Option.getWithDefault("10px"), ())>
        <Row alignItems=#center spacing=1.>
            <span style=ReactDOM.Style.make(~paddingLeft="10px", ())>
                {
                    switch pct {
                        | Some(pct) => `${text}: ${(pct *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`
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

let rndInfoDialog = (~text:string, ~onOk:unit=>unit, ~title:option<string>=?, ()) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <span 
                style=ReactDOM.Style.make(
                    ~fontWeight="bold", 
                    ~display=?{if (title->Belt_Option.isNone) {Some("none")} else {None}}, 
                    ()
                )
            >
                {title->Belt_Option.getWithDefault("")->React.string}
            </span>
            <span>
                {text->React.string}
            </span>
            <Button onClick={_=>onOk()} variant=#contained >
                {React.string("Ok")}
            </Button>
        </Col>
    </Paper>
}

let openInfoDialog = (~modalRef:modalRef, ~text:string, ~onOk:option<unit=>unit>=?, ~title:option<string>=?, ()) => {
    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
        updateModal(modalRef, modalId, () => {
            rndInfoDialog(
                ~text, 
                ~onOk = () => {
                    closeModal(modalRef, modalId)
                    onOk->Belt_Option.forEach(clbk => clbk())
                },
                ~title?,
                ()
            )
        })
    })->ignore
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
    ~act:unit=>unit,
    ()
) => {
    { btn, alt, shift, ctrl, act, }
}

let clickHnd = (
    ~btn:mouseButton=Left,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit,
    ()
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

let keyCodeEnter = 13
let keyCodeEsc = 27

type kbrdCallback = {
    keyCode:int,
    alt:bool,
    shift:bool,
    ctrl:bool,
    act:unit=>unit,
}

let kbrdClbkMake = (
    ~keyCode:int,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit,
    ()
) => {
    { keyCode, alt, shift, ctrl, act, }
}

let kbrdHnd = (
    ~keyCode:int,
    ~alt:bool=false,
    ~shift:bool=false,
    ~ctrl:bool=false,
    ~act:unit=>unit,
    ()
):(ReactEvent.Keyboard.t => unit) => {
    evt => {
        if (
            evt->ReactEvent.Keyboard.keyCode === keyCode
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
        evt->ReactEvent.Keyboard.keyCode === clbk.keyCode
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
