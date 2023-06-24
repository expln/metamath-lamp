open Expln_React_common

let rndProgress: (~text:string, ~pct:float=?, ~onTerminate:unit=>unit=?, ()) => reElem

let openInfoDialog: (
    ~modalRef: Expln_React_Modal.modalRef,
    ~text: string,
    ~onOk: unit => unit =?,
    ~title:string=?,
    unit,
) => unit

let copyToClipboard: string => unit
let getAvailWidth: () => int

type mouseButton = Left | Middle | Right
type clickCallback

let clickClbkMake: (
    ~btn:mouseButton=?,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => clickCallback

let clickHnd: (
    ~btn:mouseButton=?,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => (ReactEvent.Mouse.t => unit)

let clickHnd2: ( clickCallback, clickCallback, ) => (ReactEvent.Mouse.t => unit)

let keyCodeEnter: int
let keyCodeEsc: int

type kbrdCallback

let kbrdClbkMake: (
    ~keyCode:int,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => kbrdCallback

let kbrdHnd: (
    ~keyCode:int,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => (ReactEvent.Keyboard.t => unit)

let kbrdHnd2: ( kbrdCallback, kbrdCallback ) => (ReactEvent.Keyboard.t => unit)