open Expln_React_common
open MM_context
open MM_wrk_settings

let rndProgress: (~text:string, ~pct:float=?, ~onTerminate:unit=>unit=?, ()) => reElem

let rndHiddenTextField: (~key:string=?, ~onKeyDown:reKeyboardHnd, ()) => reElem

let openInfoDialog: (
    ~modalRef: Expln_React_Modal.modalRef,
    ~text: string=?,
    ~content:React.element=?, 
    ~icon:React.element=?,
    ~onOk: unit => unit =?,
    ~title:string=?,
    unit,
) => unit

let rndSmallTextBtn: ( ~onClick:unit=>unit, ~text:string, ~color:string=?, ()) => React.element

let rndColorSelect: (
    ~availableColors:array<string>, 
    ~selectedColor:string, 
    ~onNewColorSelected:string=>unit,
    ~label:string=?,
    unit
) => React.element

let getFrmLabelBkgColor: (frame, settings) => option<string>

let copyToClipboard: string => promise<unit>
let readFromClipboard: () => promise<string>
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

let keyEnter: string
let keyEsc: string

type kbrdCallback

let kbrdClbkMake: (
    ~key:string,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => kbrdCallback

let kbrdHnd: (
    ~key:string,
    ~alt:bool=?,
    ~shift:bool=?,
    ~ctrl:bool=?,
    ~act:unit=>unit,
    ()
) => (ReactEvent.Keyboard.t => unit)

let kbrdHnd2: ( kbrdCallback, kbrdCallback ) => (ReactEvent.Keyboard.t => unit)
let kbrdHnd3: ( kbrdCallback, kbrdCallback, kbrdCallback ) => (ReactEvent.Keyboard.t => unit)
let kbrdHnds: array<kbrdCallback> => (ReactEvent.Keyboard.t => unit)
