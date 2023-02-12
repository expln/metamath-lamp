open Expln_React_common

let rndProgress: (~text:string, ~pct:float=?, ~onTerminate:unit=>unit=?, ()) => reElem

let openInfoDialog: (
    ~modalRef: Expln_React_Modal.modalRef,
    ~text: string,
    ~onOk: unit => unit =?,
    unit,
) => unit

let kbrdHnd: (
    ~onCtrlEnter: unit => unit=?,
    ~onEnter: unit => unit=?,
    ~onEsc: unit => unit=?,
    unit,
    ReactEvent.Keyboard.t,
) => unit