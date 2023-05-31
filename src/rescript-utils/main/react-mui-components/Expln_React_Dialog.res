open Expln_React_common

@module("./Dialog.js") @react.component
external make: (
    ~opn:bool=?,
    ~disableEscapeKeyDown:bool=?,
    ~fullScreen:bool=?,
    ~fullWidth:bool=?,
    ~maxWidth:string=?,
    ~onClose:unit=>unit=?,
    ~children: reElem,
) => reElem = "default"
