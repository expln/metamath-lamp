open Expln_React_common

@module("./Select2.js") @react.component
external make: (
    ~labelId:string=?,
    ~label:string=?,
    ~value:string,
    ~onChange:reFormHnd=?,
    ~onClose:unit=>unit=?,
    ~children:reElem=?
) => reElem = "default"
