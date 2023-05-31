open Expln_React_common

@module("./Menu.js") @react.component
external make: (
    ~opn:bool=?,
    ~anchorEl:Dom.element=?,
    ~onClose:unit=>unit=?,
    ~children:reElem=?
) => reElem = "default"