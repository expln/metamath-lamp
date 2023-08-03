open Expln_React_common

@module("@mui/material/MenuItem") @react.component
external make: (
    ~value:string=?,
    ~disabled:bool=?,
    ~onClick:unit=>unit=?,
    ~children:reElem=?
) => reElem = "default"