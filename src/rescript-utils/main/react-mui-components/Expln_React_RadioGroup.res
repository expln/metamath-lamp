open Expln_React_common

@module("@mui/material/RadioGroup") @react.component
external make: (
    ~value:string=?,
    ~onChange:reFormHnd=?,
    ~row:bool=?,
    ~children:reElem=?
) => reElem = "default"