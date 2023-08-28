open Expln_React_common

@module("@mui/material/List") @react.component
external make: (
    ~disablePadding:bool=?,
    ~dense:bool=?,
    ~children: reElem=?,
) => reElem = "default"
