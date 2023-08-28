open Expln_React_common

@module("@mui/material/ListItem") @react.component
external make: (
    ~disablePadding:bool=?,
    ~children: reElem=?,
) => reElem = "default"
