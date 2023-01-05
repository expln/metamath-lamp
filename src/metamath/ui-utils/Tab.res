open Expln_React_common

@module("@mui/material/Tab") @react.component
external make: (
    ~value:string=?,
    ~label: reElem=?,
) => reElem = "default"
