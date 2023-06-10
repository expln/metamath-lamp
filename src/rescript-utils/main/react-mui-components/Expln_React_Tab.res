open Expln_React_common

@module("@mui/material/Tab") @react.component
external make: (
    ~value:string=?,
    ~style:reStyle=?,
    ~label: reElem=?,
) => reElem = "default"
