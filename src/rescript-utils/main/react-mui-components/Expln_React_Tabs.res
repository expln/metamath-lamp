open Expln_React_common

type variant = [ #scrollable | #fullWidth | #standard ]

@module("@mui/material/Tabs") @react.component
external make: (
    ~value:string=?,
    ~onChange:(ReactEvent.Mouse.t,string)=>unit=?,
    ~variant:variant=?,
    ~style:reStyle=?,
    ~children: reElem,
) => reElem = "default"
