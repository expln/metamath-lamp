open Expln_React_common

type variant = [ #scrollable | #fullWidth | #standard ]

@module("@mui/material/Tabs") @react.component
external make: (
    ~value:string=?,
    ~onChange:(ReactEvent.Mouse.t,string)=>unit=?,
    ~variant:variant=?,
    ~children: reElem,
) => reElem = "default"
