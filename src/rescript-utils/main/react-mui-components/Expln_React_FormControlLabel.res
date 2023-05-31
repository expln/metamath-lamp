open Expln_React_common

@module("@mui/material/FormControlLabel") @react.component
external make: (
    ~control: reElem, 
    ~label: string, 
    ~disabled: bool=?, 
    ~style: reStyle=?,
    ~value: string=?,
) => reElem = "default"
