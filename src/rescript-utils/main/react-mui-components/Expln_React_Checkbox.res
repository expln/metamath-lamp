open Expln_React_common

@module("@mui/material/Checkbox") @react.component
external make: (
    ~style: reStyle=?,
    ~disabled: bool=?,
    ~indeterminate: bool=?,
    ~checked: bool=?,
    ~onChange: reFormHnd=?,
) => reElem = "default"
