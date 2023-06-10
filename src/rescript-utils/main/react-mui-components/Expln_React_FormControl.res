open Expln_React_common

type size = [#small | #medium]

@module("@mui/material/FormControl") @react.component
external make: (
    ~disabled:bool=?,
    ~size:size=?,
    ~style:reStyle=?,
    ~children:reElem=?
) => reElem = "default"