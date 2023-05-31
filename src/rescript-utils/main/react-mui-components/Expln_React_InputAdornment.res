open Expln_React_common

type position = [ #end | #start ]

@module("@mui/material/InputAdornment") @react.component
external make: (
    ~position:position=?,
    ~children:reElem=?
) => reElem = "default"