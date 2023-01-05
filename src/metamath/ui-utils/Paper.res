open Expln_React_common

type variant = [#elevation | #outlined]

@module("@mui/material/Paper") @react.component
external make: (
    ~variant:variant=?,
    ~elevation:int=?,
    ~square:bool=?,
    ~children: reElem,
) => reElem = "default"
