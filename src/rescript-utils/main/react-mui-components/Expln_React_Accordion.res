open Expln_React_common

@module("@mui/material/Accordion") @react.component
external make: (
    ~expanded: bool=?,
    ~children: reElem,
) => reElem = "default"
