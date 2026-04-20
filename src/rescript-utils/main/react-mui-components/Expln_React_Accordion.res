open Expln_React_common

@module("@mui/material/Accordion") @react.component
external make: (
    ~expanded: bool=?,
    ~slotProps:{..}=?,
    ~children: reElem,
) => reElem = "default"
