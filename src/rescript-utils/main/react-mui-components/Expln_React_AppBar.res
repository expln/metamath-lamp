open Expln_React_common

type position = [ #absolute | #fixed | #relative | #static | #sticky ]

@module("@mui/material/AppBar") @react.component
external make: (
    ~position: position=?,
    ~color: string=?,
    ~children: reElem,
) => reElem = "default"
