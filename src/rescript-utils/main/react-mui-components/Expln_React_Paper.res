open Expln_React_common

type variant = [ #elevation | #outlined ]

@module("@mui/material/Paper") @react.component
external make: (
    ~onClick:reMouseHnd=?,
    ~elevation:int=?,
    ~style: reStyle=?,
    ~variant:variant=?,
    ~square:bool=?,
    ~title:string=?,
    ~children: reElem=?
) => reElem = "default"
