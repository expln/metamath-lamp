open Expln_React_common

type variant = [#text|#contained|#outlined]
@module("@mui/material/Button") @react.component
external make: (
    ~onClick: reMouseHnd=?, 
    ~variant:variant=?, 
    ~disabled:bool=?,
    ~color:string=?,
    ~title:string=?,
    ~style:reStyle=?,
    ~children: reElem=?
) => reElem = "default"
