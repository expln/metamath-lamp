open Expln_React_common

type variant = [#text|#contained|#outlined]
@module("@mui/material/IconButton") @react.component
external make: (
    ~ref:ReactDOM.domRef=?,
    ~onClick: reMouseHnd=?, 
    ~color: string=?, 
    ~style: reStyle=?,
    ~component:string=?,
    ~disabled:bool=?,
    ~title:string=?,
    ~children: reElem=?
) => reElem = "default"
