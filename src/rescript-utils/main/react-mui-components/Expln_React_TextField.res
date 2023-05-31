open Expln_React_common

type size = [ #medium | #small ]
type variant = [ #filled | #outlined | #standard ]

@module("@mui/material/TextField") @react.component
external make: (
    ~inputRef:ReactDOM.domRef=?,
    ~value:string=?,
    ~label:string=?,
    ~size:size=?,
    ~style:reStyle=?,
    ~variant:variant=?,
    ~multiline:bool=?,
    ~minRows:int=?,
    ~maxRows:int=?,
    ~rows:int=?,
    ~onChange:reFormHnd=?,
    ~onKeyDown:reKeyboardHnd=?,
    ~onKeyUp:reKeyboardHnd=?,
    ~inputProps:{..}=?,
    ~disabled:bool=?,
    ~autoFocus:bool=?,
    ~title:string=?,
    ~error:bool=?,
) => reElem = "default"