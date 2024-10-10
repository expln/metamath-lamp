open Expln_React_common

type direction = [ #column | #"column-reverse" | #row | #"row-reverse" ]
type justifyContent = [ #"flex-start" | #"center" | #"flex-end" | #"space-between" | #"space-around" | #"space-evenly" ]
type alignItems = [ #"flex-start" | #"center" | #"flex-end" | #"stretch" | #"baseline" ]
@module("@mui/material/Unstable_Grid2") @react.component
external make: (
    ~ref:ReactDOM.domRef=?,
    ~container:bool=?, 
    ~direction:direction=?,
    ~justifyContent:justifyContent=?,
    ~alignItems:alignItems=?,
    ~spacing:float=?,
    ~style:reStyle=?, 
    ~xsOffset:JSON.t=?,
    ~children: reElem=?
) => reElem = "default"