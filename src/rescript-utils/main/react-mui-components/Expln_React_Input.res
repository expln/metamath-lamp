open Expln_React_common

type size = [ #small | #medium ]

@module("@mui/material/Input") @react.component
external make: (
    ~value:string=?,
    ~size:size=?,
    ~onChange:reFormHnd=?,
) => reElem = "default"