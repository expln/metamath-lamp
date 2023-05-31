open Expln_React_common

@module("@mui/material/Select") @react.component
external make: (
    ~sx:{..}=?,
    ~labelId:string=?,
    ~label:string=?,
    ~value:string,
    ~onChange:reFormHnd=?,
    ~onClose:unit=>unit=?,
    ~children:reElem=?
) => reElem = "default"