open Expln_React_common

type variant = [#text|#contained|#outlined]
@module("@mui/material/Box") @react.component
external make: (
    ~sx:{..}=?,
    ~children:reElem=?
) => reElem = "default"
