open Expln_React_common

type orientation = [ #horizontal | #vertical ]

@module("@mui/material/Divider") @react.component
external make: (
    ~orientation:orientation=?,
) => reElem = "default"
