open Expln_React_common

@module("@mui/material/ListItemButton") @react.component
external make: (
    ~onClick: reMouseHnd=?,
    ~children: reElem=?
) => reElem = "default"
