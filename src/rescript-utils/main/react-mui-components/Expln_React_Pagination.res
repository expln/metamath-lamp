open Expln_React_common

@module("@mui/material/Pagination") @react.component
external make: (
    ~count:int,
    ~page:int,
    ~siblingCount:int=?,
    ~onChange:(_,int) => unit,
) => reElem = "default"
