open Expln_React_common

type size = [#small | #medium]

@module("./AutocompleteVirtualized.js") @react.component
external make: (
    ~inputRef:ReactDOM.domRef=?,
    ~value:option<string>,
    ~options: array<string>,
    ~onChange: option<string>=>unit,
    ~size:size=?,
    ~width:int=?,
    ~label:string,
) => reElem = "default"
