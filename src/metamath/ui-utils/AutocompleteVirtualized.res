open Expln_React_common

type size = [#small | #medium]

@module("./AutocompleteVirtualized.js") @react.component
external make: (
    ~value:option<string>,
    ~options: array<string>,
    ~onChange: option<string>=>unit,
    ~size:size=?,
    ~width:int=?,
) => reElem = "default"
