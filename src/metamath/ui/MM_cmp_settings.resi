open MM_context
open Expln_React_common
open Expln_React_Modal
open MM_wrk_settings

let createDefaultSettings: unit => settings
let settingsReadFromLocStor: string => option<settings>
let getParens: settings => array<string>
let getTypeColors: settings => Belt_MapString.t<string>

@react.component
let make: (~modalRef:modalRef, ~ctx:mmContext, ~initialSettings:settings, ~onChange: settings => unit) => reElem