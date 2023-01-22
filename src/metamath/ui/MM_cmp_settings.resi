open MM_context
open Expln_React_common
open Expln_React_Modal
open MM_wrk_settings

let settingsReadFromLocStor: () => settings

@react.component
let make: (~modalRef:modalRef, ~ctx:mmContext, ~initialSettings:settings, ~onChange: settings => unit) => reElem