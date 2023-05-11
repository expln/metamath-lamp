open MM_context
open Expln_React_common
open Expln_React_Modal
open MM_wrk_settings

let settingsReadFromLocStor: () => settings
let settingsSaveToLocStor: (settings,bool) => unit

@react.component
let make: (
    ~modalRef:modalRef, 
    ~ctx:mmContext, 
    ~settingsVer:int, 
    ~settings:settings, 
    ~onChange: settings => unit
) => reElem