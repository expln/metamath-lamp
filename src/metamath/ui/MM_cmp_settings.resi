open MM_context
open Expln_React_common
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_pre_ctx_data

let settingsReadFromLocStor: () => settings
let settingsSaveToLocStor: settings => unit

@react.component
let make: (
    ~modalRef:modalRef, 
    ~preCtxData:preCtxData,
    ~onChange: settings => unit
) => reElem