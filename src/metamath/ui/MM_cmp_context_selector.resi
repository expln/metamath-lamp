open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_editor

@react.component
let make: (
    ~modalRef: modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
) => React.element
