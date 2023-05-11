open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_editor
open Expln_utils_promise

@react.component
let make: (
    ~modalRef: modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
    ~reloadCtx: React.ref<Js.Nullable.t<array<mmCtxSrcDto> => promise<result<unit,string>>>>,
    ~tempMode:bool,
) => React.element
