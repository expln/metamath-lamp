open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_pre_ctx_data
open Expln_React_common

@react.component
let make: (
    ~modalRef: modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
    ~reloadCtx: React.ref<Js.Nullable.t<array<mmCtxSrcDto> => promise<result<unit,string>>>>,
    ~style:reStyle=?,
    ~tempMode:bool,
    ~onExpandedChange:bool=>unit,
    ~doExpand: React.ref<Js.Nullable.t<unit=>unit>>,
) => React.element
