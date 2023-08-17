open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_pre_ctx_data
open Expln_React_common

type reloadCtxFunc = (
    ~srcs:array<mmCtxSrcDto>, 
    ~settings:settings, 
    ~force:bool=?, 
    ~showError:bool=?, 
    ()
) => promise<result<unit,string>>

@react.component
let make: (
    ~modalRef: modalRef,
    ~settings:settings,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
    ~reloadCtx: React.ref<Js.Nullable.t<reloadCtxFunc>>,
    ~style:reStyle=?,
    ~onExpandedChange:bool=>unit,
    ~doToggle: React.ref<Js.Nullable.t<unit=>unit>>,
) => React.element
