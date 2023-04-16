open MM_context
open MM_cmp_context_selector_single
open Expln_React_Modal
open MM_wrk_settings

type mmCtxSrcDto = {
    fileSrc: mmFileSource,
    readInstr: readInstr,
    label: string,
}

@react.component
let make: (
    ~modalRef: modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
) => React.element
