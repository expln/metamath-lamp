open MM_context
open MM_cmp_context_selector_single
open Expln_React_Modal

type mmCtxSrcDto = {
    fileSrc: mmFileSource,
    readInstr: readInstr,
    label: string,
}

@react.component
let make: (
    ~modalRef: modalRef,
    ~onChange: (array<mmCtxSrcDto>, mmContext) => unit,
) => React.element
