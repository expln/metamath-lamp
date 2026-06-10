let searchAssertions: (
    ~modalRef: Expln_React_Modal.modalRef,
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~typ: option<int>,
    ~label: string,
    ~patternStr: string,
    ~patternVersion: int,
) => promise<result<array<(string, option<MM_wrk_pattern_search.matchedIndices>)>, string>>

@react.component
let make: (
    ~modalRef: Expln_React_Modal.modalRef,
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~maxFrmOrd: int,
    ~wrkCtx: MM_context.mmContext,
    ~initialTyp: option<int>,
    ~onTypChange: int => unit,
    ~onCanceled: unit => unit,
    ~onResultsSelected: array<string> => unit,
) => React.element
