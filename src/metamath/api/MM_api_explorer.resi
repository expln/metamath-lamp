let framesToJson: (~preCtxData: MM_wrk_pre_ctx_data.preCtxData, array<MM_context.frame>) => JSON.t

/*
Searches assertions by a pattern in the whole loaded context (discouraged and deprecated assertions are excluded),
using the search function passed to updateExplorerApi().
If maxFrmOrd is defined, only assertions with frame.ord <= maxFrmOrd are returned.
*/
let searchAssertions: (
    ~pattern: string,
    ~maxFrmOrd: int=?,
) => promise<result<array<MM_context.frame>, string>>

let updateExplorerApi: (
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~searchAssertions: (~pattern: string) => promise<result<array<MM_context.frame>, string>>,
    ~getEditorScope: int => result<(int, string), string>,
) => unit
