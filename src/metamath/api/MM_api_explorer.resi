let framesToJson: (~preCtxData: MM_wrk_pre_ctx_data.preCtxData, array<MM_context.frame>) => JSON.t

let updateExplorerApi: (
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~searchAssertions: (~pattern: string) => promise<result<array<MM_context.frame>, string>>,
) => unit
