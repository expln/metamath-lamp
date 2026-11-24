let updateExplorerApi: (
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~searchAssertions: (~pattern: string) => promise<result<array<MM_context.frame>, string>>,
    ~getEditorScope: string => result<(int, string), string>,
) => unit
