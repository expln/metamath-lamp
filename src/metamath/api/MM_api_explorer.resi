let updateExplorerApi: (
    ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
    ~searchAssertions: (
        ~label: string, ~strictLabel: string, ~pattern: string
    ) => promise<result<array<MM_context.frame>, string>>,
    ~getEditorScope: string => result<(int, string), string>,
) => unit
