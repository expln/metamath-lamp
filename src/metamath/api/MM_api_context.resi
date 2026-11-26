type ctxSrcToLoad = {
    url:string,
    readInstr:MM_wrk_editor.readInstr,
    label:option<string>,
}

type loadedCtxInfo = {
    summary:string,
    numOfAsrts:int,
}

let updateContextApi: (
    ~loadContext:array<ctxSrcToLoad>=>promise<result<loadedCtxInfo,string>>,
) => unit

/*
* Makes the given context the one described by context.getLoadedContextInfo.
* */
let updateLoadedContext: (~preCtxData:MM_wrk_pre_ctx_data.preCtxData) => unit
