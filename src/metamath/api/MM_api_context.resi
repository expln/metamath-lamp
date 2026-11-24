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
