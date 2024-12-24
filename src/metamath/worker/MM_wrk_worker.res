open MM_wrk_api

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~wReq:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type workerFunc<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let makeRequestProcessor = (
    func:workerFunc<'req,'resp>, 
    reqToStr:'req=>string, 
    respToStr:'resp=>string
):requestProcessor => {
    (~wReq:workerRequest, ~sendToClient:workerResponse=>unit) => {
        let req = deserialize(wReq.body)
        if (wReq.traceEnabled) {
            logWorkerReceivedRequest(
                ~clientId=wReq.clientId,
                ~procName=wReq.procName,
                ~req,
                ~reqToStr,
            )
        }
        func(
            ~req, 
            ~sendToClient = resp => {
                if (wReq.traceEnabled) {
                    logWorkerIsSendingResponse(
                        ~clientId=wReq.clientId,
                        ~procName=wReq.procName,
                        ~resp,
                        ~respToStr,
                    )
                }
                sendToClient({clientId:wReq.clientId, body:serialize(resp)})
            }
        )
    }
}

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_ParseMmFile.procName, 
        makeRequestProcessor(MM_wrk_ParseMmFile.processOnWorkerSide, MM_wrk_ParseMmFile.reqToStr, MM_wrk_ParseMmFile.respToStr)
    ),
    (
        MM_wrk_LoadCtx.procName, 
        makeRequestProcessor(MM_wrk_LoadCtx.processOnWorkerSide, MM_wrk_LoadCtx.reqToStr, MM_wrk_LoadCtx.respToStr)
    ),
    (
        MM_wrk_FindParens.procName, 
        makeRequestProcessor(MM_wrk_FindParens.processOnWorkerSide, MM_wrk_FindParens.reqToStr, MM_wrk_FindParens.respToStr)
    ),
    (
        MM_wrk_ctx_proc.procName,
        makeRequestProcessor(MM_wrk_ctx_proc.processOnWorkerSide, MM_wrk_ctx_proc.reqToStr, MM_wrk_ctx_proc.respToStr)
    ),
    (
        MM_wrk_search_asrt.procName, 
        makeRequestProcessor(MM_wrk_search_asrt.processOnWorkerSide, MM_wrk_search_asrt.reqToStr, MM_wrk_search_asrt.respToStr)
    ),
    (
        MM_wrk_unify.procName, 
        makeRequestProcessor(MM_wrk_unify.processOnWorkerSide, MM_wrk_unify.reqToStr, MM_wrk_unify.respToStr)
    ),
    (
        MM_wrk_syntax_tree.procName, 
        makeRequestProcessor(MM_wrk_syntax_tree.processOnWorkerSide, MM_wrk_syntax_tree.reqToStr, MM_wrk_syntax_tree.respToStr)
    ),
])

let processRequest: workerRequest => unit = req => {
    processors->Belt_MapString.get(req.procName)
        ->Belt_Option.forEach(processor => processor(~wReq=req, ~sendToClient))
}
