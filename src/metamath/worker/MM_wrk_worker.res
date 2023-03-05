open MM_wrk_api
open Common

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~req:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type workerFunc<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let makeRequestProcessor = (func:workerFunc<'req,'resp>):requestProcessor => 
        (~req:workerRequest, ~sendToClient:workerResponse=>unit) => func(
            ~req = deserialize(req.body), 
            ~sendToClient = resp=>sendToClient({clientId:req.clientId, body:serialize(resp)}),
        )

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_ParseMmFile.procName, 
        makeRequestProcessor(MM_wrk_ParseMmFile.processOnWorkerSide)
    ),
    (
        MM_wrk_LoadCtx.procName, 
        makeRequestProcessor(MM_wrk_LoadCtx.processOnWorkerSide)
    ),
    (
        MM_wrk_FindParens.procName, 
        makeRequestProcessor(MM_wrk_FindParens.processOnWorkerSide)
    ),
    (
        MM_wrk_ctx_proc.procName,
        makeRequestProcessor(MM_wrk_ctx_proc.processOnWorkerSide)
    ),
    (
        MM_wrk_search_asrt.procName, 
        makeRequestProcessor(MM_wrk_search_asrt.processOnWorkerSide)
    ),
    (
        MM_wrk_unify.procName, 
        makeRequestProcessor(MM_wrk_unify.processOnWorkerSide)
    ),
])

let processRequest: workerRequest => unit = req => {
    if (req.traceEnabled) {
        Js.Console.log(`${currTimeStr()} [clientId=${req.clientId->Belt_Int.toString}] worker received a request, procName = ${req.procName}`)
    }
    processors->Belt_MapString.get(req.procName)->Belt_Option.forEach(processor => processor(~req, ~sendToClient=resp=>{
        if (req.traceEnabled) {
            Js.Console.log(`${currTimeStr()} [clientId=${resp.clientId->Belt_Int.toString}] worker is sending a response`)
        }
        sendToClient(resp)
    }))
}
