open MM_context
open MM_substitution
open MM_parenCounter
open MM_wrk_ctx_data

let procName: string
type request
type response

let beginWorkerInteractionUsingCtx: (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~procName: string,
    ~initialRequest: 'req,
    ~onResponse: ( ~resp: 'resp, ~sendToWorker: 'req => unit, ~endWorkerInteraction: unit => unit, ) => unit,
    ~enableTrace: bool=?,
    unit,
) => unit

let getWrkCtxExn: () => mmContext
let getWrkParenCntExn: () => parenCnt
let getWrkFrmsExn: () => Belt_MapString.t<frmSubsData>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit


