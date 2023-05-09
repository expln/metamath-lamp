open MM_context
open MM_substitution
open MM_parenCounter
open MM_wrk_settings

let procName: string
type request
type response

let beginWorkerInteractionUsingCtx: (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
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

let reqToStr: request => string
let respToStr: response => string