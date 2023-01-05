open MM_context
open MM_substitution
open MM_parenCounter

let procName: string

type wrkCtxHyp = {
    id: string,
    label: string, 
    text: string
}

type wrkPrecalcData = {
    preCtxVer: int,
    preCtx: option<mmContext>,
    wrkFrms: option<Belt_MapString.t<frmSubsData>>,
    parenStr: string,
    wrkParenInts: option<array<int>>,
    wrkParenCnt: option<parenCnt>,
    varsText: string,
    disjText: string,
    hyps: array<wrkCtxHyp>,
    wrkCtx: option<mmContext>,
}

let getWrkCtxExn: () => mmContext
let getWrkParenCntExn: () => parenCnt
let getWrkFrmsExn: () => Belt_MapString.t<frmSubsData>

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
    hypErr: option<(string, string)>,
}

type request

type response

let prepareParenInts: (mmContext, string) => array<int>

let createWrkCtx: (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
) => result<mmContext, wrkCtxErr>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

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
