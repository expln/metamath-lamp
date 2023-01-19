open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx
open MM_substitution
open MM_statements_dto

let procName: string

type request
type response

let doSearchAssertions: (
    ~wrkCtx:mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~label: string,
    ~typ: int,
    ~pattern: array<int>,
    ~onProgress: float => unit=?,
    unit,
) => array<newStmtsDto>

let searchAssertions: (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~label:string,
    ~typ: int,
    ~pattern: array<int>,
    ~onProgress:float=>unit,
) => promise<array<newStmtsDto>>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let frameMatchesPattern: (frame, array<int>) => bool