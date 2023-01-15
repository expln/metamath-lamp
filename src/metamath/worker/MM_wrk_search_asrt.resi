open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx
open MM_substitution
open MM_parenCounter

let procName: string

type request
type response

let doSearchAssertions: (
  ~wrkCtx: mmContext,
  ~frms: Belt_MapString.t<frmSubsData>,
  ~parenCnt: parenCnt,
  ~label: string,
  ~typ: int,
  ~pattern: array<int>,
  ~onProgress: float => unit=?,
  unit,
) => array<applyAssertionResult>

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
) => promise<array<applyAssertionResult>>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let frameMatchesPattern: (frame, array<int>) => bool