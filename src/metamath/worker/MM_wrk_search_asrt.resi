open MM_wrk_ctx
open MM_context
open MM_asrt_apply
open Expln_utils_promise

let procName: string

type request
type response

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