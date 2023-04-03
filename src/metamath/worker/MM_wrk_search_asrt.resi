open MM_context
open Expln_utils_promise
open MM_substitution
open MM_statements_dto
open MM_wrk_settings

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
) => array<stmtsDto>

let searchAssertions: (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~label:string,
    ~typ: int,
    ~pattern: array<int>,
    ~onProgress:float=>unit,
) => promise<array<stmtsDto>>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let frameMatchesPattern: (frame, array<int>) => bool

let reqToStr: request => string
let respToStr: response => string