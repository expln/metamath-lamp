open MM_context
open MM_substitution
open MM_statements_dto
open MM_wrk_settings

let procName: string

type request
type response

let doSearchAssertions: (
    ~wrkCtx:mmContext,
    ~frms: frms,
    ~label: string,
    ~typ: int,
    ~pattern: array<int>,
    ~onProgress: float => unit=?,
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

let frameMatchesVarPattern: (
    frame, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~mapping:Belt_HashMapInt.t<int>
) => bool

let reqToStr: request => string
let respToStr: response => string