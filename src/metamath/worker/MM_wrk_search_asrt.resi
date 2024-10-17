open MM_context
open MM_substitution
open MM_statements_dto
open MM_wrk_settings

let procName: string

type request
type response

type patternModifier = Hyp | Asrt | Exact | Adj
type stmtPattern = {
    flags: array<patternModifier>,
    varPat: array<int>,
    constPat: array<int>,
}

let parseSearchStr: string => result<array<(array<patternModifier>, array<string>)>,string>

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

let frameToStmtsDto: (
    ~wrkCtx:mmContext,
    ~frame:frame,
) => stmtsDto

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let frameMatchesPattern: (
    ~frame:frame, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~mapping:Belt_HashMapInt.t<int>
) => bool

let frameMatchesPattern2: (
    ~frame:frame, 
    ~patterns:array<stmtPattern>, 
    ~mapping:Belt_HashMapInt.t<int>
) => bool

let reqToStr: request => string
let respToStr: response => string