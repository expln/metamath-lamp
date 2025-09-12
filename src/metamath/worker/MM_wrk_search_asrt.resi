open MM_context
open MM_statements_dto
open MM_wrk_settings

let procName: string

type request
type response

let threeStateBoolMatchesTwoStateBool: (option<bool>, bool) => bool

let doSearchAssertions: (
    ~allFramesInDeclarationOrder:array<frame>,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~pattern:string,
    ~patternVersion:int,
    ~ctx:mmContext,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~onProgress:float=>unit=?
) => array<(frame,option<array<array<int>>>)>

let searchAssertions: (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~pattern:string,
    ~patternVersion:int,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~onProgress:float=>unit,
) => promise<array<(string,option<array<array<int>>>)>>

let frameToStmtsDto: (
    ~wrkCtx:mmContext,
    ~frame:frame,
) => stmtsDto

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let reqToStr: request => string
let respToStr: response => string