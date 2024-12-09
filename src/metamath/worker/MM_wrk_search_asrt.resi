open MM_context
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
let makeSearchPattern: (~searchStr:string, ~ctx:mmContext) => result<array<stmtPattern>, string>
let threeStateBoolMatchesTwoStateBool: (option<bool>, bool) => bool

let doSearchAssertions: (
    ~wrkCtx:mmContext,
    ~allFramesInDeclarationOrder:array<frame>,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~searchPattern:array<stmtPattern>,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~returnLabelsOnly:bool,
    ~onProgress:float=>unit=?
) => (array<stmtsDto>,array<string>)

let searchAssertions: (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~searchPattern:array<stmtPattern>,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~returnLabelsOnly:bool,
    ~onProgress:float=>unit,
) => promise<(array<stmtsDto>,array<string>)>

let frameToStmtsDto: (
    ~wrkCtx:mmContext,
    ~frame:frame,
) => stmtsDto

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let frameMatchesPattern: (
    ~frame:frame, 
    ~searchPattern:array<stmtPattern>, 
    ~mapping:Belt_HashMapInt.t<int>
) => bool

let reqToStr: request => string
let respToStr: response => string