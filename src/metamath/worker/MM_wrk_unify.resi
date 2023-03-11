open MM_context
open MM_wrk_ctx_data
open MM_proof_tree
open MM_proof_tree_dto
open MM_statements_dto
open MM_wrk_settings

let procName: string

type request
type response

let unify: (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: MM_context.mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~rootProvables: array<rootStmt>,
    ~bottomUpProverParams: option<MM_provers.bottomUpProverParams>,
    ~debugLevel:int,
    ~onProgress: string => unit,
) => Expln_utils_promise.promise<MM_proof_tree_dto.proofTreeDto>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let proofTreeDtoToNewStmtsDto: (
    ~treeDto:proofTreeDto, 
    ~rootStmts:array<rootStmt>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~reservedLabels: array<string>,
) => array<stmtsDto>

let reqToStr: request => string
let respToStr: response => string