open MM_context
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
    ~rootStmts: array<rootStmt>,
    ~bottomUpProverParams: option<MM_provers.bottomUpProverParams>,
    ~syntaxTypes:option<array<int>>,
    ~exprsToSyntaxCheck:option<array<expr>>,
    ~debugLevel:int,
    ~onProgress: string => unit,
) => promise<MM_proof_tree_dto.proofTreeDto>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let proofTreeDtoToNewStmtsDto: (
    ~treeDto:proofTreeDto, 
    ~exprToProve: expr,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~rootExprToLabel: Belt_HashMap.t<expr,string,ExprHash.identity>,
    ~reservedLabels: array<string>,
) => array<stmtsDto>

let reqToStr: request => string
let respToStr: response => string