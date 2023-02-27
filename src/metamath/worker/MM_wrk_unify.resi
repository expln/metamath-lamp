open MM_context
open Expln_utils_promise
open MM_wrk_ctx
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_statements_dto

let procName: string

type request
type response

let unify: (
    ~preCtxVer: int,
    ~preCtx: MM_context.mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~rootProvables: array<rootStmt>,
    ~bottomUpProverParams: option<MM_provers.bottomUpProverParams>,
    ~onProgress: string => unit,
) => Expln_utils_promise.promise<MM_proof_tree_dto.proofTreeDto>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let proofTreeDtoToNewStmtsDto: (
    ~treeDto:proofTreeDto, 
    ~rootStmts:array<rootStmt>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
) => array<stmtsDto>
