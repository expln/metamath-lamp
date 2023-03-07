open MM_proof_tree
open MM_context
open MM_substitution
open MM_parenCounter

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    maxSearchDepth: int,
    lengthRestriction: lengthRestrict,
    allowNewStmts: bool,
    allowNewVars: bool,
    args0: array<expr>,
    args1: array<expr>,
    maxNumberOfBranches: option<int>,
}

let proveFloatings: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~floatingsToProve: array<expr>,
    ~parenCnt: parenCnt,
    (),
) => proofTree


let unifyAll: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~rootProvables: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:bottomUpProverParams=?,
    ~debugLevel:int=?,
    ~onProgress: string=>unit=?,
    (),
) => proofTree

