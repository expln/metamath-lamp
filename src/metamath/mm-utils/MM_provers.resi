open MM_proof_tree
open MM_context
open MM_substitution
open MM_parenCounter

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    maxSearchDepth: int,
    lengthRestriction: lengthRestrict,
    allowNewVars: bool,
}

let proveFloatings: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<expr>,
    ~framesToSkip: array<string>=?,
    ~parenCnt: parenCnt,
    (),
) => proofTree


let unifyAll: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~framesToSkip: array<string>=?,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:bottomUpProverParams=?,
    ~onProgress: string=>unit=?,
    (),
) => proofTree

