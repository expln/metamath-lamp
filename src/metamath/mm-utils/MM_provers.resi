open MM_proof_tree
open MM_context
open MM_substitution
open MM_parenCounter

let proveFloatings: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<expr>,
    ~parenCnt: parenCnt,
    ~debug: bool=?,
    (),
) => proofTree


let unifyAll: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUp:bool,
    ~maxSearchDepth:int,
    ~onProgress: float => unit=?,
    ~debug: bool=?,
    (),
) => proofTree

