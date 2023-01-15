open MM_proof_tree2
open MM_context
open MM_substitution
open MM_parenCounter

let proveFloating: (proofTree, proofNode) => unit

let unifyAll: (
  ~ctx: mmContext,
  ~frms: Belt_MapString.t<frmSubsData>,
  ~stmts: array<rootStmt>,
  ~parenCnt: parenCnt,
  ~onProgress: float => unit=?,
  ~debug: bool=?,
  unit,
) => proofTree
