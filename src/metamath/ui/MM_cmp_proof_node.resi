open MM_proof_tree_dto
open MM_context
open Expln_React_common

@react.component
let make: (
  ~tree: proofTreeDto,
  ~nodeIdx: int,
  ~isRootStmt: int=>bool,
  ~nodeIdxToLabel: int => string,
  ~exprToStr: expr=>string,
  ~exprToReElem: expr => reElem,
) => React.element
