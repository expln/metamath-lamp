open Expln_React_common
open MM_wrk_editor
open MM_context

type rootStmtRendered = {
    id: string,
    expr: expr,
    isHyp: bool,
    label: string,
    proofStatus: option<proofStatus>,
    exprReElem: reElem,
}

@react.component
let make: (
  ~title: string,
  ~rootStmtsRendered: array<rootStmtRendered>,
  ~proofStatusesAreAvailable: bool,
  ~flags: array<bool>,
  ~onClose: array<bool> => unit,
) => React.element
