open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_proof_table
open MM_progress_tracker

type proofNode

type exprSource =
  | VarType
  | Hypothesis({label: string})
  | Assertion({args: array<proofNode>, label: string})

type proofTree

let proofTreeMake: (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => proofTree

let proofTreeGetFrms: proofTree => Belt_MapString.t<frmSubsData>
let proofTreeGetParenCnt: proofTree => parenCnt
let proofTreeIsDisj: (proofTree, int, int) => bool
let proofTreeGetNodeByExpr: (proofTree, expr) => option<proofNode>
let proofTreeAddRootNode: (proofTree, proofNode) => unit
let proofTreeMakeNode: ( proofTree, ~label: option<string>, ~expr: expr, ) => proofNode
let proofNodeAddParent: (proofNode, exprSource) => unit
let proofTreeCreateProofTable: proofNode => MM_proof_table.proofTable
