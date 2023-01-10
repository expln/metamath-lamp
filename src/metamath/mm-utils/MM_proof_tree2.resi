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
let proofTreeIsNewVarDef: (proofTree, expr) => bool
let proofTreeGetNodeByExpr: (proofTree, expr) => option<proofNode>
let proofTreeGetHypByExpr: (proofTree, expr) => option<hypothesis>
let proofTreeAddRootNode: (proofTree, proofNode) => unit
let proofTreeMakeNode: ( proofTree, ~label: option<string>, ~expr: expr, ) => proofNode

let proofNodeGetExpr: proofNode => expr
let proofNodeGetProof: proofNode => option<exprSource>
let proofNodeGetParents: proofNode => option<array<exprSource>>
let proofNodeAddNonAsrtParent: (proofNode, exprSource) => unit
let proofNodeAddAsrtParent: (proofNode, exprSource, frame) => unit
let proofNodeCreateProofTable: proofNode => proofTable
