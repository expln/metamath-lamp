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
  | Hypothesis({label:string})
  | Assertion({args:array<proofNode>, label:string})

type proofTree

let ptMake: (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => proofTree

let ptGetFrms: proofTree => Belt_MapString.t<frmSubsData>
let ptGetHypByExpr: (proofTree, expr) => option<hypothesis>
let ptGetHypByLabel: (proofTree, string) => option<hypothesis>
let ptGetMaxVar: proofTree => int
let ptGetParenCnt: proofTree => parenCnt
let ptIsDisj: (proofTree, int, int) => bool
let ptIsNewVarDef: (proofTree, expr) => bool
let ptGetNodeByExpr: (proofTree, expr) => option<proofNode>
let ptMakeNode: ( proofTree, ~label: option<string>, ~expr: expr, ) => proofNode
let ptAddNewVar: (proofTree, int) => int
let ptAddDisjPair: (proofTree, int, int) => unit

let pnGetLabel: proofNode => option<string>
let pnGetExpr: proofNode => expr
let pnGetProof: proofNode => option<exprSource>
let pnGetParents: proofNode => option<array<exprSource>>
let pnAddParent: (proofNode, exprSource) => unit
let pnCreateProofTable: proofNode => proofTable
