open MM_context
open MM_substitution
open MM_parenCounter
open MM_unification_debug

type justification = {
    args: array<string>,
    asrt: string
}

type rootStmt = {
    label: string,
    expr: expr,
    justification: option<justification>,
}

type proofNode

type exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame, err:option<unifErr>})

type proofTree

let ptMake: (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~ctxMaxVar: int,
    ~disj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => proofTree

let ptGetFrms: proofTree => Belt_MapString.t<frmSubsData>
let ptGetHypByExpr: (proofTree, expr) => option<hypothesis>
let ptGetHypByLabel: (proofTree, string) => option<hypothesis>
let ptGetMaxVar: proofTree => int
let ptGetCtxMaxVar: proofTree => int
let ptGetParenCnt: proofTree => parenCnt
let ptIsDisj: (proofTree, int, int) => bool
let ptIsNewVarDef: (proofTree, expr) => bool
let ptGetRootStmts: proofTree => array<rootStmt>
let ptGetNodeByExpr: (proofTree, expr) => option<proofNode>
let ptGetOrCreateNode: (proofTree, expr) => proofNode
let ptAddNewVar: (proofTree, int) => int
let ptAddDisjPair: (proofTree, int, int) => unit
let ptGetCopyOfNewVars: proofTree => array<expr>
let ptGetCopyOfDisj: proofTree => disjMutable
let ptGetExprToStr: proofTree => option<expr=>string>
let ptAddRootStmt: (proofTree, rootStmt) => unit
let ptClearDists: proofTree => unit

let ptGetNuberOfNodes: proofTree => int
let ptGetStats: proofTree => unit

let pnGetExpr: proofNode => expr
let pnGetExprStr: proofNode => option<string>
let pnGetProof: proofNode => option<exprSource>
let pnGetParents: proofNode => option<array<exprSource>>
let pnAddParent: (proofNode, exprSource) => unit
let pnIsInvalidFloating: proofNode => bool
let pnSetInvalidFloating: (proofNode, bool) => unit
let pnGetDist: proofNode => option<int>
let pnSetDist: (proofNode,int) => unit
