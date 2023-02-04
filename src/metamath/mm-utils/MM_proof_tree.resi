open MM_context
open MM_substitution
open MM_parenCounter

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
    | Assertion({args:array<proofNode>, label:string, frame:frame})

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
let ptGetNodeByExpr: (proofTree, expr) => option<proofNode>
let ptGetOrCreateNode: (proofTree, expr) => proofNode
let ptAddNewVar: (proofTree, int) => int
let ptAddDisjPair: (proofTree, int, int) => unit
let ptEraseDists: proofTree => unit
let ptGetCopyOfNewVars: proofTree => array<expr>
let ptGetCopyOfDisj: proofTree => disjMutable

let ptGetNuberOfNodes: proofTree => int
let ptGetStats: proofTree => unit

let pnGetExpr: proofNode => expr
let pnGetExprStr: proofNode => option<string>
let pnGetProof: proofNode => option<exprSource>
let pnGetParents: proofNode => option<array<exprSource>>
let pnAddParent: (proofNode, exprSource) => unit
let pnGetDist: proofNode => option<int>
let pnSetDist: (proofNode, option<int>) => unit
let pnIsInvalidFloating: proofNode => bool
let pnSetInvalidFloating: (proofNode, bool) => unit
