open MM_context
open MM_substitution
open MM_parenCounter
open MM_unification_debug
open MM_statements_dto

type rootStmt = {
    isHyp: bool,
    label: string,
    expr: expr,
    jstf: option<jstf>,
}

type proofTreeDbg = {
    newVars: array<string>,
    disj: array<string>,
    exprToStr: expr=>string,
}

type proofNode

type exprSrc =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame, missingDisj:option<disjMutable>})
    | AssertionWithErr({args:array<proofNode>, frame:frame, err:unifErr})

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
let ptGetNode: (proofTree, expr) => proofNode
let ptAddNewVar: (proofTree, int) => int
let ptAddDisjPair: (proofTree, int, int) => unit
let ptGetDbg: proofTree => option<proofTreeDbg>
let ptAddRootStmt: (proofTree, rootStmt) => unit
let ptClearDists: proofTree => unit

let ptGetCopyOfNewVars: proofTree => array<expr>
let ptGetDisj: proofTree => disjMutable

let pnGetExpr: proofNode => expr
let pnGetExprStr: proofNode => option<string>
let pnGetProof: proofNode => option<exprSource>
let pnGetFParents: proofNode => option<array<exprSource>>
let pnGetEParents: proofNode => array<exprSource>
let pnAddParent: (proofNode, exprSource, bool) => unit
let pnIsInvalidFloating: proofNode => bool
let pnSetInvalidFloating: proofNode => unit
let pnGetDist: proofNode => option<int>
let pnSetDist: (proofNode,int) => unit
