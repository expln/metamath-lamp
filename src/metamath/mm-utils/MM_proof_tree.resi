open MM_context
open MM_substitution
open MM_parenCounter
open MM_unification_debug
open MM_statements_dto
open MM_bottom_up_prover_params

type rootStmt = {
    isHyp: bool,
    label: string,
    expr: expr,
    jstf: option<jstf>,
}

type proofNodeDbg = {
    exprStr:string,
}

type proofTreeDbg = {
    newVars: array<string>,
    exprToStr: expr=>string,
}

type proofNode

type exprSrc =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame})
    | AssertionWithErr({args:array<proofNode>, frame:frame, err:unifErr})

type proofTree

let ptMake: (
    ~proofCtx:mmContext,
    ~frms: frms,
    ~parenCnt: parenCnt,
) => proofTree

let ptGetProofCtx: proofTree => mmContext
let ptGetFrms: proofTree => frms
let ptGetHypByExpr: (proofTree, expr) => option<hypothesis>
let ptGetHypByLabel: (proofTree, string) => option<hypothesis>
let ptGetMaxVar: proofTree => int
let ptGetCtxMaxVar: proofTree => int
let ptGetParenCnt: proofTree => parenCnt
let ptIsDisjInCtx: (proofTree, int, int) => bool
let ptIsNewVarDef: (proofTree, expr) => bool
let ptGetRootStmts: proofTree => array<rootStmt>
let ptGetNode: (proofTree, expr) => proofNode
let ptAddNewVar: (proofTree, int) => int
let ptAddRootStmt: (proofTree, rootStmt) => unit
let ptAddSyntaxProof: (proofTree, expr, proofNode) => unit
let ptGetSyntaxProof: (proofTree, expr) => option<proofNode>
let ptGetAllSyntaxProofs: proofTree => array<(expr,proofNode)>
let ptClearDists: proofTree => unit
let ptGetDbg: proofTree => option<proofTreeDbg>

let ptGetCopyOfNewVars: proofTree => array<expr>
let ptGetCtxDisj: proofTree => disjMutable

let pnGetId: proofNode => int
let pnGetExpr: proofNode => expr
let pnGetProof: proofNode => option<exprSrc>
let pnGetFParents: proofNode => option<array<exprSrc>>
let pnGetEParents: proofNode => array<exprSrc>
let pnAddParent: (proofNode, exprSrc, bool, bool) => unit
let pnIsInvalidFloating: proofNode => bool
let pnSetInvalidFloating: proofNode => unit
let pnGetDist: proofNode => option<int>
let pnSetDist: (proofNode,int) => unit
let pnGetBottomUpProverParams: proofNode => option<bottomUpProverParams>
let pnSetBottomUpProverParams: (proofNode,option<bottomUpProverParams>) => unit
let pnGetDbg: proofNode => option<proofNodeDbg>

let jstfEqSrc: (array<expr>, string, exprSrc) => bool

let ptPrintStats: ( proofTree ) => string