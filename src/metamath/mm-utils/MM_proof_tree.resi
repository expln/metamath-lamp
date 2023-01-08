open MM_context
open MM_parenCounter
open MM_substitution
open MM_proof_table

type justification = {
    args: array<string>,
    asrt: string
}

type rootStmt = {
    label: option<string>,
    expr: expr,
    justification: option<justification>,
}

type rec proofTreeNode = {
    expr:expr,
    exprStr:option<string>,//debug
    label: option<string>,
    mutable parents: option<array<exprSource>>,
    mutable children: array<proofTreeNode>,
    mutable proof: option<exprSource>,
}
and exprSource =
    | VarType
    | Hypothesis({label: string})
    | Assertion({args: array<proofTreeNode>, label: string, subs: option<array<array<string>>>/*debug*/})

type proofTree = {
    frms: Belt_MapString.t<frmSubsData>,
    hypsByExpr: Belt_Map.t<expr,hypothesis,ExprCmp.identity>,
    hypsByLabel: Belt_MapString.t<hypothesis>,
    mutable maxVar:int,
    newVars: Belt_MutableSet.t<expr,ExprCmp.identity>,
    disj: disjMutable,
    parenCnt:parenCnt,
    rootNodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
    nodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofTreeNode>,
}

let proofTreeProve: (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~bottomUp: bool=?,
    ~syntaxProof:bool,
    ~parenCnt: parenCnt,
    ~onProgress: float=>unit = ?,
    ~debug: bool=?,
    ()
) => proofTree

let proofTreeCreateProofTable: proofTreeNode => proofTable
