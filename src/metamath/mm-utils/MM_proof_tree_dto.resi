open MM_context
open MM_proof_table
open MM_proof_tree
open MM_unification_debug

type exprSourceDto =
    | VarType
    | Hypothesis({label: string})
    | Assertion({args:array<int>, label:string, missingDisj:option<disjMutable>})
    | AssertionWithErr({args:array<int>, label:string, err:unifErr})

type proofNodeDto = {
    expr: expr,
    exprStr: option<string>,
    parents: option<array<exprSourceDto>>,
    proof: option<exprSourceDto>,
}

type dbgProofTreeDto = {
    newVars: array<string>,
    disj: array<string>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofNodeDto>,
    dbg: option<dbgProofTreeDto>,
}

let proofTreeToDto: (~tree:proofTree, ~stmts:array<expr>, ~debugLevel:int) => proofTreeDto

let createProofTable: (proofTreeDto, proofNodeDto) => proofTable
