open MM_parser
open MM_context
open MM_proof_table
open MM_proof_tree

type exprSourceDto =
    | VarType
    | Hypothesis({label: string})
    | Assertion({args: array<int>, label: string})

type proofNodeDto = {
    expr: expr,
    exprStr: option<string>,
    parents: option<array<exprSourceDto>>,
    proof: option<exprSourceDto>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofNodeDto>,
}

let proofTreeToDto: (proofTree, array<expr>) => proofTreeDto

let createProofTable: (proofTreeDto, proofNodeDto) => proofTable
