open MM_context
open MM_proof_table
open MM_proof_tree
open MM_unification_debug

type exprSrcDto =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})
    | AssertionWithErr({args:array<int>, label:string, err:unifErr})

type proofNodeDbgDto = {
    exprStr:string,
}

type proofTreeDbgDto = {
    newVars: array<string>,
}

type proofNodeDto = {
    expr:expr,
    parents: array<exprSrcDto>,
    proof: option<exprSrcDto>,
    dbg:option<proofNodeDbgDto>,
}

type proofTreeDto = {
    newVars: array<expr>,
    nodes: array<proofNodeDto>,
    syntaxProofs: array<(expr,proofNodeDto)>,
    dbg: option<proofTreeDbgDto>,
}

let proofTreeToDto: (proofTree, array<expr>) => proofTreeDto

let createProofTable: (
    ~tree:proofTreeDto, 
    ~root:proofNodeDto, 
    ~essentialsOnly:bool=?,
    ~ctx:mmContext=?
) => proofTable
