open MM_context
open MM_parser
open MM_proof_verifier

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proof:exprSource,
}

type proofTable = array<proofRecord>

let createProof: (mmContext, proofTable, int) => proof
let createProofTableFromProof: proofNode => proofTable

let proofTablePrint: (mmContext, proofTable, string) => unit