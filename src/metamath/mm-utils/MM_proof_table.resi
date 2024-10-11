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

let createProof: (array<hypothesis>, proofTable, int) => proof
let createProofTableFromProof: (~proofNode:proofNode, ~mergeSameRows:bool=?) => proofTable

let proofTableToArrStr: (mmContext, proofTable) => array<string>
let proofTableToStr: (mmContext, proofTable, string) => string
let proofTablePrint: (mmContext, proofTable, string) => unit