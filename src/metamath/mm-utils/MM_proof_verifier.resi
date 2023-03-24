open MM_parser
open MM_context

type proofNodeDbg = {
    exprStr: string,
}

type rec proofNode =
    | Hypothesis({hypLabel:string, expr:expr, dbg:option<proofNodeDbg>})
    | Calculated({args:array<proofNode>, asrtLabel:string, expr:expr, dbg:option<proofNodeDbg>})

let verifyProof: (mmContext, expr, proof) => proofNode
let getExprFromNode: proofNode => expr

let compressedProofBlockToArray: string => array<string>
let compressedProofStrToInt: string => int
let intToCompressedProofStr: int => string
