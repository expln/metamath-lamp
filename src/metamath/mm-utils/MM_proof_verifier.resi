open MM_parser
open MM_context

type proofNodeDbg = {
    exprStr: string,
}

type rec proofNode =
    | Hypothesis({id:int, hypLabel:string, expr:expr, dbg:option<proofNodeDbg>})
    | Calculated({id:int, args:array<proofNode>, asrtLabel:string, expr:expr, dbg:option<proofNodeDbg>})

let proofNodeGetId: proofNode => int
let proofNodeGetExpr: proofNode => expr
let verifyProof: (
    ~ctx:mmContext, 
    ~expr:expr, 
    ~proof:proof,
    ~isDisjInCtx: (int,int) => bool,
) => proofNode

let compressedProofBlockToArray: string => array<string>
let compressedProofStrToInt: string => int
let intToCompressedProofStr: int => string
