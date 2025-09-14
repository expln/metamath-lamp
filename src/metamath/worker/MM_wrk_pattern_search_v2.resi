type matchResult = 
    | Matched(option<array<array<int>>>) 
    | NotMatched

type variable = {
    typ: int,
    mutable capVar: int, //captured variable
    mutable capVarIdx: int, //index of the first occurrence of the captured variable
}

type rec sym = {
    constOrVar: constOrVar,
    mutable matchedIdx: int, //index of the matched symbol
}
and constOrVar = Const(int) | Var(variable)

type rec symSeq = {
    elems: seqGrp,
    minLen:int,
    mutable minConstMismatchIdx:int,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

type patternTarget = Frm | Hyps | Asrt

type pattern = {
    target: patternTarget,
    symSeq: symSeq,
    allSeq: array<symSeq>,
}

let parsePattern: (
    string, 
    ~symMap:Belt_HashMapString.t<constOrVar>=?,
    ~ctx:MM_context.mmContext=?
) => result<array<pattern>,string>

let frameMatchesPatterns: (MM_context.frame, array<pattern>) => matchResult

//FOR TESTING

let validatePattern: (~text:string, ~ctx:MM_context.mmContext) => option<string>

let countMinLen: array<symSeq> => int

let exprIncludesSeq: (~expr: array<int>, ~seq: symSeq, ~varTypes: array<int>) => option<array<int>>

let convertMatchedIndices: (MM_context.frame, array<int>, patternTarget) => array<array<int>>

let mergeMatchedIndices: array<array<array<int>>> => array<array<int>>