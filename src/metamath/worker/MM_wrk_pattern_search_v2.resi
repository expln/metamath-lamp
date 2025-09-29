type matchResult = 
    | Matched(option<array<array<int>>>) 
    | NotMatched

type variable = {
    typ: int,
    capVars:ref<array<bool>>, //captured variables
    mutable capVar: int, //captured variable
    mutable capVarIdx: int, //index of the first occurrence of the captured variable
}

type constOrVar = Const(int) | Var(variable)

type sym = {
    constOrVar: constOrVar,
    mutable matchedIdx: int, //index of the matched symbol
}

type patternTarget = Frm | Hyps | Asrt

type rec symSeq = {
    elems: seqGrp,
    target: patternTarget,
    singleStmt: bool,
    minLen:int,
    minConstMismatchIdx:array<int>,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)
    | OneOf(array<symSeq>)

type pattern = {
    neg: bool,
    symSeq: symSeq,
    allSeq: array<symSeq>,
    capVars:ref<array<bool>>, //captured variables
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

let exprIncludesSeq: (
    ~expr: array<int>, ~seq: symSeq, ~varTypes: array<int>, ~frmData:MM_context.patternSearchData
) => option<array<int>>

let convertMatchedIndices: (MM_context.frame, array<int>) => array<array<int>>

let mergeMatchedIndices: array<array<array<int>>> => array<array<int>>