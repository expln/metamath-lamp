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
}

let exprIncludesSeq: (~expr: array<int>, ~seq: symSeq, ~varTypes: array<int>) => bool

let getMatchedIndices: symSeq => array<int>

let parsePattern: (string, Belt_HashMapString.t<sym>) => result<array<pattern>,string>