type variable = {
    mutable capVar: int, 
    mutable capVarIdx: int
}
type sym = {
    isConst: bool, 
    constOrType: int, 
    var: variable, 
    mutable matchedIdx: int
}

type rec symSeq = {
    elems: seqGrp,
    mutable minConstMismatchIdx:int,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

let exprIncludesSeq: (~expr: array<int>, ~seq: symSeq, ~varTypes: array<int>) => bool
