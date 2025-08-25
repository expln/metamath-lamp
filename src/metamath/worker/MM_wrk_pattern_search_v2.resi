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

type either<'a, 'b> = 
    | Left('a) 
    | Right('b)

type rec symSeq = {
    ordered: bool,
    elems: either<array<sym>, array<symSeq>>,
    mutable minConstMismatchIdx: int,
}

let exprIncludesSeq: (~expr: array<int>, ~seq: symSeq, ~varTypes: array<int>) => bool
