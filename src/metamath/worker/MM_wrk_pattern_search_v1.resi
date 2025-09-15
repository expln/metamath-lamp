type patternModifier = Hyp | Asrt | Exact | Adj
type stmtPattern = {
    flags: array<patternModifier>,
    varPat: array<int>,
    constPat: array<int>,
}

let makeSearchPattern: (~searchStr:string, ~ctx:MM_context.mmContext) => result<array<stmtPattern>, string>

let frameMatchesPattern: (
    ~frame:MM_context.frame, 
    ~searchPattern:array<stmtPattern>, 
    ~mapping:Belt_HashMapInt.t<int>
) => bool

// TEST

let parseSearchStr: string => result<array<(array<patternModifier>, array<string>)>,string>