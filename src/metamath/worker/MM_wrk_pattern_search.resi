type pattern

type matchResult = 
    | Matched 
    | MatchedIndices(array<array<int>>) 
    | NotMatched

let parsePattern: (~patternStr:string, ~patternVersion:int, ~ctx:MM_context.mmContext) => result<pattern,string>

let frameMatchesPatterns: (MM_context.frame, pattern) => matchResult