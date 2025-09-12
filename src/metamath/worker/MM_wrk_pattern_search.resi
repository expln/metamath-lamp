type pattern

type matchResult = 
    | Matched(option<array<array<int>>>) 
    | NotMatched

let parsePattern: (~patternStr:string, ~patternVersion:int, ~ctx:MM_context.mmContext) => result<pattern,string>

let frameMatchesPattern: (MM_context.frame, pattern) => matchResult

let patternIsEmpty: pattern => bool