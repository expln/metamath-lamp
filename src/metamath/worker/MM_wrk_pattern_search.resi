type pattern

type matchedIndices = array<array<int>>

type matchResult = 
    | Matched(option<matchedIndices>) 
    | NotMatched

let parsePattern: (~patternStr:string, ~patternVersion:int, ~ctx:MM_context.mmContext) => result<pattern,string>

let frameMatchesPattern: (MM_context.frame, pattern) => matchResult

let patternIsEmpty: pattern => bool