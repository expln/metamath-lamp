module PS1 = MM_wrk_pattern_search_v1
module PS2 = MM_wrk_pattern_search_v2

type pattern =
    | V1({pattern:array<PS1.stmtPattern>, mapping:Belt_HashMapInt.t<int>})
    | V2(array<PS2.pattern>)

type matchedIndices = array<array<int>>

type matchResult =
    | Matched(option<matchedIndices>)
    | NotMatched

let frameMatchesPattern = (frame:MM_context.frame, pattern:pattern):matchResult => {
    switch pattern {
        | V1({pattern, mapping}) => {
            if (PS1.frameMatchesPattern(~frame, ~searchPattern=pattern, ~mapping)) {
                Matched(None)
            } else {
                NotMatched
            }
        }
        | V2(pattern) => {
            switch PS2.frameMatchesPatterns(frame, pattern) {
                | Matched(matchedIndices) => Matched(matchedIndices)
                | NotMatched => NotMatched
            }
        }
    }
}

let parsePattern = (~patternStr:string, ~patternVersion:int, ~ctx:MM_context.mmContext):result<pattern,string> => {
    if (patternVersion == 1) {
        PS1.makeSearchPattern(~searchStr=patternStr, ~ctx)->Result.map(pat => {
            V1({pattern:pat, mapping:Belt_HashMapInt.make(~hintSize=10)})
        })
    } else if (patternVersion == 2) {
        PS2.parsePattern(patternStr, ~ctx)->Result.map(pat => V2(pat))
    } else {
        Error(`Unexpected pattern version ${patternVersion->Int.toString}`)
    }
}

let patternIsEmpty = (pattern:pattern):bool => {
    switch pattern {
        | V1({pattern}) => pattern->Array.length == 0
        | V2(pattern) => pattern->Array.length == 0
    }
}