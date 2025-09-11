module PS1 = MM_wrk_pattern_search_v1
module PS2 = MM_wrk_pattern_search_v2

type pattern =
    | V1({pattern:array<PS1.stmtPattern>, mapping:Belt_HashMapInt.t<int>})
    | V2(array<PS2.pattern>)

type matchResult =
    | Matched
    | MatchedIndices(array<array<int>>)
    | NotMatched

let frameMatchesPatterns = (frame:MM_context.frame, pattern:pattern):matchResult => {
    switch pattern {
        | V1({pattern, mapping}) => {
            if (PS1.frameMatchesPattern(~frame, ~searchPattern=pattern, ~mapping)) {
                Matched
            } else {
                NotMatched
            }
        }
        | V2(pattern) => {
            switch PS2.frameMatchesPatterns(frame, pattern) {
                | Some(matchedIndices) => MatchedIndices(matchedIndices)
                | None => NotMatched
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
