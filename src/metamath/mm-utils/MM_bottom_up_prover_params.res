open MM_context
open Common
open MM_apply_asrt_matcher_type

//customParams is an object, but {..} produces a compiler error "In type ({..} as 'a) the variable 'a is unbound".
//So, string is used instead of {..}
type customParams = string

type lengthRestrict = No | LessEq | Less

type bottomUpProverFrameParams = {
    minDist: option<int>,
    maxDist: option<int>,
    frmsToUse: option<array<string>>,
    matches: option<array<applyAsrtResultMatcher>>,
    deriveFrom: array<expr>,
    allowNewDisjForExistingVars: bool,
    allowNewStmts: bool,
    allowNewVars: bool,
    lengthRestrict: lengthRestrict,
    maxNumberOfBranches: option<int>,
}

type rec bottomUpProverParams = {
    maxSearchDepth: int, 
    frameParams: array<bottomUpProverFrameParams>,
    updateParams: option<updateParams>,
} 
and updateParams = (
    proverParams,
    expr, /* expr */
    int, /* dist */
    int=>option<string>, /* proofCtxIntToSymOpt */
    string=>option<int>, /* symToProofCtxIntOpt */
) => option<proverParams>
and proverParams = {
    bottomUpProverParams:option<bottomUpProverParams>,
    customParams:option<customParams>,
}

let bottomUpProverParamsMakeDefault = (
    ~asrtLabel: option<string>=?,
    ~maxSearchDepth: int=4,
    ~lengthRestrict: lengthRestrict=Less,
    ~allowNewDisjForExistingVars: bool=true,
    ~allowNewStmts: bool=true,
    ~allowNewVars: bool=false,
    ~deriveFromOnLevel0: array<expr>=[],
    ~deriveFromOnLevel1: array<expr>=[],
    ~maxNumberOfBranches: option<int>=?
):bottomUpProverParams => {
    {
        maxSearchDepth,
        frameParams: [
            {
                minDist: Some(0),
                maxDist: Some(0),
                frmsToUse: asrtLabel->Belt_Option.map(label => [label]),
                matches: None,
                deriveFrom: deriveFromOnLevel0,
                allowNewDisjForExistingVars,
                allowNewStmts,
                allowNewVars: allowNewVars,
                lengthRestrict: No,
                maxNumberOfBranches,
            },
            {
                minDist: Some(1),
                maxDist: None,
                matches: None,
                frmsToUse: None,
                deriveFrom: deriveFromOnLevel1,
                allowNewDisjForExistingVars,
                allowNewStmts,
                allowNewVars: false,
                lengthRestrict: lengthRestrict,
                maxNumberOfBranches,
            }
        ],
        updateParams: None,
    }
}

let lengthRestrictToStr = (len:lengthRestrict) => {
    switch len {
        | No => "No"
        | LessEq => "LessEq"
        | Less => "Less"
    }
}
let lengthRestrictFromStr = (str:string):option<lengthRestrict> => {
    switch str {
        | "No" => Some(No)
        | "LessEq" => Some(LessEq)
        | "Less" => Some(Less)
        | _ => None
    }
}
let lengthRestrictFromStrExn = (str:string):lengthRestrict => {
    switch lengthRestrictFromStr(str) {
        | Some(v) => v
        | None => raise(MmException({msg:`Cannot convert '${str}' to lengthRestrict.`}))
    }
}

