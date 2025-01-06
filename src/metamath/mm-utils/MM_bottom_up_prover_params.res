open MM_context
open Common
open MM_apply_asrt_matcher_type

//customParams is an object, but {..} produces a compiler error "In type ({..} as 'a) the variable 'a is unbound".
//So, string is used instead of {..}
type customParams = string

type lengthRestrict = 
    | @as("No") No 
    | @as("LessEq") LessEq 
    | @as("Less") Less

type bottomUpProverFrameParams = {
    @as("minDist") minDist: option<int>,
    @as("maxDist") maxDist: option<int>,
    @as("assertionsToUse") frmsToUse: option<array<string>>,
    @as("matches") matches: option<array<applyAsrtResultMatcher>>,
    @as("deriveFrom") deriveFrom: array<expr>,
    @as("allowNewDisjointsForExistingVariables") allowNewDisjForExistingVars: bool,
    @as("allowNewStatements") allowNewStmts: bool,
    @as("allowNewVariables") allowNewVars: bool,
    @as("statementLengthRestriction") lengthRestrict: lengthRestrict,
    @as("maxNumberOfBranches") maxNumberOfBranches: option<int>,
}

type rec bottomUpProverParams = {
    @as("maxSearchDepth") maxSearchDepth: int, 
    @as("assertionParams") frameParams: array<bottomUpProverFrameParams>,
    @as("updateParamsStr") updateParamsStr: option<string>,
    @as("updateParams") updateParams: option<updateParams>,
} 
and updateParams = (
    proverParams,
    expr, /* expr */
    int, /* dist */
    int=>option<string>, /* proofCtxIntToSymOpt */
    string=>option<int>, /* symToProofCtxIntOpt */
) => option<proverParams>
and proverParams = {
    @as("bottomUpProverParams") bottomUpProverParams:option<bottomUpProverParams>,
    @as("customParams") customParams:option<customParams>,
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
        updateParamsStr: None,
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

