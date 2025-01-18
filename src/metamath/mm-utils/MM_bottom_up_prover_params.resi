open MM_context
open MM_apply_asrt_matcher_type

type customParams

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
    @as("customParams") customParams:option<customParams>,
    @as("maxSearchDepth") maxSearchDepth: int, 
    @as("assertionParams") frameParams: array<bottomUpProverFrameParams>,
    @as("updateParamsStr") updateParamsStr: option<string>,
    @as("updateParams") updateParams: option<updateParams>,
} 
and updateParams = (
    bottomUpProverParams,
    expr, /* expr */
    int, /* dist */
    int=>option<string>, /* proofCtxIntToSymOpt */
    string=>option<int>, /* symToProofCtxIntOpt */
) => option<bottomUpProverParams>

let jsonToCustomParams: JSON.t => customParams

let bottomUpProverParamsMakeDefault: (
    ~asrtLabel: string=?,
    ~maxSearchDepth: int=?,
    ~lengthRestrict: lengthRestrict=?,
    ~allowNewDisjForExistingVars: bool=?,
    ~allowNewStmts: bool=?,
    ~allowNewVars: bool=?,
    ~deriveFromOnLevel0: array<MM_context.expr>=?,
    ~deriveFromOnLevel1: array<MM_context.expr>=?,
    ~maxNumberOfBranches: int=?,
) => bottomUpProverParams

let lengthRestrictToStr: lengthRestrict => string
let lengthRestrictFromStr: string => option<lengthRestrict>
let lengthRestrictFromStrExn: string => lengthRestrict
