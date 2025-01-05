open MM_context

type customParams

type lengthRestrict = No | LessEq | Less

type bottomUpProverFrameParams = {
    minDist: option<int>,
    maxDist: option<int>,
    frmsToUse: option<array<string>>,
    matches: option<array<MM_apply_asrt_matcher_type.applyAsrtResultMatcher>>,
    deriveFrom: array<MM_context.expr>,
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
