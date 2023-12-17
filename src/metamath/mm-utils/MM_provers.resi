open MM_proof_tree
open MM_context
open MM_substitution
open MM_parenCounter
open MM_wrk_settings

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    frmsToUse: option<array<string>>,
    maxSearchDepth: int,
    lengthRestrict: lengthRestrict,
    allowNewDisjForExistingVars: bool,
    allowNewStmts: bool,
    allowNewVars: bool,
    args0: array<expr>,
    args1: array<expr>,
    maxNumberOfBranches: option<int>,
}

let bottomUpProverParamsMake: (
    ~asrtLabel: string=?,
    ~frmsToUse: array<string>=?,
    ~maxSearchDepth: int=?,
    ~lengthRestrict: lengthRestrict=?,
    ~allowNewDisjForExistingVars: bool=?,
    ~allowNewStmts: bool=?,
    ~allowNewVars: bool=?,
    ~args0: array<expr>=?,
    ~args1: array<expr>=?,
    ~maxNumberOfBranches: int=?,
    ()
) => bottomUpProverParams

let lengthRestrictToStr: lengthRestrict => string
let lengthRestrictFromStr: string => option<lengthRestrict>
let lengthRestrictFromStrExn: string => lengthRestrict

let createProofCtx: (mmContext, array<rootStmt>) => mmContext

let proveFloatings: (
    ~wrkCtx: mmContext,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~floatingsToProve: array<expr>,
    ~parenCnt: parenCnt,
) => proofTree

let proveSyntaxTypes: (
    ~proofTree:proofTree=?,
    ~wrkCtx:mmContext=?,
    ~frms:frms=?,
    ~frameRestrict:frameRestrict,
    ~parenCnt:parenCnt=?,
    ~exprs: array<expr>,
    ~syntaxTypes: array<int>,
    ~onProgress:float=>unit=?,
    ()
) => proofTree


let unifyAll: (
    ~wrkCtx: mmContext,
    ~frms: frms,
    ~rootStmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:bottomUpProverParams=?,
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
    ~syntaxTypes:array<int>=?,
    ~exprsToSyntaxCheck:array<expr>=?,
    ~debugLevel:int=?,
    ~onProgress: string=>unit=?,
    (),
) => proofTree

let makeParenCnt: ( ~ctx:mmContext, ~parens:string, ) => parenCnt