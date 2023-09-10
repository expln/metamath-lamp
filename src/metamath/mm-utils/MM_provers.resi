open MM_proof_tree
open MM_context
open MM_substitution
open MM_parenCounter
open MM_wrk_settings

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
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

let createProofCtx: (mmContext, array<rootStmt>) => mmContext

let proveFloatings: (
    ~wrkCtx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~syntaxTypes: array<int>,
    ~frameRestrict:frameRestrict,
    ~floatingsToProve: array<expr>,
    ~parenCnt: parenCnt,
) => proofTree

let proveSyntaxTypes: (
    ~proofTree:proofTree=?,
    ~wrkCtx:mmContext=?,
    ~frms:Belt_MapString.t<frmSubsData>=?,
    ~frameRestrict:frameRestrict,
    ~parenCnt:parenCnt=?,
    ~exprs: array<expr>,
    ~syntaxTypes: array<int>,
    ~onProgress:float=>unit=?,
    ()
) => proofTree


let unifyAll: (
    ~wrkCtx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~rootStmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:bottomUpProverParams=?,
    ~allowedFrms:allowedFrms,
    ~syntaxTypes:array<int>,
    ~exprsToSyntaxCheck:array<expr>=?,
    ~debugLevel:int=?,
    ~onProgress: string=>unit=?,
    (),
) => proofTree

