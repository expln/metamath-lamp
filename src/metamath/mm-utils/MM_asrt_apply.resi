open MM_substitution
open MM_context
open MM_parenCounter
open MM_unification_debug

type applyAssertionResult = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    asrtLabel: string,
    subs: subs,
    missingDisj:option<disjMutable>,
    err:option<unifErr>,
}

let applyAssertionResultEq: (applyAssertionResult, applyAssertionResult) => bool

let applyAssertionResultHash: applyAssertionResult => int

module ApplyAssertionResultHash: {
    type identity
    type t = applyAssertionResult
    let hash: Belt_Id.hash<t, identity>
    let eq: Belt_Id.eq<t, identity>
}

let applyAssertions: (
    ~maxVar:int,
    ~frms:Belt_MapString.t<frmSubsData>,
    ~isDisjInCtx:(int,int)=>bool,
    ~statements:array<expr>,
    ~exactOrderOfStmts:bool=?,
    ~allowEmptyArgs:bool=?,
    ~allowNewVars:bool=?,
    ~result:expr=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frame=>bool=?,
    ~strictDisj:bool=?,
    ~onMatchFound:applyAssertionResult=>contunieInstruction,
    ~debugLevel:int=?,
    ~onProgress:float=>unit=?,
    ()
) => unit


// ------ TEST ----------------------

let iterateCombinations: (
    ~numOfStmts:int,
    ~numOfHyps:int,
    ~stmtCanMatchHyp:(int,int)=>bool,
    ~combinationConsumer:array<int>=>contunieInstruction,
) => contunieInstruction