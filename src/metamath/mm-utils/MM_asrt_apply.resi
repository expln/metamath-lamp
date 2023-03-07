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
    err:option<unifErr>,
}

let applyAssertionResultEq: (applyAssertionResult, applyAssertionResult) => bool

let applyAssertionResultHash: applyAssertionResult => int

let applyAssertions: (
    ~maxVar:int,
    ~frms:Belt_MapString.t<frmSubsData>,
    ~isDisjInCtx:(int,int)=>bool,
    ~statements:array<expr>,
    ~exactOrderOfStmts:bool=?,
    ~allowEmptyArgs:bool=?,
    ~result:expr=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frame=>bool=?,
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