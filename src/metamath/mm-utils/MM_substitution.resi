open MM_parenCounter
open MM_context
open MM_unification_debug

type contunieInstruction = Continue | Stop

type constParts = {
    length: int,
    begins: array<int>,
    ends: array<int>,
    remainingMinLength: array<int>,
}

type varGroup = {
    leftConstPartIdx:int,
    frmExpr:expr,
    varsBeginIdx:int,
    numOfVars:int,
    mutable exprBeginIdx:int,
    mutable exprEndIdx:int
}

type workVars = {
    maxVar:int,
    newVars: array<int>,
    newVarTypes: array<int>,
    hypIdxToExprWithWorkVars: array<option<expr>>,
}

type subs = {
    size: int,
    begins: array<int>,
    ends: array<int>,
    exprs: array<expr>,
    isDefined: array<bool>,
}

type frmSubsData = {
    frame: frame,
    hypsE: array<hypothesis>,
    numOfHypsE:int,
    frmConstParts:array<constParts>,
    constParts:array<constParts>,
    varGroups:array<array<varGroup>>,
    subs:subs,
}

let subsClone: subs => subs
let subsEq: (subs, subs) => bool
let createConstParts: expr => constParts
let createMatchingConstParts: constParts => constParts
let createVarGroups: (~frmExpr:expr, ~frmConstParts:constParts) => array<varGroup>
let createSubs: (~numOfVars:int) => subs
let prepareFrmSubsDataForFrame: frame => frmSubsData
let prepareFrmSubsData: (
    ~ctx:mmContext,
    ~asrtsToSkip:Belt_HashSetString.t=?,
    ()
) => Belt_MapString.t<frmSubsData>
let iterateSubstitutions: (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~varGroups:array<varGroup>,
    ~subs:subs,
    ~parenCnt:parenCnt,
    ~consumer: subs => contunieInstruction
) => contunieInstruction
let applySubs: (~frmExpr:expr, ~subs:subs, ~createWorkVar:int=>int) => expr
let verifyDisjoints: (
    ~frmDisj:Belt_MapInt.t<Belt_SetInt.t>, 
    ~subs:subs, 
    ~isDisjInCtx:(int,int)=>bool,
    ~debugLevel:int,
) => option<unifErr>

//------------------------- TEST ---------------------------

let test_iterateConstParts: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr, ~parens:string) => (array<(int,int)>, array<array<(int,int)>>)
let test_iterateSubstitutions: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr, ~parens:string) => array<array<expr>>