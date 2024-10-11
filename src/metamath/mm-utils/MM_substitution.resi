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

type frms

let subsClone: subs => subs
let subsEq: (subs, subs) => bool
let subsHash: subs => int
let createConstParts: expr => constParts
let createMatchingConstParts: constParts => constParts
let createSubs: (~numOfVars:int) => subs
let prepareFrmSubsDataForFrame: frame => frmSubsData
let prepareFrmSubsData: (
    ~ctx:mmContext
) => frms
let sortFrames: (frms, array<string>) => array<string>
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

let frmsEmpty: unit => frms
let frmsSize: frms => int
let frmsForEach: (frms, ~typ:int=?, frmSubsData=>unit) => unit
let frmsGetAll: frms => array<frmSubsData>
let frmsGetByLabel: (frms, string) => option<frmSubsData>
let frmsGetByType: (frms, int) => option<array<frmSubsData>>
let frmsSelect: (frms, ~typ:int=?, ~label:string=?) => array<frmSubsData>
let frmsGetAllTypes: frms => array<int>
let frmsGetAllGroupedByLabel: frms => Belt_HashMapString.t<frmSubsData>

//------------------------- TEST ---------------------------

let test_iterateConstParts: (~frmExpr:expr, ~expr:expr, ~parenCnt:parenCnt) => (array<(int,int)>, array<array<(int,int)>>)
let test_iterateSubstitutions: (~frmExpr:expr, ~expr:expr, ~parenCnt:parenCnt) => array<array<expr>>