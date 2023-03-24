open MM_parser

type expr = array<int>

type mmContext

type hypothesisType = F | E

type tokenType = C | V | F | E | A | P

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

type disjMutable

type frameDbg = {
    disj: array<string>,
    hyps: array<string>,
    asrt: string,
}

type frame = {
    disj: Belt.Map.Int.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    frameVarToSymb: array<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
    descr:option<string>,
    proof:option<proof>,
    dbg: option<frameDbg>,
}

let createContext: (~parent:mmContext=?, ~debug:bool=?, ()) => mmContext
let applySingleStmt: (mmContext, stmt) => unit
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let resetToParentContext: mmContext => unit
let loadContext: (
    mmAstNode, 
    ~initialContext:mmContext=?, 
    ~stopBefore:string=?, 
    ~stopAfter:string=?, 
    ~onPreProcess: (mmContext,MM_parser.stmt)=>unit =?,
    ~expectedNumOfAssertions:int=?, 
    ~onProgress:float=>unit=?, 
    ~debug:bool=?, 
    ()
) => mmContext

let isDebug: mmContext => bool
let isConst: (mmContext,string) => bool
let isVar: (mmContext,string) => bool
let isHyp: (mmContext,string) => bool
let isAsrt: (mmContext,string) => bool
let isDisj: (mmContext,int,int) => bool
let getHypothesis: (mmContext,string) => option<hypothesis>
let getHypByExpr: (mmContext, expr) => option<hypothesis>
let getMandHyps: (mmContext, expr) => array<hypothesis>
let getAllHyps: mmContext => Belt_MapString.t<hypothesis>
let getAllFrames: mmContext => Belt_MapString.t<frame>
let getAllDisj: mmContext => disjMutable
let getLocalDisj: mmContext => disjMutable
let getFrame: (mmContext,string) => option<frame>
let getLocalVars: mmContext => array<string>
let getLocalHyps: mmContext => array<hypothesis>
let getNumOfVars: mmContext => int
let forEachFrame: (mmContext, frame => option<'a>) => option<'a>
let forEachHypothesisInDeclarationOrder: (mmContext, hypothesis => option<'a>) => option<'a>
let getNestingLevel: mmContext => int
let findParentheses: (mmContext, ~onProgress:float=>unit=?, unit) => array<int>
let getTypeOfVar: (mmContext, int) => option<int>
let getTypeOfVarExn: (mmContext, int) => int
let getTokenType: (mmContext, string) => option<tokenType>

let ctxSymToInt: (mmContext, string) => option<int>
let ctxIntToSym: (mmContext, int) => option<string>
let ctxSymToIntExn: (mmContext, string) => int
let ctxSymsToIntsExn: (mmContext,array<string>) => expr
let ctxStrToIntsExn: (mmContext, string) => expr
let ctxIntToSymExn: (mmContext, int) => string
let ctxIntsToSymsExn: (mmContext, expr) => array<string>
let ctxIntsToStrExn: (mmContext, expr) => string
let frmIntToSymExn: (mmContext, frame, int) => string
let frmIntsToStrExn: (mmContext, frame, expr) => string
let createFrame: (
    ~ctx:mmContext, 
    ~label:string, 
    ~exprStr:array<string>, 
    ~proof:option<proof>,
    ~tokenType:string=?,
    ~skipEssentials:bool=?, 
    ~skipFirstSymCheck:bool=?, 
    ()
) => frame

let moveConstsToBegin: (mmContext, string) => unit

let generateNewVarNames: ( 
    ~ctx: mmContext, 
    ~types: array<int>, 
    ~typeToPrefix: Belt_MapString.t<string>, 
    ~reservedNames: Belt_HashSetString.t=?, 
    unit
) => array<string>

let generateNewLabels: (
    ~ctx: mmContext,
    ~prefix: string,
    ~amount: int,
    ~reservedLabels: Belt_HashSetString.t=?,
    ~checkHypsOnly:bool=?,
    unit,
) => array<string>

let disjMake: unit => disjMutable
let disjAddPair: (disjMutable, int, int) => unit
let disjContains: (disjMutable, int, int) => bool
let disjToArr: disjMutable => array<array<int>>
let disjForEachArr: (disjMutable, array<int> => unit) => unit
let disjForEach: (disjMutable, (int,int) => unit) => unit
let disjIsEmpty: disjMutable => bool

let exprEq: (expr,expr) => bool

module ExprCmp: {
    type identity
    type t = expr
    let cmp: Belt_Id.cmp<t, identity>
}

module ExprHash: {
    type identity
    type t = expr
    let hash: Belt_Id.hash<t, identity>
    let eq: Belt_Id.eq<t, identity>
}
