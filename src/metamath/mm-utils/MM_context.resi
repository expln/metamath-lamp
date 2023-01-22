open MM_parser

type expr = array<int>

type mmContext

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

type disjMutable

type frame = {
    disj: Belt.Map.Int.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    frameVarToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
}

let createContext: (~parent:mmContext=?, ()) => mmContext
let applySingleStmt: (mmContext, stmt) => unit
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let resetToParentContext: mmContext => unit
let loadContext: (mmAstNode, ~initialContext:mmContext=?, ~stopBefore:string=?, ~stopAfter:string=?, 
                    ~expectedNumOfAssertions:int=?, ~onProgress:float=>unit=?, ()) => mmContext

let isConst: (mmContext,string) => bool
let isVar: (mmContext,string) => bool
let isHyp: (mmContext,string) => bool
let isAsrt: (mmContext,string) => bool
let isDisj: (mmContext,int,int) => bool
let getHypothesis: (mmContext,string) => option<hypothesis>
let getMandHyps: (mmContext, expr) => array<hypothesis>
let getAllHyps: mmContext => Belt_MapString.t<hypothesis>
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
let createFrame: (mmContext, string, array<string>, ~skipHyps:bool=?, ~skipFirstSymCheck:bool=?, ()) => (frame, string)

let ctxRemoveFrameDescriptions: mmContext => unit

let generateNewVarNames: (mmContext, array<int>, Belt_MapString.t<string>) => array<string>
let generateNewLabels: (mmContext, ~prefix:string, ~amount:int) => array<string>

let disjMutableMake: unit => disjMutable
let disjAddPair: (disjMutable, int, int) => unit
let disjContains: (disjMutable, int, int) => bool
let disjNumOfGroups: disjMutable => int
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