

type state = {
    frmCtx: MM_context.mmContext,
    symColors: Belt_HashMapString.t<string>,
    eHyps: array<MM_context.expr>,
    asrt: MM_context.expr,
    symRename: option<Belt_HashMapString.t<string>>,
    descrIsExpanded: bool,
    disj: option<array<array<(string,option<string>)>>>,
}
let makeInitialState: (
    ~preCtx: MM_context.mmContext,
    ~frame: MM_context.frame,
    ~typeColors: Belt_HashMapString.t<string>,
    ~typeOrderInDisj:Belt_HashMapInt.t<int>,
) => state
let toggleDescrIsExpanded: state => state

let createDisjGroups: (
    ~disj:Belt_MapInt.t<Belt_SetInt.t>,
    ~intToSym: int => (string,option<string>),
    ~ctx:MM_context.mmContext,
    ~typeOrder:Belt_HashMapInt.t<int>,
) => array<array<(string,option<string>)>>

let disjGrpDelim: string
let rndDisj: array<array<(string,option<string>)>> => React.element