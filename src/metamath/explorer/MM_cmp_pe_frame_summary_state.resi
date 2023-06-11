

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
) => state
let toggleDescrIsExpanded: state => state