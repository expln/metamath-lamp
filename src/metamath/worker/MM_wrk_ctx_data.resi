open MM_context

type wrkCtxHyp = {
    id: string,
    label: string, 
    text: string
}

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
    hypErr: option<(string, string)>,
}

let prepareParenInts: (mmContext, string) => array<int>

let textToVarDefs: string => result<array<array<string>>,string>

let createWrkCtx: (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
) => result<mmContext, wrkCtxErr>


