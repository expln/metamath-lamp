open MM_context
open MM_substitution
open MM_parenCounter

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

let createWrkCtx: (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
) => result<mmContext, wrkCtxErr>


