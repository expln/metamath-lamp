open MM_context

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
}

let prepareParenInts: (mmContext, string) => array<int>

let textToVarDefs: string => result<array<array<string>>,string>

let createWrkCtx: (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
) => result<mmContext, wrkCtxErr>


