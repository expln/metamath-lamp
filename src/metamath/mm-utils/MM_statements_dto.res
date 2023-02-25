open MM_context

type stmtJstfDto = {
    args:array<string>,
    label:string,
}

type stmtDto = {
    label:string,
    expr:expr,
    exprStr:string,
    jstf:option<stmtJstfDto>,
    isProved: bool,
}

type stmtsDto = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    newDisjStr:array<string>,
    stmts: array<stmtDto>,
}
    