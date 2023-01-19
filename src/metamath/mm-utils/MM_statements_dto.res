open MM_context

type newStmtJstfDto = {
    args:array<string>,
    label:string,
}

type newStmtDto = {
    label:string,
    expr:expr,
    exprStr:string,
    jstf:option<newStmtJstfDto>,
}

type newStmtsDto = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    newDisjStr:array<string>,
    stmts: array<newStmtDto>,
}
    