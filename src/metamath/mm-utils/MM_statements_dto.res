open MM_context

type jstf = {
    args: array<string>,
    label: string
}

type stmtDto = {
    label:string,
    expr:expr,
    exprStr:string,
    jstf:option<jstf>,
    isProved: bool,
}

type stmtsDto = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    newDisjStr:array<string>,
    stmts: array<stmtDto>,
}
    
let jstfEq = (a:jstf, b:jstf):bool => {
    a.label == b.label && a.args == b.args
}