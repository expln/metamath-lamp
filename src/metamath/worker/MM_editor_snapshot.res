open MM_wrk_editor

type stmtSnapshot = {
    id: stmtId,
    label: string,
    typ: userStmtType,
    isGoal: bool,
    jstfText: string,
    cont: string,
    proofStatus: option<proofStatus>,
}

type editorSnapshot = {
    descr: string,
    varsText: string,
    disjText: string,
    stmts: array<stmtSnapshot>,
}

type editorDiff =
    | Snapshot(editorSnapshot)
    | Descr(string)
    | Vars(string)
    | Disj(string)
    | StmtLabel({stmtId:stmtId, label:string})
    | StmtTyp({stmtId:stmtId, typ: userStmtType, isGoal: bool})
    | StmtJstf({stmtId:stmtId, jstfText: string})
    | StmtCont({stmtId:stmtId, cont: string})
    | StmtStatus({stmtId:stmtId, proofStatus: option<proofStatus>})