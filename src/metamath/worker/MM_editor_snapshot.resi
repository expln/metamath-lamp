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
    | Descr(string)
    | Vars(string)
    | Disj(string)
    | StmtLabel({stmtId: stmtId, label: string})
    | StmtTyp({stmtId: stmtId, typ: userStmtType, isGoal: bool})
    | StmtJstf({stmtId: stmtId, jstfText: string})
    | StmtCont({stmtId: stmtId, cont: string})
    | StmtStatus({stmtId: stmtId, proofStatus: option<proofStatus>})
    | StmtAdd({idx: int, stmt: stmtSnapshot})
    | StmtRemove({stmtId: stmtId})
    | StmtMove({stmtId: stmtId, idx: int})
    | Snapshot(editorSnapshot)

let findDiff: (editorSnapshot, editorSnapshot) => array<editorDiff>

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff: (editorSnapshot, array<editorDiff>) => editorSnapshot
