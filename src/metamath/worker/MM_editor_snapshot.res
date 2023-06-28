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
    | StmtAdd({idx:int, stmt:stmtSnapshot})
    | StmtRemove({stmtId:stmtId})
    | StmtMove({stmtId:stmtId, idx:int})
    | StmtLabel({stmtId:stmtId, label:string})
    | StmtTyp({stmtId:stmtId, typ: userStmtType, isGoal: bool})
    | StmtJstf({stmtId:stmtId, jstfText: string})
    | StmtCont({stmtId:stmtId, cont: string})
    | StmtStatus({stmtId:stmtId, proofStatus: option<proofStatus>})

/* 
Ok(None): a and b are same;
Ok(Some): some diff is found;
Error: a and b are different, but the function cannot explain the difference in terms of the editorDiff type.
 */
let findDiffStmt = (a:array<stmtSnapshot>, b:array<stmtSnapshot>):result<option<array<editorDiff>>,unit> => {
    let aLen = a->Js_array2.length
    let bLen = b->Js_array2.length
    if (aLen == 0) {
        if (bLen == 0) {
            Ok(None)
        }
    }
}

let findDiff = (a:editorSnapshot, b:editorSnapshot):array<editorDiff> => {

}

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff = (a:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {

}
