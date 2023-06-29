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

let proofStatusEq = (a:option<proofStatus>, b:option<proofStatus>):bool => {
    switch a {
        | None => {
            switch b {
                | None => true
                | Some(_) => false
            }
        }
        | Some(aStatus) => {
            switch b {
                | None => false
                | Some(bStatus) => aStatus == bStatus
            }
        }
    }
}

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
        } else {
            Ok(Some(
                b->Js_array2.mapi((stmt,i) => StmtAdd({idx:i, stmt}))
            ))
        }
    } else {
        if (bLen == 0) {
            Ok(Some(
                b->Js_array2.map(stmt => StmtRemove({stmtId:stmt.id}))
            ))
        } else if (aLen == bLen) {
            // let baseIdx = ref(-1)
            // let inc = ref(false)
            // let diffIsSmall = ref(true)
            // let i = ref(0)
            // while (diffIsSmall.contents && i.contents < aLen) {
            //     if (i.contents == 0) {

            //     }
            //     if (a[i.contents].stmt)
            //     if (baseIdx.contents >= 0)
            //     i := i.contents + 1
            // }


            if (a->Js_array2.everyi((aStmt,i) => aStmt.id == b[i].id)) {
                let diffs = []
                for i in 0 to aLen-1 {
                    let aStmt = a[i]
                    let bStmt = b[i]
                    if (aStmt.label != bStmt.label) {
                        diffs->Js.Array2.push(StmtLabel({stmtId:bStmt.id, label:bStmt.label}))->ignore
                    }
                    if (aStmt.typ != bStmt.typ || aStmt.isGoal != bStmt.isGoal) {
                        diffs->Js.Array2.push(StmtTyp({stmtId:bStmt.id, typ:bStmt.typ, isGoal:bStmt.isGoal}))->ignore
                    }
                    if (aStmt.jstfText != bStmt.jstfText) {
                        diffs->Js.Array2.push(StmtJstf({stmtId:bStmt.id, jstfText:bStmt.jstfText}))->ignore
                    }
                    if (aStmt.cont != bStmt.cont) {
                        diffs->Js.Array2.push(StmtCont({stmtId:bStmt.id, cont:bStmt.cont}))->ignore
                    }
                    if (!(aStmt.proofStatus->proofStatusEq(bStmt.proofStatus))) {
                        diffs->Js.Array2.push(StmtStatus({stmtId:bStmt.id, proofStatus:bStmt.proofStatus}))->ignore
                    }
                }
                if (diffs->Js.Array2.length == 0) {
                    Ok(None)
                } else {
                    Ok(Some(diffs))
                }
            }
        } else {

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
