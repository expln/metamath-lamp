open MM_wrk_editor
open MM_parser

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

let getStmtA = (
    a:array<stmtSnapshot>, 
    idx:int,
    ~idxAdd:option<int>,
):option<stmtSnapshot> => {
    switch idxAdd {
        | Some(idxAdd) => {
            if (idx < idxAdd) {
                Some(a[idx])
            } else if (idx == idxAdd) {
                None
            } else {
                Some(a[idx-1])
            }
        }
        | None => Some(a[idx])
    }
}

let getStmtB = (
    b:array<stmtSnapshot>, 
    idx:int,
    ~idxSwap:option<int>,
    ~idxRemove:option<int>,
):option<stmtSnapshot> => {
    switch idxSwap {
        | Some(idxSwap) => {
            if (idx < idxSwap || idxSwap + 1 < idx) {
                Some(b[idx])
            } else if (idx == idxSwap) {
                Some(b[idx+1])
            } else {
                Some(b[idxSwap])
            }
        }
        | None => {
            switch idxRemove {
                | Some(idxRemove) => {
                    if (idx < idxRemove) {
                        Some(b[idx])
                    } else if (idx == idxRemove) {
                        None
                    } else {
                        Some(b[idx-1])
                    }
                }
                | None => Some(b[idx])
            }
        }
    }
}

let findSmallDiffStmt = (
    a:array<stmtSnapshot>, 
    b:array<stmtSnapshot>, 
    ~idxSwap:option<int>,
    ~idxRemove:option<int>,
    ~idxAdd:option<int>,
):array<editorDiff> => {
    let modCnt = ref(if (idxSwap->Belt_Option.isNone) {0} else {1})
    modCnt := modCnt.contents + if (idxRemove->Belt_Option.isNone) {0} else {1}
    modCnt := modCnt.contents + if (idxAdd->Belt_Option.isNone) {0} else {1}
    if (modCnt.contents > 1) {
        raise(MmException({msg:`findSmallDiffStmt: modCnt.contents > 1`}))
    }
    let diffs = []
    switch idxSwap {
        | Some(idxSwap) => {
            diffs->Js.Array2.push(StmtMove({stmtId:a[idxSwap].id, idx:idxSwap+1}))->ignore
        }
        | None => ()
    }
    switch idxRemove {
        | Some(idxRemove) => {
            diffs->Js.Array2.push(StmtRemove({stmtId:a[idxRemove].id}))->ignore
        }
        | None => ()
    }
    switch idxAdd {
        | Some(idxAdd) => {
            diffs->Js.Array2.push(StmtAdd({idx:idxAdd, stmt:b[idxAdd]}))->ignore
        }
        | None => ()
    }
    let maxI = Js.Math.max_int(a->Js_array2.length, b->Js_array2.length) - 1
    for i in 0 to maxI {
        switch getStmtA(a, i, ~idxAdd) {
            | None => ()
            | Some(stmtA) => {
                switch getStmtB(b, i, ~idxSwap, ~idxRemove) {
                    | None => ()
                    | Some(stmtB) => {
                        if (stmtA.id != stmtB.id) {
                            raise(MmException({msg:`stmtA.id != stmtB.id`}))
                        }
                        if (stmtA.label != stmtB.label) {
                            diffs->Js.Array2.push(StmtLabel({stmtId:stmtA.id, label:stmtB.label}))->ignore
                        }
                        if (stmtA.typ != stmtB.typ || stmtA.isGoal != stmtB.isGoal) {
                            diffs->Js.Array2.push(StmtTyp({stmtId:stmtA.id, typ:stmtB.typ, isGoal:stmtB.isGoal}))->ignore
                        }
                        if (stmtA.jstfText != stmtB.jstfText) {
                            diffs->Js.Array2.push(StmtJstf({stmtId:stmtA.id, jstfText:stmtB.jstfText}))->ignore
                        }
                        if (stmtA.cont != stmtB.cont) {
                            diffs->Js.Array2.push(StmtCont({stmtId:stmtA.id, cont:stmtB.cont}))->ignore
                        }
                        if (!(stmtA.proofStatus->proofStatusEq(stmtB.proofStatus))) {
                            diffs->Js.Array2.push(StmtStatus({stmtId:stmtA.id, proofStatus:stmtB.proofStatus}))->ignore
                        }
                    }
                }
            }
        }
    }
    diffs
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
            let idxSwap = ref(None)
            let match = ref(true)
            let i = ref(0)
            while (match.contents && i.contents < aLen) {
                if (a[i.contents].id != b[i.contents].id) {
                    switch idxSwap.contents {
                        | None => {
                            if (i.contents < aLen-1 && a[i.contents].id == b[i.contents+1].id && a[i.contents+1].id == b[i.contents].id) {
                                idxSwap := Some(i.contents)
                            } else {
                                match := false
                            }
                        }
                        | Some(idxSwap) => match := idxSwap + 1 == i.contents
                    }
                }
                i := i.contents + 1
            }
            if (!match.contents) {
                Error(())
            } else {
                let diffs = findSmallDiffStmt( a, b, ~idxSwap=idxSwap.contents, ~idxRemove=None, ~idxAdd=None)
                if (diffs->Js.Array2.length == 0) {
                    Ok(None)
                } else {
                    Ok(Some(diffs))
                }
            }
        } else {
            raise(MmException({msg:`not implemented`}))
        }
    }
}

let findDiff = (a:editorSnapshot, b:editorSnapshot):array<editorDiff> => {
    raise(MmException({msg:`not implemented`}))
}

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff = (a:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {
    raise(MmException({msg:`not implemented`}))
}
