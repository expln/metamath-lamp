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

type rec editorHistoryNode = {
    mutable prev:option<editorHistoryNode>,
    diff:array<editorDiff>,
}

type editorHistory = {
    head: editorSnapshot,
    prev: option<editorHistoryNode>,
    maxLength: int,
}

let editorSnapshotMake = (st:editorState):editorSnapshot => {
    {
        descr: st.descr,
        varsText: st.varsText,
        disjText: st.disjText,
        stmts: st.stmts->Js.Array2.map(stmt => {
            {
                id: stmt.id,
                label: stmt.label,
                typ: stmt.typ,
                isGoal: stmt.isGoal,
                jstfText: stmt.jstfText,
                cont: stmt.cont->contToStr,
                proofStatus: stmt.proofStatus,
            }
        })
    }
}

let updateEditorStateFromSnapshot = (st:editorState, sn:editorSnapshot):editorState => {
    {
        ...st,
        descr: sn.descr,
        varsText: sn.varsText,
        disjText: sn.disjText,
        stmts: sn.stmts->Js.Array2.map(stmt => {
            {
                id: stmt.id,

                label: stmt.label,
                labelEditMode: false,
                typ: stmt.typ,
                typEditMode: false,
                isGoal: stmt.isGoal,
                cont: stmt.cont->strToCont(
                    ~preCtxColors=st.preCtxColors,
                    ~wrkCtxColors=st.wrkCtxColors,
                    ()
                ),
                contEditMode: false,
                
                jstfText: stmt.jstfText,
                jstfEditMode: false,

                stmtErr: None,

                expr: None,
                jstf: None,
                proofTreeDto: None,
                src: None,
                proof: None,
                proofStatus: None,
                unifErr: None,
                syntaxErr: None,
            }
        })
    }
}

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
    ~idxSwap:option<int>=?,
    ~idxRemove:option<int>=?,
    ~idxAdd:option<int>=?,
    ()
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

let findIdxAdd = (a:array<stmtSnapshot>, b:array<stmtSnapshot>):option<int> => {
    let aLen = a->Js_array2.length
    let bLen = b->Js_array2.length
    if (aLen + 1 == bLen) {
        let idxAdd = ref(None)
        let match = ref(true)
        let i = ref(0)
        while (match.contents && i.contents < aLen) {
            if (a[i.contents].id != b[i.contents].id) {
                switch idxAdd.contents {
                    | None => {
                        if (a[i.contents].id == b[i.contents+1].id) {
                            idxAdd := Some(i.contents)
                        } else {
                            match := false
                        }
                    }
                    | Some(idxAdd) => match := a[i.contents].id == b[i.contents+1].id
                }
            }
            i := i.contents + 1
        }
        if (!match.contents) {
            None
        } else {
            idxAdd.contents->Belt_Option.orElse(Some(aLen))
        }
    } else {
        None
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
                let diffs = findSmallDiffStmt( a, b, ~idxSwap=?(idxSwap.contents), ())
                if (diffs->Js.Array2.length == 0) {
                    Ok(None)
                } else {
                    Ok(Some(diffs))
                }
            }
        } else if (aLen + 1 == bLen) {
            switch findIdxAdd(a, b) {
                | None => Error(())
                | Some(idxAdd) => Ok(Some(findSmallDiffStmt( a, b, ~idxAdd, ())))
            }
        } else if (aLen - 1 == bLen) {
            switch findIdxAdd(b, a) {
                | None => Error(())
                | Some(idxRemove) => Ok(Some(findSmallDiffStmt( a, b, ~idxRemove, ())))
            }
        } else {
            Error(())
        }
    }
}

let findDiff = (a:editorSnapshot, b:editorSnapshot):array<editorDiff> => {
    switch findDiffStmt(a.stmts, b.stmts) {
        | Error(_) => [Snapshot(b)]
        | Ok(diffsOpt) => {
            let diffs = switch diffsOpt {
                | None => []
                | Some(diffs) => diffs
            }
            if (a.descr != b.descr) {
                diffs->Js.Array2.push(Descr(b.descr))->ignore
            }
            if (a.varsText != b.varsText) {
                diffs->Js.Array2.push(Vars(b.varsText))->ignore
            }
            if (a.disjText != b.disjText) {
                diffs->Js.Array2.push(Disj(b.disjText))->ignore
            }
            diffs
        }
    }
}

let isStmtMove = (diff:editorDiff):bool => {
    switch diff {
        | StmtMove(_) => true
        | _ => false
    }
}

let mergeDiff = (a:array<editorDiff>, b:array<editorDiff>):option<array<editorDiff>> => {
    let aLen = a->Js_array2.length
    let bLen = b->Js_array2.length
    if (aLen == bLen) {
        if (aLen == 1 && isStmtMove(a[0])) {
            switch a[0] {
                | StmtMove({stmtId: stmtIdA}) => {
                    switch b[0] {
                        | StmtMove({stmtId: stmtIdB}) => {
                            if (stmtIdA == stmtIdB) {
                                Some(b)
                            } else {
                                None
                            }
                        }
                        | _ => None
                    }
                }
                | _ => None
            }
        } else {
            None
        }
    } else {
        None
    }
}

let updateStmt = (sn:editorSnapshot, stmtId:stmtId, update:stmtSnapshot=>stmtSnapshot):editorSnapshot => {
    {
        ...sn,
        stmts:sn.stmts->Js_array2.map(stmt => if (stmt.id == stmtId) {stmt->update} else {stmt})
    }
}

let addStmt = (sn:editorSnapshot, idx:int, stmt:stmtSnapshot):editorSnapshot => {
    let newStmts = sn.stmts->Js_array2.copy
    newStmts->Js.Array2.spliceInPlace(~pos=idx, ~remove=0, ~add=[stmt])->ignore
    {
        ...sn,
        stmts:newStmts
    }
}

let removeStmt = (sn:editorSnapshot, stmtId:stmtId):editorSnapshot => {
    {
        ...sn,
        stmts:sn.stmts->Js.Array2.filter(stmt => stmt.id != stmtId)
    }
}

let moveStmt = (sn:editorSnapshot, stmtId:stmtId, idx:int):editorSnapshot => {
    switch sn.stmts->Js.Array2.find(stmt => stmt.id == stmtId) {
        | None => sn
        | Some(stmt) => {
            let sn = sn->removeStmt(stmtId)
            sn->addStmt(idx, stmt)
        }
    }
}

let applyDiffSingle = (sn:editorSnapshot, diff:editorDiff):editorSnapshot => {
    switch diff {
        | Descr(descr) => {...sn, descr}
        | Vars(varsText) => {...sn, varsText}
        | Disj(disjText) => {...sn, disjText}
        | StmtLabel({stmtId, label}) => sn->updateStmt(stmtId, stmt => {...stmt, label})
        | StmtTyp({stmtId, typ, isGoal}) => sn->updateStmt(stmtId, stmt => {...stmt, typ, isGoal})
        | StmtJstf({stmtId, jstfText}) => sn->updateStmt(stmtId, stmt => {...stmt, jstfText})
        | StmtCont({stmtId, cont}) => sn->updateStmt(stmtId, stmt => {...stmt, cont})
        | StmtStatus({stmtId, proofStatus}) => sn->updateStmt(stmtId, stmt => {...stmt, proofStatus})
        | StmtAdd({idx, stmt}) => sn->addStmt(idx, stmt)
        | StmtRemove({stmtId}) => sn->removeStmt(stmtId)
        | StmtMove({stmtId, idx}) => sn->moveStmt(stmtId, idx)
        | Snapshot(editorSnapshot) => editorSnapshot
    }
}

let applyDiff = (sn:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {
    diff->Js_array2.reduce( applyDiffSingle, sn )
}

let editorHistMake = (~initState:editorState, maxLength:int):editorHistory => {
    {
        maxLength: Js_math.max_int(1, Js_math.min_int(200, maxLength)),
        head: initState->editorSnapshotMake,
        prev: None
    }
}

let editorHistTruncate = (ht:editorHistory):editorHistory => {
    if (ht.maxLength <= 1) {
        switch ht.prev {
            | None => ht
            | Some(_) => {
                {
                    ...ht,
                    prev: None
                }
            }
        }
    } else {
        let nodeRef = ref(ht.prev)
        let len = ref(2)
        let modified = ref(false)
        while (nodeRef.contents->Belt_Option.isSome) {
            let node = nodeRef.contents->Belt_Option.getExn
            if (len.contents < ht.maxLength) {
                nodeRef := node.prev
                len := len.contents + 1
            } else {
                node.prev = None
                modified := true
                nodeRef := None
            }
        }
        if (modified.contents) {
            {
                ...ht,
                prev: ht.prev,
            }
        } else {
            ht
        }
    }
}

let editorHistAddSnapshot = (ht:editorHistory, st:editorState):editorHistory => {
    let newHead = editorSnapshotMake(st)
    let diff = newHead->findDiff(ht.head)
    if (diff->Js.Array2.length == 0) {
        ht
    } else {
        let ht = switch ht.prev {
            | None => {
                {
                    ...ht,
                    head: newHead,
                    prev: Some({ prev:None, diff, }),
                }
            }
            | Some(prev) => {
                switch diff->mergeDiff(prev.diff) {
                    | None => {
                        {
                            ...ht,
                            head: newHead,
                            prev: Some({ prev:ht.prev, diff, }),
                        }
                    }
                    | Some(mergedDiff) => {
                        {
                            ...ht,
                            head: newHead,
                            prev: Some({ prev:prev.prev, diff: mergedDiff, }),
                        }
                    }
                }
            }
        }
        ht->editorHistTruncate
    }
}

let editorHistSetMaxLength = (ht:editorHistory, maxLength:int):editorHistory => {
    if (maxLength == ht.maxLength) {
        ht
    } else {
        {
            ...ht,
            maxLength
        }->editorHistTruncate
    }
}

let editorHistIsEmpty = (ht:editorHistory):bool => {
    ht.prev->Belt_Option.isNone
}

let editorHistLength = (ht:editorHistory):int => {
    let len = ref(0)
    let node = ref(ht.prev)
    while (node.contents->Belt_Option.isSome) {
        len := len.contents + 1
        node := (node.contents->Belt_Option.getExn).prev
    }
    len.contents
}

let restoreEditorStateFromSnapshot = (st:editorState, ht:editorHistory, idx:int): option<editorState> => {
    if (ht->editorHistIsEmpty) {
        None
    } else {
        let curIdx = ref(0)
        let curNode = ref(ht.prev)
        let curSn = ref(curNode.contents->Belt_Option.map(curNode => ht.head->applyDiff(curNode.diff)))
        while (curSn.contents->Belt_Option.isSome && curIdx.contents < idx) {
            curIdx := curIdx.contents + 1
            curNode := curNode.contents->Belt_Option.flatMap(curNode => curNode.prev)
            curSn := curNode.contents->Belt_Option.flatMap(
                curNode => curSn.contents->Belt_Option.map(applyDiff(_, curNode.diff))
            )
        }
        curSn.contents->Belt_Option.map(st->updateEditorStateFromSnapshot)
    }
}