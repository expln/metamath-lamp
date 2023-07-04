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

type editorHistory = {
    head: editorSnapshot,
    prev: array<array<editorDiff>>,
    maxLength: int,
}

let editorSnapshotMake = (st:editorState):editorSnapshot => {
    {
        descr: st.descr,
        varsText: st.varsText,
        disjText: st.disjText,
        stmts: st.stmts->Js.Array2.map(stmt => {
            let res:stmtSnapshot = {
                id: stmt.id,
                label: stmt.label,
                typ: stmt.typ,
                isGoal: stmt.isGoal,
                jstfText: stmt.jstfText,
                cont: stmt.cont->contToStr,
                proofStatus: stmt.proofStatus,
            }
            res
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
                a->Js_array2.map(stmt => StmtRemove({stmtId:stmt.id}))
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

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff = (sn:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {
    diff->Js_array2.reduce( applyDiffSingle, sn )
}

let editorHistMake = (~initState:editorState, ~maxLength:int):editorHistory => {
    {
        maxLength: Js_math.max_int(0, Js_math.min_int(200, maxLength)),
        head: initState->editorSnapshotMake,
        prev: []
    }
}

let editorHistAddSnapshot = (ht:editorHistory, st:editorState):editorHistory => {
    let newHead = editorSnapshotMake(st)
    let diff = newHead->findDiff(ht.head)
    if (diff->Js.Array2.length == 0) {
        ht
    } else {
        {
            ...ht,
            head: newHead,
            prev: 
                if (ht.prev->Js_array2.length == 0) {
                    [diff]
                } else {
                    switch diff->mergeDiff(ht.prev[0]) {
                        | None => {
                            [diff]->Js.Array2.concat(ht.prev->Js_array2.slice(~start=0, ~end_=ht.maxLength-1))
                        }
                        | Some(mergedDiff) => {
                            [mergedDiff]->Js.Array2.concat(ht.prev->Js_array2.slice(~start=1, ~end_=ht.maxLength))
                        }
                    }
                }
        }
    }
}

let editorHistSetMaxLength = (ht:editorHistory, maxLength:int):editorHistory => {
    if (maxLength == ht.maxLength) {
        ht
    } else {
        {
            ...ht,
            maxLength,
            prev:ht.prev->Js_array2.slice(~start=0,~end_=maxLength)
        }
    }
}

let editorHistIsEmpty = (ht:editorHistory):bool => {
    ht.prev->Js_array2.length == 0
}

let editorHistLength = (ht:editorHistory):int => {
    ht.prev->Js_array2.length
}

let restoreEditorStateFromSnapshot = (st:editorState, ht:editorHistory, idx:int): result<(editorState,editorHistory),string> => {
    let histLen = ht->editorHistLength
    if (histLen == 0 || histLen <= idx) {
        Error(`histLen == 0 || histLen <= idx`)
    } else {
        let curSn = ref(ht.head)
        for i in 0 to idx {
            curSn := curSn.contents->applyDiff(ht.prev[i])
        }
        Ok((
            st->updateEditorStateFromSnapshot(curSn.contents),
            {
                ...ht,
                prev: ht.prev->Js.Array2.sliceFrom(idx+1)
            }
        ))
    }
}

type stmtSnapshotLocStor = {
    id: string,
    label: string,
    typ: string,
    isGoal: bool,
    jstfText: string,
    cont: string,
    proofStatus: option<string>,
}

type editorSnapshotLocStor = {
    descr: string,
    varsText: string,
    disjText: string,
    stmts: array<stmtSnapshotLocStor>,
}

type editorDiffLocStor = {
    typ: string,
    id?:string,
    bool?:bool,
    int?:int,
    str?:string,
    stmt?:stmtSnapshotLocStor,
    sn?:editorSnapshotLocStor,
}

type editorHistoryLocStor = {
    head: editorSnapshotLocStor,
    prev: array<array<editorDiffLocStor>>,
    maxLength: int,
}

let proofStatusToStr = (status:proofStatus):string => {
    switch status {
        | Ready => "r"
        | Waiting => "w"
        | NoJstf => "n"
        | JstfIsIncorrect => "i"
    }
}

let proofStatusFromStr = (str:string):proofStatus => {
    switch str {
        | "r"  => Ready
        | "w"  => Waiting
        | "n"  => NoJstf
        | "i"  => JstfIsIncorrect
        | _ => raise(MmException({msg:`Cannot convert '${str}' to proofStatus.`}))
    }
}

let stmtSnapshotToLocStor = (stmt:stmtSnapshot):stmtSnapshotLocStor => {
    {
        id: stmt.id,
        label: stmt.label,
        typ: stmt.typ->userStmtTypeToStr,
        isGoal: stmt.isGoal,
        jstfText: stmt.jstfText,
        cont: stmt.cont,
        proofStatus: stmt.proofStatus->Belt_Option.map(proofStatusToStr)
    }
}

let stmtSnapshotFromLocStor = (stmt:stmtSnapshotLocStor):stmtSnapshot => {
    {
        id: stmt.id,
        label: stmt.label,
        typ: stmt.typ->userStmtTypeFromStr,
        isGoal: stmt.isGoal,
        jstfText: stmt.jstfText,
        cont: stmt.cont,
        proofStatus: stmt.proofStatus->Belt_Option.map(proofStatusFromStr)
    }
}

let editorSnapshotToLocStor = (sn:editorSnapshot):editorSnapshotLocStor => {
    {
        descr: sn.descr,
        varsText: sn.varsText,
        disjText: sn.disjText,
        stmts: sn.stmts->Js.Array2.map(stmtSnapshotToLocStor)
    }
}

let editorSnapshotFromLocStor = (sn:editorSnapshotLocStor):editorSnapshot => {
    {
        descr: sn.descr,
        varsText: sn.varsText,
        disjText: sn.disjText,
        stmts: sn.stmts->Js.Array2.map(stmtSnapshotFromLocStor)
    }
}

let editorDiffToLocStor = (diff:editorDiff):editorDiffLocStor => {
    switch diff {
        | Descr(descr) => 
            { typ:"D", str:descr }
        | Vars(varsText) => 
            { typ:"V", str:varsText }
        | Disj(disjText) => 
            { typ:"J", str:disjText }
        | StmtLabel({stmtId, label}) => 
            { typ:"SL", id:stmtId, str:label }
        | StmtTyp({stmtId, typ, isGoal}) => 
            { typ:"ST", id:stmtId, str:typ->userStmtTypeToStr, bool:isGoal }
        | StmtJstf({stmtId, jstfText}) => 
            { typ:"SJ", id:stmtId, str:jstfText }
        | StmtCont({stmtId, cont}) => 
            { typ:"SC", id:stmtId, str:cont }
        | StmtStatus({stmtId, proofStatus}) => 
            { typ:"SS", id:stmtId, str:?(proofStatus->Belt_Option.map(proofStatusToStr)) }
        | StmtAdd({idx, stmt}) => 
            { typ:"SA", int:idx, stmt:stmt->stmtSnapshotToLocStor }
        | StmtRemove({stmtId}) => 
            { typ:"SR", id:stmtId }
        | StmtMove({stmtId, idx}) => 
            { typ:"SM", id:stmtId, int:idx }
        | Snapshot(editorSnapshot) => 
            { typ:"E", sn:editorSnapshot->editorSnapshotToLocStor }
    }
}

let optGetEdls = (opt:option<'a>, typ:string, attrName:string):'a => {
    switch opt {
        | None => raise(MmException({msg:`'${attrName}' is not set in an editorDiffLocStor.`}))
        | Some(str) => str
    }
}

let edlsGetId = (d:editorDiffLocStor):string => optGetEdls(d.id, d.typ, "id")
let edlsGetBool = (d:editorDiffLocStor):bool => optGetEdls(d.bool, d.typ, "bool")
let edlsGetInt = (d:editorDiffLocStor):int => optGetEdls(d.int, d.typ, "int")
let edlsGetStr = (d:editorDiffLocStor):string => optGetEdls(d.str, d.typ, "str")
let edlsGetStmt = (d:editorDiffLocStor):stmtSnapshotLocStor => optGetEdls(d.stmt, d.typ, "stmt")
let edlsGetSn = (d:editorDiffLocStor):editorSnapshotLocStor => optGetEdls(d.sn, d.typ, "sn")

let editorDiffFromLocStor = (diff:editorDiffLocStor):editorDiff => {
    switch diff.typ {
        | "D" => Descr(diff->edlsGetStr)
        | "V" => Vars(diff->edlsGetStr)
        | "J" => Disj(diff->edlsGetStr)
        | "SL" => StmtLabel({stmtId:diff->edlsGetId, label:diff->edlsGetStr})
        | "ST" => StmtTyp({stmtId:diff->edlsGetId, typ:diff->edlsGetStr->userStmtTypeFromStr, isGoal:diff->edlsGetBool})
        | "SJ" => StmtJstf({stmtId:diff->edlsGetId, jstfText:diff->edlsGetStr})
        | "SC" => StmtCont({stmtId:diff->edlsGetId, cont:diff->edlsGetStr})
        | "SS" => StmtStatus({stmtId:diff->edlsGetId, proofStatus:diff.str->Belt.Option.map(proofStatusFromStr)})
        | "SA" => StmtAdd({idx:diff->edlsGetInt, stmt:diff->edlsGetStmt->stmtSnapshotFromLocStor})
        | "SR" => StmtRemove({stmtId:diff->edlsGetId})
        | "SM" => StmtMove({stmtId:diff->edlsGetId, idx:diff->edlsGetInt})
        | "E" => Snapshot(diff->edlsGetSn->editorSnapshotFromLocStor)
        | _ => raise(MmException({msg:`Cannot convert editorDiffLocStor to editorDiff for diff.typ='${diff.typ}'.`}))
    }
}

let editorHistoryToLocStor = (ht:editorHistory):editorHistoryLocStor => {
    {
        head: ht.head->editorSnapshotToLocStor,
        prev: ht.prev->Js_array2.map(diff => diff->Js_array2.map(editorDiffToLocStor)),
        maxLength: ht.maxLength,
    }
}

let editorHistoryFromLocStor = (ht:editorHistoryLocStor):editorHistory => {
    {
        head: ht.head->editorSnapshotFromLocStor,
        prev: ht.prev->Js_array2.map(diff => diff->Js_array2.map(editorDiffFromLocStor)),
        maxLength: ht.maxLength,
    }
}

// --------- Tests for private types and functions ---------------------------------------

open Expln_test

let a:editorSnapshot = {
    {
        descr: "descr",
        varsText: "varsText",
        disjText: "disjText",
        stmts: [
            { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
            { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
        ],
    }
}

let moveStmtTest = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => {
    let newStmts = a.stmts->Js_array2.copy
    let idx = newStmts->Js.Array2.findIndex(stmt => stmt.id == stmtId)
    let tmp = newStmts[idx]
    newStmts[idx] = newStmts[idx+1]
    newStmts[idx+1] = tmp
    {
        ...a,
        stmts:newStmts
    }
}

let testApplyDiff = (
    ~initState:editorSnapshot,
    ~changes:editorSnapshot=>editorSnapshot,
    ~expectedEndState:editorSnapshot
):unit => {
    //given
    let diff = initState->findDiff(initState->changes)
    assertEq(diff->Js.Array2.length > 0, true)

    //when
    let actualEndState = initState->applyDiff(diff)

    //then
    assertEq(actualEndState, expectedEndState)
}

let mm_editor_snapshot__test_findDiff = ():unit => {
    it("finds diffs", _ => {
        assertEq( findDiff(a, {...a, descr: a.descr}), [] )

        assertEq( findDiff(a, {...a, descr: "descr-new"}), [Descr("descr-new")] )
        assertEq( findDiff(a, {...a, varsText: "varsText-new"}), [Vars("varsText-new")] )
        assertEq( findDiff(a, {...a, disjText: "disjText-new"}), [Disj("disjText-new")] )

        assertEq( 
            findDiff(a, a->updateStmt("1", stmt => {...stmt, label:"label-new"})), 
            [StmtLabel({stmtId: "1", label: "label-new"})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, typ:E})), 
            [StmtTyp({stmtId: "2", typ: E, isGoal: false})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, isGoal:true})), 
            [StmtTyp({stmtId: "2", typ: P, isGoal: true})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})), 
            [StmtTyp({stmtId: "2", typ: E, isGoal: true})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("3", stmt => {...stmt, jstfText: "jstfText-new"})), 
            [StmtJstf({stmtId: "3", jstfText: "jstfText-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("1", stmt => {...stmt, cont: "cont-new"})), 
            [StmtCont({stmtId: "1", cont: "cont-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, proofStatus: None})), 
            [StmtStatus({stmtId: "2", proofStatus: None})]
        )

        assertEq( 
            findDiff(
                {...a, stmts:[]}, 
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
            ), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 1, stmt: { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 2, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 3, stmt: { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )

        assertEq( findDiff(a, a->removeStmt("1")), [StmtRemove({stmtId: "1"})] )
        assertEq( findDiff(a, a->removeStmt("2")), [StmtRemove({stmtId: "2"})] )
        assertEq( findDiff(a, a->removeStmt("3")), [StmtRemove({stmtId: "3"})] )
        assertEq( 
            findDiff(
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
                {...a, stmts:[]},
            ), 
            [StmtRemove({stmtId: "4"})]
        )

        assertEq( findDiff(a, a->moveStmtTest("1")), [StmtMove({stmtId: "1", idx: 1})] )
        assertEq( findDiff(a, a->moveStmtTest("2")), [StmtMove({stmtId: "2", idx: 2})] )

        assertEq( 
            findDiff(a, a->removeStmt("1")->removeStmt("2")), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                        ],
                    }
                )
            ] 
        )

        assertEq( 
            findDiff(
                a, 
                a
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            ), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) },
                            { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
                            { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) },
                            { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                        ],
                    }
                )
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtTest("1")->moveStmtTest("1") ), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                            { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
                        ],
                    }
                )
            ] 
        )

        assertEq( 
            findDiff(
                a
                    ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:None}),
                a
                    ->updateStmt("1", stmt => {...stmt, proofStatus:Some(Ready)})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:Some(Waiting)})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:Some(JstfIsIncorrect)}),
            ), 
            [
                StmtStatus({stmtId: "1", proofStatus: Some(Ready)}),
                StmtStatus({stmtId: "2", proofStatus: Some(Waiting)}),
                StmtStatus({stmtId: "3", proofStatus: Some(JstfIsIncorrect)}),
            ] 
        )

        assertEq( 
            findDiff(
                a,
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"}),
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtLabel({stmtId: "1", label: "ABC"}),
                StmtStatus({stmtId: "1", proofStatus: Some(NoJstf)}),
                StmtTyp({stmtId: "2", typ:E, isGoal:true}),
                StmtJstf({stmtId: "3", jstfText:"BBB"}),
                StmtCont({stmtId: "3", cont:"TTTTT"}),
                Descr("descr-new"),
                Vars("varsText-new"),
                Disj("disjText-new"),
            ] 
        )

        assertEq( 
            findDiff(
                a,
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) }),
            ), 
            [
                Snapshot(
                    {
                        descr: "descr-new",
                        varsText: "varsText-new",
                        disjText: "disjText-new",
                        stmts: [
                            { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) },
                            { id: "1", label: "ABC", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: Some(NoJstf) },
                            { id: "2", label: "label2", typ:E, isGoal:true, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText:"BBB", cont:"TTTTT", proofStatus: Some(Waiting) },
                            { id: "5", label: "label5", typ: E, isGoal: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) },
                        ],
                    }
                )
            ] 
        )
    })
}

let mm_editor_snapshot__test_applyDiff = ():unit => {
    it("applies diffs", _ => {
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, descr: "descr-new"},
            ~expectedEndState = {...a, descr: "descr-new"}
        )
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, varsText: "varsText-new"},
            ~expectedEndState = {...a, varsText: "varsText-new"}
        )
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, disjText: "disjText-new"},
            ~expectedEndState = {...a, disjText: "disjText-new"}
        )

        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "1", stmt => {...stmt, label:"label-new"}),
            ~expectedEndState = a->updateStmt("1", stmt => {...stmt, label:"label-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, typ:E}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, typ:E})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, isGoal:true}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, isGoal:true})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, typ:E, isGoal:true}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "3", stmt => {...stmt, jstfText: "jstfText-new"}),
            ~expectedEndState = a->updateStmt("3", stmt => {...stmt, jstfText: "jstfText-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "1", stmt => {...stmt, cont: "cont-new"}),
            ~expectedEndState = a->updateStmt("1", stmt => {...stmt, cont: "cont-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, proofStatus: None}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, proofStatus: None})
        )

        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 1, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 2, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 3, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
        )

        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "1"),
            ~expectedEndState = a->removeStmt("1")
        )
        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "2"),
            ~expectedEndState = a->removeStmt("2")
        )
        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "3"),
            ~expectedEndState = a->removeStmt("3")
        )

        testApplyDiff( ~initState=a,
            ~changes = moveStmtTest(_, "1"),
            ~expectedEndState = a->moveStmtTest("1")
        )
        testApplyDiff( ~initState=a,
            ~changes = moveStmtTest(_, "2"),
            ~expectedEndState = a->moveStmtTest("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->removeStmt("1")->removeStmt("2"),
            ~expectedEndState = a->removeStmt("1")->removeStmt("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                sn
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            },
            ~expectedEndState = a
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->moveStmtTest("1")->moveStmtTest("1"),
            ~expectedEndState = a->moveStmtTest("1")->moveStmtTest("1")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                sn
                    ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:None})
            },
            ~expectedEndState = a
                ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                ->updateStmt("3", stmt => {...stmt, proofStatus:None})
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                {...sn, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
        )
    })
}

let mm_editor_snapshot__test_mergeDiff = ():unit => {
    it("merges consecutive moves of same step", _ => {
        //given
        let b = a->moveStmtTest("1")
        let diff1 = findDiff(a,b)
        let c = b->moveStmtTest("1")
        let diff2 = findDiff(b,c)

        //when
        let mergeResult = mergeDiff(diff1,diff2)

        //then
        assertEq(mergeResult, Some([StmtMove({stmtId: "1", idx: 2})]))
    })
    
    it("does not merge consecutive moves of different steps", _ => {
        //given
        let b = a->moveStmtTest("1")
        let diff1 = findDiff(a,b)
        let c = b->moveStmtTest("2")
        let diff2 = findDiff(b,c)

        //when
        let mergeResult = mergeDiff(diff1,diff2)

        //then
        assertEq(mergeResult, None)
    })
}