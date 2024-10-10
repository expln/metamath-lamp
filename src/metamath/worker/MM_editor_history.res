open MM_wrk_editor
open MM_parser

type stmtSnapshot = {
    id: stmtId,
    label: string,
    typ: userStmtType,
    isGoal: bool,
    isBkm: bool,
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
    | StmtBkm({stmtId: stmtId, isBkm: bool})
    | StmtJstf({stmtId: stmtId, jstfText: string})
    | StmtCont({stmtId: stmtId, cont: string})
    | StmtStatus({stmtId: stmtId, proofStatus: option<proofStatus>})
    | StmtStatusUnset({stmtIds: array<stmtId>})
    | StmtAdd({idx: int, stmt: stmtSnapshot})
    | StmtRemove({stmtId: stmtId})
    | StmtMove({stmtId: stmtId, idx: int})

type editorHistory = {
    head: editorSnapshot,
    prev: array<array<editorDiff>>,
    maxLength: int,
}

let isStmtStatusRemove = (diff:editorDiff):bool => {
    switch diff {
        | StmtStatus({proofStatus:None}) | StmtStatusUnset(_) => true
        | _ => false
    }
}

let isStmtStatusSet = (diff:editorDiff):bool => {
    switch diff {
        | StmtStatus({proofStatus:Some(_)}) => true
        | _ => false
    }
}

let isStmtMove = (diff:editorDiff):bool => {
    switch diff {
        | StmtMove(_) => true
        | _ => false
    }
}

let allStatusUnset = (diff:array<editorDiff>):bool => {
    diff->Js_array2.every(isStmtStatusRemove)
}

let allStatusSet = (diff:array<editorDiff>):bool => {
    diff->Js_array2.every(isStmtStatusSet)
}

let allMoveAndStatusSet = (diff:array<editorDiff>):bool => {
    diff->Js_array2.every(d => isStmtMove(d) || isStmtStatusSet(d))
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
                isBkm: stmt.isBkm,
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
                isBkm: stmt.isBkm,
                cont: stmt.cont->strToCont(
                    ~preCtxColors=st.preCtxColors,
                    ~wrkCtxColors=st.wrkCtxColors,
                    ()
                ),
                contEditMode: false,
                isDuplicated: false,
                
                jstfText: stmt.jstfText,
                jstfEditMode: false,

                stmtErr: None,

                expr: None,
                jstf: None,
                proofTreeDto: None,
                src: None,
                proof: None,
                proofStatus: stmt.proofStatus,
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

let updateStmt = (sn:editorSnapshot, stmtId:stmtId, update:stmtSnapshot=>stmtSnapshot):editorSnapshot => {
    {
        ...sn,
        stmts:sn.stmts->Js_array2.map(stmt => if (stmt.id == stmtId) {stmt->update} else {stmt})
    }
}

let addStmt = (sn:editorSnapshot, idx:int, stmt:stmtSnapshot):editorSnapshot => {
    let newStmts = sn.stmts->Array.copy
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
        | StmtBkm({stmtId, isBkm}) => sn->updateStmt(stmtId, stmt => {...stmt, isBkm})
        | StmtJstf({stmtId, jstfText}) => sn->updateStmt(stmtId, stmt => {...stmt, jstfText})
        | StmtCont({stmtId, cont}) => sn->updateStmt(stmtId, stmt => {...stmt, cont})
        | StmtStatus({stmtId, proofStatus}) => sn->updateStmt(stmtId, stmt => {...stmt, proofStatus})
        | StmtStatusUnset({stmtIds}) => {
            {
                ...sn,
                stmts: sn.stmts->Js.Array2.map(stmt => {
                    if (stmtIds->Js.Array2.includes(stmt.id)) {
                        {...stmt, proofStatus:None}
                    } else {
                        stmt
                    }
                })
            }
        }
        | StmtAdd({idx, stmt}) => sn->addStmt(idx, stmt)
        | StmtRemove({stmtId}) => sn->removeStmt(stmtId)
        | StmtMove({stmtId, idx}) => sn->moveStmt(stmtId, idx)
    }
}

let getStmtIdsFromStatusUnset = (diffs:array<editorDiff>):array<stmtId> => {
    diffs->Js_array2.map(diff => {
        switch diff {
            | StmtStatus({stmtId, proofStatus:None}) => stmtId
            | _ => raise(MmException({msg:`getStmtIdsFromStatusUnset: unexpected type of diff.`}))
        }
    })
}

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff = (sn:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {
    diff->Js_array2.reduce( applyDiffSingle, sn )
}

let findDiff = (a:editorSnapshot, b:editorSnapshot):array<editorDiff> => {
    let diffs = []

    let aIds = a.stmts->Js_array2.map(stmt => stmt.id)->Belt_HashSetString.fromArray
    let bIds = b.stmts->Js_array2.map(stmt => stmt.id)->Belt_HashSetString.fromArray
    a.stmts->Js_array2.forEach(stmtA => {
        if (!(bIds->Belt_HashSetString.has(stmtA.id))) {
            diffs->Array.push(StmtRemove({stmtId:stmtA.id}))
        }
    })
    b.stmts->Js_array2.forEachi((stmtB,i) => {
        if (!(aIds->Belt_HashSetString.has(stmtB.id))) {
            diffs->Array.push(StmtAdd({idx:i, stmt:stmtB}))
        }
    })

    let aMod = ref(a->applyDiff(diffs))
    let aModLen = aMod.contents.stmts->Js_array2.length
    let bLen = b.stmts->Js_array2.length
    if (aModLen != bLen) {
        raise(MmException({msg:`aModLen != bLen`}))
    }
    for i in 0 to bLen-1 {
        let stmtA = aMod.contents.stmts->Array.getUnsafe(i)
        let stmtB = b.stmts->Array.getUnsafe(i)
        if (stmtA.id != stmtB.id) {
            let move = StmtMove({stmtId:stmtB.id, idx:i})
            diffs->Array.push(move)
            aMod := aMod.contents->applyDiffSingle(move)
        }
    }

    b.stmts->Js_array2.forEachi((stmtB,i) => {
        let stmtA = aMod.contents.stmts->Array.getUnsafe(i)
        if (stmtA.id != stmtB.id) {
            raise(MmException({msg:`stmtA.id != stmtB.id`}))
        }
        if (stmtA.label != stmtB.label) {
            diffs->Array.push(StmtLabel({stmtId:stmtA.id, label:stmtB.label}))
        }
        if (stmtA.typ != stmtB.typ || stmtA.isGoal != stmtB.isGoal) {
            diffs->Array.push(StmtTyp({stmtId:stmtA.id, typ:stmtB.typ, isGoal:stmtB.isGoal}))
        }
        if (stmtA.isBkm != stmtB.isBkm) {
            diffs->Array.push(StmtBkm({stmtId:stmtA.id, isBkm:stmtB.isBkm}))
        }
        if (stmtA.jstfText != stmtB.jstfText) {
            diffs->Array.push(StmtJstf({stmtId:stmtA.id, jstfText:stmtB.jstfText}))
        }
        if (stmtA.cont != stmtB.cont) {
            diffs->Array.push(StmtCont({stmtId:stmtA.id, cont:stmtB.cont}))
        }
        if (!(stmtA.proofStatus->proofStatusEq(stmtB.proofStatus))) {
            diffs->Array.push(StmtStatus({stmtId:stmtA.id, proofStatus:stmtB.proofStatus}))
        }
    })

    if (a.descr != b.descr) {
        diffs->Array.push(Descr(b.descr))
    }
    if (a.varsText != b.varsText) {
        diffs->Array.push(Vars(b.varsText))
    }
    if (a.disjText != b.disjText) {
        diffs->Array.push(Disj(b.disjText))
    }

    if (diffs->Js.Array2.length > 1 && diffs->allStatusUnset) {
        [StmtStatusUnset({ stmtIds:diffs->getStmtIdsFromStatusUnset})]
    } else {
        diffs
    }

}

let editorHistMake = (~initState:editorState, ~maxLength:int):editorHistory => {
    {
        maxLength: Js_math.max_int(0, Js_math.min_int(1000, maxLength)),
        head: initState->editorSnapshotMake,
        prev: []
    }
}

let prependDiffToFirstElem = (diff:array<editorDiff>, prev:array<array<editorDiff>>, maxLength:int) => {
    if (diff->Js_array2.length == 0) {
        raise(MmException({msg:`diff->Js_array2.length == 0`}))
    }
    if (prev->Js_array2.length == 0) {
        raise(MmException({msg:`prev->Js_array2.length == 0`}))
    }
    [ diff->Array.concat(prev->Array.getUnsafe(0)) ]->Array.concat(prev->Js_array2.slice(~start=1, ~end_=maxLength))
}

let editorHistAddSnapshot = (ht:editorHistory, st:editorState):editorHistory => {
    if (st->isEditMode) {
        ht
    } else {
        let newHead = editorSnapshotMake(st)
        if (ht.maxLength == 0) {
            {
                ...ht,
                head: newHead,
            }
        } else {
            let diff = newHead->findDiff(ht.head)
            if (diff->Js.Array2.length == 0 || diff->allStatusSet) {
                ht
            } else if (diff->allStatusUnset) {
                if (ht.prev->Js_array2.length == 0) {
                    {
                        ...ht,
                        head: newHead,
                    }
                } else {
                    {
                        ...ht,
                        head: newHead,
                        prev: prependDiffToFirstElem(diff, ht.prev, ht.maxLength),
                    }
                }
            } else if (diff->allMoveAndStatusSet && ht.prev->Js_array2.length > 0 && ht.prev->Array.getUnsafe(0)->allMoveAndStatusSet) {
                {
                    ...ht,
                    head: newHead,
                    prev: prependDiffToFirstElem(diff, ht.prev, ht.maxLength),
                }
            } else {
                {
                    ...ht,
                    head: newHead,
                    prev: [diff]->Array.concat(ht.prev->Js_array2.slice(~start=0, ~end_=ht.maxLength-1))
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

let editorHistGetSnapshotPreview = (ht:editorHistory, idx:int, st:editorState): result<editorState,string> => {
    if (idx == -1) {
        Ok(st->updateEditorStateFromSnapshot(ht.head))
    } else {
        let histLen = ht->editorHistLength
        if (histLen == 0 || histLen <= idx) {
            Error(`histLen == 0 || histLen <= idx`)
        } else {
            let curSn = ref(ht.head)
            for i in 0 to idx {
                curSn := curSn.contents->applyDiff(ht.prev->Array.getUnsafe(i))
            }
            Ok(st->updateEditorStateFromSnapshot(curSn.contents)->recalcWrkColors)
        }
    }
}

let restoreEditorStateFromSnapshot = (st:editorState, ht:editorHistory, idx:int): result<editorState,string> => {
    editorHistGetSnapshotPreview(ht, idx, st)->Belt_Result.map(st => {
        {
            ...st,
            stmts: st.stmts->Js.Array2.map(stmt => {
                {
                    ...stmt,
                    proofStatus: None,
                }
            })
        }
    })
}

type stmtSnapshotLocStor = {
    d: string, /* id */
    l: string, /* label */
    t: string, /* typ */
    g: bool, /* isGoal */
    b: bool, /* isBkm */
    j: string, /* jstfText */
    c: string, /* cont */
    p: option<string>, /* proofStatus */
}

type editorSnapshotLocStor = {
    d: string, /* descr */
    v: string, /* varsText */
    j: string, /* disjText */
    s: array<stmtSnapshotLocStor>, /* stmts */
}

type editorDiffLocStor = {
    t: string, /* typ */
    d?:string, /* id */
    b?:bool, /* bool */
    i?:int, /* int */
    s?:string, /* str */
    m?:stmtSnapshotLocStor, /* stmt */
    a?:array<string>, /* ids */
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
        d: stmt.id,
        l: stmt.label,
        t: stmt.typ->userStmtTypeToStr,
        g: stmt.isGoal,
        b: stmt.isBkm,
        j: stmt.jstfText,
        c: stmt.cont,
        p: stmt.proofStatus->Belt_Option.map(proofStatusToStr)
    }
}

let stmtSnapshotFromLocStor = (stmt:stmtSnapshotLocStor):stmtSnapshot => {
    {
        id: stmt.d,
        label: stmt.l,
        typ: stmt.t->userStmtTypeFromStr,
        isGoal: stmt.g,
        isBkm: stmt.b,
        jstfText: stmt.j,
        cont: stmt.c,
        proofStatus: stmt.p->Belt_Option.map(proofStatusFromStr)
    }
}

let editorSnapshotToLocStor = (sn:editorSnapshot):editorSnapshotLocStor => {
    {
        d: sn.descr,
        v: sn.varsText,
        j: sn.disjText,
        s: sn.stmts->Js.Array2.map(stmtSnapshotToLocStor)
    }
}

let editorSnapshotFromLocStor = (sn:editorSnapshotLocStor):editorSnapshot => {
    {
        descr: sn.d,
        varsText: sn.v,
        disjText: sn.j,
        stmts: sn.s->Js.Array2.map(stmtSnapshotFromLocStor)
    }
}

let editorDiffToLocStor = (diff:editorDiff):editorDiffLocStor => {
    switch diff {
        | Descr(descr) => 
            { t:"D", s:descr }
        | Vars(varsText) => 
            { t:"V", s:varsText }
        | Disj(disjText) => 
            { t:"J", s:disjText }
        | StmtLabel({stmtId, label}) => 
            { t:"SL", d:stmtId, s:label }
        | StmtTyp({stmtId, typ, isGoal}) => 
            { t:"ST", d:stmtId, s:typ->userStmtTypeToStr, b:isGoal }
        | StmtBkm({stmtId, isBkm}) => 
            { t:"SB", d:stmtId, b:isBkm }
        | StmtJstf({stmtId, jstfText}) => 
            { t:"SJ", d:stmtId, s:jstfText }
        | StmtCont({stmtId, cont}) => 
            { t:"SC", d:stmtId, s:cont }
        | StmtStatus({stmtId, proofStatus}) => 
            { t:"SS", d:stmtId, s:?(proofStatus->Belt_Option.map(proofStatusToStr)) }
        | StmtStatusUnset({stmtIds}) => 
            { t:"SU", a:stmtIds }
        | StmtAdd({idx, stmt}) => 
            { t:"SA", i:idx, m:stmt->stmtSnapshotToLocStor }
        | StmtRemove({stmtId}) => 
            { t:"SR", d:stmtId }
        | StmtMove({stmtId, idx}) => 
            { t:"SM", d:stmtId, i:idx }
    }
}

let optGetEdls = (opt:option<'a>, typ:string, attrName:string):'a => {
    switch opt {
        | None => raise(MmException({msg:`'${attrName}' is not set in an editorDiffLocStor for typ = '${typ}'.`}))
        | Some(str) => str
    }
}

let edlsGetId = (d:editorDiffLocStor):string => optGetEdls(d.d, d.t, "d")
let edlsGetIds = (d:editorDiffLocStor):array<string> => optGetEdls(d.a, d.t, "a")
let edlsGetBool = (d:editorDiffLocStor):bool => optGetEdls(d.b, d.t, "b")
let edlsGetInt = (d:editorDiffLocStor):int => optGetEdls(d.i, d.t, "i")
let edlsGetStr = (d:editorDiffLocStor):string => optGetEdls(d.s, d.t, "s")
let edlsGetStmt = (d:editorDiffLocStor):stmtSnapshotLocStor => optGetEdls(d.m, d.t, "m")

let editorDiffFromLocStor = (diff:editorDiffLocStor):editorDiff => {
    switch diff.t {
        | "D" => Descr(diff->edlsGetStr)
        | "V" => Vars(diff->edlsGetStr)
        | "J" => Disj(diff->edlsGetStr)
        | "SL" => StmtLabel({stmtId:diff->edlsGetId, label:diff->edlsGetStr})
        | "ST" => StmtTyp({stmtId:diff->edlsGetId, typ:diff->edlsGetStr->userStmtTypeFromStr, isGoal:diff->edlsGetBool})
        | "SB" => StmtBkm({stmtId:diff->edlsGetId, isBkm:diff->edlsGetBool})
        | "SJ" => StmtJstf({stmtId:diff->edlsGetId, jstfText:diff->edlsGetStr})
        | "SC" => StmtCont({stmtId:diff->edlsGetId, cont:diff->edlsGetStr})
        | "SS" => StmtStatus({stmtId:diff->edlsGetId, proofStatus:diff.s->Belt.Option.map(proofStatusFromStr)})
        | "SU" => StmtStatusUnset({stmtIds:diff->edlsGetIds})
        | "SA" => StmtAdd({idx:diff->edlsGetInt, stmt:diff->edlsGetStmt->stmtSnapshotFromLocStor})
        | "SR" => StmtRemove({stmtId:diff->edlsGetId})
        | "SM" => StmtMove({stmtId:diff->edlsGetId, idx:diff->edlsGetInt})
        | _ => raise(MmException({msg:`Cannot convert editorDiffLocStor to editorDiff for diff.typ='${diff.t}'.`}))
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

let editorHistToString = (ht:editorHistory):string => {
    Expln_utils_common.stringify(ht->editorHistoryToLocStor)
}

let parseStmtSnapshotLocStor = (d:Expln_utils_jsonParse.jsonAny):stmtSnapshotLocStor => {
    open Expln_utils_jsonParse
    {
        d: d->str("d", ()),
        l: d->str("l", ()),
        t: d->str("t", ()),
        g: d->bool("g", ()),
        b: d->bool("b", ()),
        j: d->str("j", ()),
        c: d->str("c", ()),
        p: d->strOpt("p", ()),
    }
}

let editorHistFromString = (jsonStr:string):result<editorHistory,string> => {
    open Expln_utils_jsonParse
    parseJson(jsonStr, asObj(_, d=>{
        {
            head: d->obj("head", d=>{
                {
                    d: d->str("d", ()),
                    v: d->str("v", ()),
                    j: d->str("j", ()),
                    s: d->arr("s", parseStmtSnapshotLocStor, ())
                }
            }, ()),
            prev: d->arr("prev", d=>{
                d->asArr(d=>{
                    {
                        t: d->str("t", ()),
                        d: ?d->strOpt("d", ()),
                        b: ?d->boolOpt("b", ()),
                        i: ?d->intOpt("i", ()),
                        s: ?d->strOpt("s", ()),
                        m: ?d->objOpt("m", parseStmtSnapshotLocStor, ()),
                        a: ?d->arrOpt("a", asStr(_, ()), ()),
                    }
                }, ())
            }, ()),
            maxLength: d->int("maxLength", ())
        }
    }, ()), ())->Belt.Result.map(editorHistoryFromLocStor)
}

let stmtSnapshotToStringExtended = (stmt:stmtSnapshot):string => {
    let typStr = switch stmt.typ {
        | E => "H"
        | P => if (stmt.isGoal) {"G"} else {"P"}
    }
    let bkmStr = if (stmt.isBkm) {
        "BKM "
    } else {
        ""
    }
    let statusStr = switch stmt.proofStatus {
        | None => ""
        | Some(Ready) => "\u2713"
        | Some(Waiting) => "\u223F"
        | Some(NoJstf) => "?"
        | Some(JstfIsIncorrect) => "\u2717"
    }
    `${stmt.id}: ${statusStr} ${bkmStr}${stmt.label} ${typStr} [${stmt.jstfText}] ${stmt.cont}`
}

let editorSnapshotToStringExtended = (sn:editorSnapshot):string => {
    let res = []

    res->Array.push("Description")
    res->Array.push(sn.descr)
    if (sn.descr->Js.String2.length > 0) {
        res->Array.push("")
    }

    res->Array.push("Variables")
    res->Array.push(sn.varsText)
    if (sn.varsText->Js.String2.length > 0) {
        res->Array.push("")
    }

    res->Array.push("Disjoints")
    res->Array.push(sn.disjText)
    if (sn.disjText->Js.String2.length > 0) {
        res->Array.push("")
    }

    sn.stmts->Js.Array2.forEach(stmt => {
        res->Array.push(stmt->stmtSnapshotToStringExtended)
    })
    res->Js.Array2.joinWith("\n")
}

let diffToStringExtendedSingle = (diff:editorDiff):string => {
    switch diff {
        | Descr(descr) => `Descr(${descr})`
        | Vars(varsText) => `Vars(${varsText->Js.String2.replaceByRe(%re("/[\n\r]+/g"), " ; ")})`
        | Disj(disjText) => `Disj(${disjText->Js.String2.replaceByRe(%re("/[\n\r]+/g"), " ; ")})`
        | StmtLabel({stmtId, label}) => `StmtLabel({stmtId=${stmtId}, label=${label}})`
        | StmtTyp({stmtId, typ, isGoal}) => 
            `StmtTyp({stmtId=${stmtId}, typ=${typ->userStmtTypeToStr}, isGoal=${isGoal->Expln_utils_common.stringify}})`
        | StmtBkm({stmtId, isBkm}) => 
            `StmtBkm({stmtId=${stmtId}, isBkm=${isBkm->Expln_utils_common.stringify}})`
        | StmtJstf({stmtId, jstfText}) => `StmtJstf({stmtId=${stmtId}, jstfText=${jstfText}})`
        | StmtCont({stmtId, cont}) => `StmtCont({stmtId=${stmtId}, cont=${cont}})`
        | StmtStatus({stmtId, proofStatus}) => 
            `StmtStatus({stmtId=${stmtId}, proofStatus=${proofStatus->Belt_Option.map(proofStatusToStr)->Belt_Option.getWithDefault("None")}})`
        | StmtStatusUnset({stmtIds}) => 
            `StmtStatusUnset({stmtIds=[${stmtIds->Js.Array2.joinWith(", ")}]})`
        | StmtAdd({idx, stmt}) => `StmtAdd({idx=${idx->Belt.Int.toString}, stmtId=${stmt.id}})`
        | StmtRemove({stmtId}) => `StmtRemove({stmtId=${stmtId}})`
        | StmtMove({stmtId, idx}) => `StmtMove({stmtId=${stmtId}, idx=${idx->Belt.Int.toString}})`
    }
}

let diffToStringExtended = (diff:array<editorDiff>):string => {
    diff->Js_array2.map(diffToStringExtendedSingle)->Js.Array2.joinWith("\n")
}

let editorHistToStringExtended = (ht:editorHistory):string => {
    let res = []
    let delim1 = "------------------------------------------------------------------------------------"
    let delim2 = "===================================================================================="
    let curSn = ref(ht.head)
    res->Array.push(curSn.contents->editorSnapshotToStringExtended)
    res->Array.push(delim2)
    ht.prev->Js.Array2.forEach(diff => {
        res->Array.push(diff->diffToStringExtended)
        res->Array.push(delim1)
        curSn := curSn.contents->applyDiff(diff)
        res->Array.push(curSn.contents->editorSnapshotToStringExtended)
        res->Array.push(delim2)
    })
    res->Js.Array2.joinWith("\n")
}

// --------- Tests for private types and functions ---------------------------------------

open Expln_test

let a:editorSnapshot = {
    {
        descr: "descr",
        varsText: "varsText",
        disjText: "disjText",
        stmts: [
            { id: "1", label: "label1", typ: E, isGoal: false, isBkm: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
            { id: "2", label: "label2", typ: P, isGoal: false, isBkm: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
            { id: "3", label: "label3", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
        ],
    }
}

let moveStmtTest = (a:editorSnapshot, stmtId:stmtId, dIdx:int):editorSnapshot => {
    let newStmts = a.stmts->Array.copy
    let idx = newStmts->Js.Array2.findIndex(stmt => stmt.id == stmtId)
    let tmp = newStmts->Array.getUnsafe(idx)
    newStmts[idx] = newStmts->Array.getUnsafe(idx+dIdx)
    newStmts[idx+dIdx] = tmp
    {
        ...a,
        stmts:newStmts
    }
}

let moveStmtUp = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => moveStmtTest(a, stmtId, -1)
let moveStmtDown = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => moveStmtTest(a, stmtId, 1)

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

let mm_editor_history__test_findDiff = ():unit => {
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
            findDiff(a, a->updateStmt("2", stmt => {...stmt, isBkm:true})), 
            [StmtBkm({stmtId: "2", isBkm: true})]
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
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
            ), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 1, stmt: { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 2, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 3, stmt: { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )

        assertEq( findDiff(a, a->removeStmt("1")), [StmtRemove({stmtId: "1"})] )
        assertEq( findDiff(a, a->removeStmt("2")), [StmtRemove({stmtId: "2"})] )
        assertEq( findDiff(a, a->removeStmt("3")), [StmtRemove({stmtId: "3"})] )
        assertEq( 
            findDiff(
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
                {...a, stmts:[]},
            ), 
            [StmtRemove({stmtId: "4"})]
        )

        assertEq( findDiff(a, a->moveStmtDown("1")), [StmtMove({stmtId: "2", idx: 0})] )
        assertEq( findDiff(a, a->moveStmtDown("2")), [StmtMove({stmtId: "3", idx: 1})] )

        assertEq( 
            findDiff(a, a->removeStmt("1")->removeStmt("2")), 
            [
                StmtRemove({stmtId: "1"}),
                StmtRemove({stmtId: "2"}),
            ] 
        )

        assertEq( 
            findDiff(
                a, 
                a
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtAdd({idx: 2, stmt: { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) }}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtDown("1") ), 
            [
                StmtMove({stmtId: "2", idx: 0}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtDown("1")->moveStmtDown("1") ), 
            [
                StmtMove({stmtId: "2", idx: 0}),
                StmtMove({stmtId: "3", idx: 1}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtUp("3") ), 
            [
                StmtMove({stmtId: "3", idx: 1}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtUp("3")->moveStmtUp("3") ), 
            [
                StmtMove({stmtId: "3", idx: 0}),
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
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT", isBkm:true}),
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtLabel({stmtId: "1", label: "ABC"}),
                StmtStatus({stmtId: "1", proofStatus: Some(NoJstf)}),
                StmtTyp({stmtId: "2", typ:E, isGoal:true}),
                StmtBkm({stmtId: "3", isBkm:true}),
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
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf), isBkm:true})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) }),
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtAdd({idx: 4, stmt: { id: "5", label: "label5", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) }}),
                StmtLabel({stmtId: "1", label: "ABC"}),
                StmtBkm({stmtId: "1", isBkm: true}),
                StmtStatus({stmtId: "1", proofStatus: Some(NoJstf)}),
                StmtTyp({stmtId: "2", typ:E, isGoal:true}),
                StmtJstf({stmtId: "3", jstfText:"BBB"}),
                StmtCont({stmtId: "3", cont:"TTTTT"}),
                Descr("descr-new"),
                Vars("varsText-new"),
                Disj("disjText-new"),
            ], 
        )
    })
}

let mm_editor_history__test_applyDiff = ():unit => {
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
            ~changes = updateStmt(_, "3", stmt => {...stmt, isBkm:true}),
            ~expectedEndState = a->updateStmt("3", stmt => {...stmt, isBkm:true})
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
            ~changes = addStmt(_, 0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
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
            ~changes = moveStmtDown(_, "1"),
            ~expectedEndState = a->moveStmtDown("1")
        )
        testApplyDiff( ~initState=a,
            ~changes = moveStmtDown(_, "2"),
            ~expectedEndState = a->moveStmtDown("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->removeStmt("1")->removeStmt("2"),
            ~expectedEndState = a->removeStmt("1")->removeStmt("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                sn
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            },
            ~expectedEndState = a
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->moveStmtDown("1")->moveStmtDown("1"),
            ~expectedEndState = a->moveStmtDown("1")->moveStmtDown("1")
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
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
        )

        testApplyDiff( ~initState=a,
            ~changes = _ => {
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
        )
    })
}