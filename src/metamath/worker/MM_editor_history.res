open MM_wrk_editor
open MM_editor_snapshot_diff
open Common

let version = 2

type editorHistory = {
    ver:int,
    head: editorSnapshot,
    prev: array<array<editorDiff>>,
    maxLength: int,
}

let editorSnapshotMake = (st:editorState):editorSnapshot => {
    {
        descr: st.descr,
        varsText: st.varsText,
        disjText: st.disjText,
        stmts: st.stmts->Array.map(stmt => {
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
        stmts: sn.stmts->Array.map(stmt => {
            {
                id: stmt.id,

                label: stmt.label,
                labelEditMode: false,
                typ: stmt.typ,
                typEditMode: false,
                isGoal: stmt.isGoal,
                isBkm: stmt.isBkm,
                cont: stmt.cont->strToCont(
                    ~preCtxColors=st.preCtxData.symColors,
                    ~wrkCtxColors=st.wrkCtxColors
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

let editorHistMake = (~initState:editorState, ~maxLength:int):editorHistory => {
    {
        ver:version,
        maxLength: Math.Int.max(0, Math.Int.min(1000, maxLength)),
        head: initState->editorSnapshotMake,
        prev: []
    }
}

let prependDiffToFirstElem = (diff:array<editorDiff>, prev:array<array<editorDiff>>, maxLength:int) => {
    if (diff->Array.length == 0) {
        throw(MmException({msg:`diff->Array.length == 0`}))
    }
    if (prev->Array.length == 0) {
        throw(MmException({msg:`prev->Array.length == 0`}))
    }
    [ diff->Array.concat(prev->Array.getUnsafe(0)) ]->Array.concat(prev->Array.slice(~start=1, ~end=maxLength))
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
                prev: [],
            }
        } else {
            let diff = newHead->findDiff(ht.head)
            if (diff->Array.length == 0 || diff->allStatusSet) {
                ht
            } else if (diff->allStatusUnset) {
                if (ht.prev->Array.length == 0) {
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
            } else if (diff->allMoveAndStatusSet && ht.prev->Array.length > 0 && ht.prev->Array.getUnsafe(0)->allMoveAndStatusSet) {
                {
                    ...ht,
                    head: newHead,
                    prev: prependDiffToFirstElem(diff, ht.prev, ht.maxLength),
                }
            } else {
                {
                    ...ht,
                    head: newHead,
                    prev: [diff]->Array.concat(ht.prev->Array.slice(~start=0, ~end=ht.maxLength-1))
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
            prev:ht.prev->Array.slice(~start=0,~end=maxLength)
        }
    }
}

let editorHistIsEmpty = (ht:editorHistory):bool => {
    ht.prev->Array.length == 0
}

let editorHistLength = (ht:editorHistory):int => {
    ht.prev->Array.length
}

let editorHistGetSnapshotPreview = (ht:editorHistory, idx:int, st:editorState): result<editorState,string> => {
    if (idx == -1) {
        Ok(st->updateEditorStateFromSnapshot(ht.head)->recalcWrkColors)
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
            stmts: st.stmts->Array.map(stmt => {
                {
                    ...stmt,
                    proofStatus: None,
                }
            })
        }
    })
}

let selectMaxId = (id1:option<int>, id2:option<int>):option<int> => {
    switch id1 {
        | None => id2
        | Some(id1Int) => {
            switch id2 {
                | None => id1
                | Some(id2Int) => Some(Math.Int.max(id1Int, id2Int))
            }
        }
    }
}

let getMaxIntStmtIdFromDiffs = (diffs:array<editorDiff>):option<int> => {
    diffs->Array.reduce(None, (maxId, diff) => {
        switch diff {
            | Descr(_) => maxId
            | Vars(_) => maxId
            | Disj(_) => maxId
            | StmtLabel({stmtId}) => selectMaxId(maxId, stmtId->Int.fromString)
            | StmtTyp({stmtId}) => selectMaxId(maxId, stmtId->Int.fromString)
            | StmtBkm({stmtIds}) | StmtStatus({stmtIds}) | StmtRemove({stmtIds}) => {
                stmtIds->Array.reduce(maxId, (maxId,stmtId) => selectMaxId(maxId, stmtId->Int.fromString))
            }
            | StmtJstf({stmtId}) => selectMaxId(maxId, stmtId->Int.fromString)
            | StmtCont({stmtId}) => selectMaxId(maxId, stmtId->Int.fromString)
            | StmtAdd({stmt: {id}}) => selectMaxId(maxId, id->Int.fromString)
            | StmtMove({stmtId}) => selectMaxId(maxId, stmtId->Int.fromString)
        }
    })
}

let editorHistGetMaxIntStmtId = (ht:editorHistory):option<int> => {
    let maxId = ht.head.stmts->Array.reduce(None, (maxId, stmt) => selectMaxId(maxId, stmt.id->Int.fromString))
    let maxId = ht.prev->Array.reduce(maxId, (maxId, diffs) => selectMaxId(maxId, getMaxIntStmtIdFromDiffs(diffs)))
    maxId
}

let editorStatesHaveSameContent = (st1:editorState, st2:editorState):bool => {
    let sn1 = st1->editorSnapshotMake
    let sn2 = st2->editorSnapshotMake
    sn1.descr == sn2.descr
        && sn1.varsText == sn2.varsText
        && sn1.disjText == sn2.disjText
        && sn1.stmts->Array.length == sn2.stmts->Array.length
        && sn1.stmts->Array.reduceWithIndex(true, (res, stmt1, idx) => {
            if (!res) {
                res
            } else {
                let stmt2 = sn2.stmts->Array.getUnsafe(idx)
                stmt1.label == stmt2.label
                    && stmt1.typ == stmt2.typ
                    && stmt1.isGoal == stmt2.isGoal
                    && stmt1.isBkm == stmt2.isBkm
                    && stmt1.jstfText == stmt2.jstfText
                    && stmt1.cont == stmt2.cont
            }
        })
}

let editorHistoryPrintIdInfo = (ht:editorHistory):unit => {
    Console.log(`--- History id info ---------------------------`)
    Console.log2("maxStmtId", ht->editorHistGetMaxIntStmtId)
    ht.prev->Array.forEachWithIndex((diffs,i) => {
        let addRemoveStmts = diffs->Array.map(diff => {
            switch diff {
                | StmtAdd({stmt:{id}}) => Some("+" ++ id)
                | StmtRemove({stmtIds}) => Some("-" ++ stmtIds->Array.join(","))
                | _ => None
            }
        })->Array.filter(Option.isSome(_))->Array.map(Option.getOrThrow(_))->Array.join(", ")
        if (addRemoveStmts->String.length > 0) {
            Console.log(`${Int.toString(i+1)}: ${addRemoveStmts}`)
        }
    })
    Console.log(`-----------------------------------------------`)
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
    ver: int,
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
        | _ => throw(MmException({msg:`Cannot convert '${str}' to proofStatus.`}))
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
        s: sn.stmts->Array.map(stmtSnapshotToLocStor)
    }
}

let editorSnapshotFromLocStor = (sn:editorSnapshotLocStor):editorSnapshot => {
    {
        descr: sn.d,
        varsText: sn.v,
        disjText: sn.j,
        stmts: sn.s->Array.map(stmtSnapshotFromLocStor)
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
        | StmtBkm({stmtIds, isBkm}) => 
            { t:"SB", a:stmtIds, b:isBkm }
        | StmtJstf({stmtId, jstfText}) => 
            { t:"SJ", d:stmtId, s:jstfText }
        | StmtCont({stmtId, cont}) => 
            { t:"SC", d:stmtId, s:cont }
        | StmtStatus({stmtIds, proofStatus}) => 
            { t:"SS", a:stmtIds, s:?(proofStatus->Belt_Option.map(proofStatusToStr)) }
        | StmtAdd({idx, stmt}) => 
            { t:"SA", i:idx, m:stmt->stmtSnapshotToLocStor }
        | StmtRemove({stmtIds}) => 
            { t:"SR", a:stmtIds }
        | StmtMove({stmtId, idx}) => 
            { t:"SM", d:stmtId, i:idx }
    }
}

let optGetEdls = (opt:option<'a>, typ:string, attrName:string):'a => {
    switch opt {
        | None => throw(MmException({msg:`'${attrName}' is not set in an editorDiffLocStor for typ = '${typ}'.`}))
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
        | "SB" => StmtBkm({stmtIds:diff->edlsGetIds, isBkm:diff->edlsGetBool})
        | "SJ" => StmtJstf({stmtId:diff->edlsGetId, jstfText:diff->edlsGetStr})
        | "SC" => StmtCont({stmtId:diff->edlsGetId, cont:diff->edlsGetStr})
        | "SS" => StmtStatus({stmtIds:diff->edlsGetIds, proofStatus:diff.s->Belt.Option.map(proofStatusFromStr)})
        | "SA" => StmtAdd({idx:diff->edlsGetInt, stmt:diff->edlsGetStmt->stmtSnapshotFromLocStor})
        | "SR" => StmtRemove({stmtIds:diff->edlsGetIds})
        | "SM" => StmtMove({stmtId:diff->edlsGetId, idx:diff->edlsGetInt})
        | _ => throw(MmException({msg:`Cannot convert editorDiffLocStor to editorDiff for diff.typ='${diff.t}'.`}))
    }
}

let editorHistoryToLocStor = (ht:editorHistory):editorHistoryLocStor => {
    {
        ver: ht.ver,
        head: ht.head->editorSnapshotToLocStor,
        prev: ht.prev->Array.map(diff => diff->Array.map(editorDiffToLocStor)),
        maxLength: ht.maxLength,
    }
}

let editorHistoryFromLocStor = (ht:editorHistoryLocStor):editorHistory => {
    {
        ver: ht.ver,
        head: ht.head->editorSnapshotFromLocStor,
        prev: ht.prev->Array.map(diff => diff->Array.map(editorDiffFromLocStor)),
        maxLength: ht.maxLength,
    }
}

let editorHistToString = (ht:editorHistory):string => {
    Expln_utils_common.stringify(ht->editorHistoryToLocStor)
}

let parseStmtSnapshotLocStor = (d:Expln_utils_jsonParse.jsonAny):stmtSnapshotLocStor => {
    open Expln_utils_jsonParse
    {
        d: d->str("d"),
        l: d->str("l"),
        t: d->str("t"),
        g: d->bool("g"),
        b: d->bool("b"),
        j: d->str("j"),
        c: d->str("c"),
        p: d->strOpt("p"),
    }
}

let editorHistFromString = (jsonStr:string):result<editorHistory,string> => {
    open Expln_utils_jsonParse
    parseJson(jsonStr, asObj(_, d=>{
        {
            ver: d->int(
                "ver", 
                ~validator = v => {
                    v == version
                        ?Ok(v)
                        :Error(
                            `History protocol version mismatch, ` 
                                ++ `expected ${version->Int.toString}, got ${v->Int.toString}.`
                        )
                }
            ),
            head: d->obj("head", d=>{
                {
                    d: d->str("d"),
                    v: d->str("v"),
                    j: d->str("j"),
                    s: d->arr("s", parseStmtSnapshotLocStor)
                }
            }),
            prev: d->arr("prev", d=>{
                d->asArr(d=>{
                    {
                        t: d->str("t"),
                        d: ?d->strOpt("d"),
                        b: ?d->boolOpt("b"),
                        i: ?d->intOpt("i"),
                        s: ?d->strOpt("s"),
                        m: ?d->objOpt("m", parseStmtSnapshotLocStor),
                        a: ?d->arrOpt("a", asStr(_)),
                    }
                })
            }),
            maxLength: d->int("maxLength")
        }
    }))->Belt.Result.map(editorHistoryFromLocStor)
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
    if (sn.descr->String.length > 0) {
        res->Array.push("")
    }

    res->Array.push("Variables")
    res->Array.push(sn.varsText)
    if (sn.varsText->String.length > 0) {
        res->Array.push("")
    }

    res->Array.push("Disjoints")
    res->Array.push(sn.disjText)
    if (sn.disjText->String.length > 0) {
        res->Array.push("")
    }

    sn.stmts->Array.forEach(stmt => {
        res->Array.push(stmt->stmtSnapshotToStringExtended)
    })
    res->Array.joinUnsafe("\n")
}

let diffToStringExtendedSingle = (diff:editorDiff):string => {
    switch diff {
        | Descr(descr) => `Descr(${descr})`
        | Vars(varsText) => `Vars(${varsText->String.replaceRegExp(%re("/[\n\r]+/g"), " ; ")})`
        | Disj(disjText) => `Disj(${disjText->String.replaceRegExp(%re("/[\n\r]+/g"), " ; ")})`
        | StmtLabel({stmtId, label}) => `StmtLabel({stmtId=${stmtId}, label=${label}})`
        | StmtTyp({stmtId, typ, isGoal}) => 
            `StmtTyp({stmtId=${stmtId}, typ=${typ->userStmtTypeToStr}, isGoal=${isGoal->Expln_utils_common.stringify}})`
        | StmtBkm({stmtIds, isBkm}) => 
            `StmtBkm({stmtIds=[${stmtIds->Array.joinUnsafe(", ")}], isBkm=${isBkm->Expln_utils_common.stringify}})`
        | StmtJstf({stmtId, jstfText}) => `StmtJstf({stmtId=${stmtId}, jstfText=${jstfText}})`
        | StmtCont({stmtId, cont}) => `StmtCont({stmtId=${stmtId}, cont=${cont}})`
        | StmtStatus({stmtIds, proofStatus}) => 
            `StmtStatus({stmtIds=[${stmtIds->Array.joinUnsafe(", ")}], ` 
                ++ `proofStatus=${proofStatus->Belt_Option.map(proofStatusToStr)->Belt_Option.getWithDefault("None")}})`
        | StmtAdd({idx, stmt}) => `StmtAdd({idx=${idx->Belt.Int.toString}, stmtId=${stmt.id}})`
        | StmtRemove({stmtIds}) => `StmtRemove({stmtIds=[${stmtIds->Array.joinUnsafe(", ")}]})`
        | StmtMove({stmtId, idx}) => `StmtMove({stmtId=${stmtId}, idx=${idx->Belt.Int.toString}})`
    }
}

let diffToStringExtended = (diff:array<editorDiff>):string => {
    diff->Array.map(diffToStringExtendedSingle)->Array.joinUnsafe("\n")
}

let editorHistToStringExtended = (ht:editorHistory):string => {
    let res = []
    let delim1 = "------------------------------------------------------------------------------------"
    let delim2 = "===================================================================================="
    let curSn = ref(ht.head)
    res->Array.push(curSn.contents->editorSnapshotToStringExtended)
    res->Array.push(delim2)
    ht.prev->Array.forEach(diff => {
        res->Array.push(diff->diffToStringExtended)
        res->Array.push(delim1)
        curSn := curSn.contents->applyDiff(diff)
        res->Array.push(curSn.contents->editorSnapshotToStringExtended)
        res->Array.push(delim2)
    })
    res->Array.joinUnsafe("\n")
}
