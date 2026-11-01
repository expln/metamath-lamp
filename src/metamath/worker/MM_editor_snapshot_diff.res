open MM_wrk_editor
open Common

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
    | StmtBkm({stmtIds: array<stmtId>, isBkm: bool})
    | StmtJstf({stmtId: stmtId, jstfText: string})
    | StmtCont({stmtId: stmtId, cont: string})
    | StmtStatus({stmtIds: array<stmtId>, proofStatus: option<proofStatus>})
    | StmtAdd({idx: int, stmt: stmtSnapshot})
    | StmtRemove({stmtIds: array<stmtId>})
    | StmtMove({stmtId: stmtId, idx: int})

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

module ProofStatusHash = Belt.Id.MakeHashable({
    type t = option<proofStatus>
    let hash = (status:option<proofStatus>):int => {
        switch status {
            | None => 0
            | Some(Ready) => 1
            | Some(Waiting) => 2
            | Some(NoJstf) => 3
            | Some(JstfIsIncorrect) => 4
        }
    }
    let eq = proofStatusEq
})

module BoolHash = Belt.Id.MakeHashable({
    type t = bool
    let hash = (b:bool):int => b?1:0
    let eq = (a,b) => a === b
})

let isStmtStatusRemove = (diff:editorDiff):bool => {
    switch diff {
        | StmtStatus({proofStatus:None}) => true
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
    diff->Array.every(isStmtStatusRemove)
}

let allStatusSet = (diff:array<editorDiff>):bool => {
    diff->Array.every(isStmtStatusSet)
}

let allMoveAndStatusSet = (diff:array<editorDiff>):bool => {
    diff->Array.every(d => isStmtMove(d) || isStmtStatusSet(d))
}

let updateStmt = (sn:editorSnapshot, stmtId:stmtId, update:stmtSnapshot=>stmtSnapshot):editorSnapshot => {
    {
        ...sn,
        stmts:sn.stmts->Array.map(stmt => if (stmt.id == stmtId) {stmt->update} else {stmt})
    }
}

let addStmt = (sn:editorSnapshot, idx:int, stmt:stmtSnapshot):editorSnapshot => {
    let newStmts = sn.stmts->Array.copy
    newStmts->Array.splice(~start=idx, ~remove=0, ~insert=[stmt])
    {
        ...sn,
        stmts:newStmts
    }
}

let removeStmt = (sn:editorSnapshot, stmtId:stmtId):editorSnapshot => {
    {
        ...sn,
        stmts:sn.stmts->Array.filter(stmt => stmt.id != stmtId)
    }
}

let moveStmt = (sn:editorSnapshot, stmtId:stmtId, idx:int):editorSnapshot => {
    switch sn.stmts->Array.find(stmt => stmt.id == stmtId) {
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
        | StmtBkm({stmtIds, isBkm}) => {
            {
                ...sn,
                stmts: sn.stmts->Array.map(stmt => {
                    if (stmtIds->Array.includes(stmt.id)) {
                        {...stmt, isBkm}
                    } else {
                        stmt
                    }
                })
            }
        }
        | StmtJstf({stmtId, jstfText}) => sn->updateStmt(stmtId, stmt => {...stmt, jstfText})
        | StmtCont({stmtId, cont}) => sn->updateStmt(stmtId, stmt => {...stmt, cont})
        | StmtStatus({stmtIds, proofStatus}) => {
            {
                ...sn,
                stmts: sn.stmts->Array.map(stmt => {
                    if (stmtIds->Array.includes(stmt.id)) {
                        {...stmt, proofStatus}
                    } else {
                        stmt
                    }
                })
            }
        }
        | StmtAdd({idx, stmt}) => sn->addStmt(idx, stmt)
        | StmtRemove({stmtIds}) => {
            {
                ...sn,
                stmts: sn.stmts->Array.filter(stmt => !(stmtIds->Array.includes(stmt.id)))
            }
        }
        | StmtMove({stmtId, idx}) => sn->moveStmt(stmtId, idx)
    }
}

/*
If findDiff(a,b)==diff then applyDiff(a,diff)==b
*/
let applyDiff = (sn:editorSnapshot, diff:array<editorDiff>):editorSnapshot => {
    diff->Array.reduce( sn, applyDiffSingle )
}

let findDiff = (a:editorSnapshot, b:editorSnapshot):array<editorDiff> => {
    let diffs = []

    let aIds = a.stmts->Array.map(stmt => stmt.id)->Belt_HashSetString.fromArray
    let bIds = b.stmts->Array.map(stmt => stmt.id)->Belt_HashSetString.fromArray
    let stmtIdsToRemove = []
    a.stmts->Array.forEach(stmtA => {
        if (!(bIds->Belt_HashSetString.has(stmtA.id))) {
            stmtIdsToRemove->Array.push(stmtA.id)
        }
    })
    if (stmtIdsToRemove->Array.length != 0) {
        diffs->Array.push(StmtRemove({stmtIds:stmtIdsToRemove}))
    }
    b.stmts->Array.forEachWithIndex((stmtB,i) => {
        if (!(aIds->Belt_HashSetString.has(stmtB.id))) {
            diffs->Array.push(StmtAdd({idx:i, stmt:stmtB}))
        }
    })

    let aMod = ref(a->applyDiff(diffs))
    let aModLen = aMod.contents.stmts->Array.length
    let bLen = b.stmts->Array.length
    if (aModLen != bLen) {
        let aIdsStr = "["++a.stmts->Array.map(stmt => stmt.id)->Array.join(",")++"]"
        let bIdsStr = "["++b.stmts->Array.map(stmt => stmt.id)->Array.join(",")++"]"
        throw(MmException({
            msg: `aModLen != bLen (aModLen=${aModLen->Int.toString}, bLen=${bLen->Int.toString}, `
                ++ `aIds=${aIdsStr}, bIds=${bIdsStr})`
        }))
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

    let isBkms:Belt_HashMap.t<bool,array<stmtId>,BoolHash.identity>
        = Belt_HashMap.make(~hintSize=2, ~id=module(BoolHash))
    let proofStatuses:Belt_HashMap.t<option<proofStatus>,array<stmtId>,ProofStatusHash.identity>
        = Belt_HashMap.make(~hintSize=5, ~id=module(ProofStatusHash))
    b.stmts->Array.forEachWithIndex((stmtB,i) => {
        let stmtA = aMod.contents.stmts->Array.getUnsafe(i)
        if (stmtA.id != stmtB.id) {
            throw(MmException({msg:`stmtA.id != stmtB.id`}))
        }
        if (stmtA.label != stmtB.label) {
            diffs->Array.push(StmtLabel({stmtId:stmtB.id, label:stmtB.label}))
        }
        if (stmtA.typ != stmtB.typ || stmtA.isGoal != stmtB.isGoal) {
            diffs->Array.push(StmtTyp({stmtId:stmtB.id, typ:stmtB.typ, isGoal:stmtB.isGoal}))
        }
        if (stmtA.isBkm != stmtB.isBkm) {
            switch isBkms->Belt_HashMap.get(stmtB.isBkm) {
                | None => isBkms->Belt_HashMap.set(stmtB.isBkm, [stmtB.id])
                | Some(stmtIds) => stmtIds->Array.push(stmtB.id)
            }
        }
        if (stmtA.jstfText != stmtB.jstfText) {
            diffs->Array.push(StmtJstf({stmtId:stmtB.id, jstfText:stmtB.jstfText}))
        }
        if (stmtA.cont != stmtB.cont) {
            diffs->Array.push(StmtCont({stmtId:stmtB.id, cont:stmtB.cont}))
        }
        if (!(stmtA.proofStatus->proofStatusEq(stmtB.proofStatus))) {
            switch proofStatuses->Belt_HashMap.get(stmtB.proofStatus) {
                | None => proofStatuses->Belt_HashMap.set(stmtB.proofStatus, [stmtB.id])
                | Some(stmtIds) => stmtIds->Array.push(stmtB.id)
            }
        }
    })
    isBkms->Belt_HashMap.forEach((isBkm,stmtIds) => {
        diffs->Array.push(StmtBkm({stmtIds, isBkm}))
    })
    proofStatuses->Belt_HashMap.forEach((proofStatus,stmtIds) => {
        diffs->Array.push(StmtStatus({stmtIds, proofStatus}))
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

    diffs
}
