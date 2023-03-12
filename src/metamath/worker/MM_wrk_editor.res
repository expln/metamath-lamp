open MM_context
open MM_parser
open MM_proof_tree
open MM_proof_tree_dto
open MM_syntax_tree
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_wrk_ctx_data
open MM_provers
open MM_statements_dto

let newLabelPrefix = ""

type stmtSym = {
    id: string,
    sym: string,
    color: option<string>,
}

type stmtCont =
    | Text(array<stmtSym>)
    | Tree(syntaxTreeNode)

let contIsEmpty = cont => {
    switch cont {
        | Text(arr) => arr->Js_array2.length == 0
        | Tree(syntaxTreeNode) => syntaxTreeIsEmpty(syntaxTreeNode)
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text(arr) => arr->Js_array2.map(stmtSym => stmtSym.sym)
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let contToStr = cont => {
    cont->contToArrStr->Js_array2.joinWith(" ")
}

let strToCont = (
    str:string,
    ~preCtxColors: option<Belt_HashMapString.t<string>>=?,
    ~wrkCtxColors: option<Belt_HashMapString.t<string>>=?,
    ()
) => {
    Text(
        getSpaceSeparatedValuesAsArray(str)->Js.Array2.mapi((sym,i) => {
            {
                id: i->Belt.Int.toString ++ sym,
                sym,
                color:
                    switch preCtxColors->Belt_Option.flatMap(map => map->Belt_HashMapString.get(sym)) {
                        | Some(color) => Some(color)
                        | None => {
                            switch wrkCtxColors->Belt_Option.flatMap(map => map->Belt_HashMapString.get(sym)) {
                                | Some(color) => Some(color)
                                | None => None
                            }
                        }
                    }
            }
        })
    )
}

type userStmtType = E | P

let userStmtTypeFromStr = str => {
    switch str {
        | "e" => E
        | "p" => P
        | _ => raise(MmException({msg:`Cannot convert '${str}' to userStmtType`}))
    }
}

let userStmtTypeToStr = typ => {
    switch typ {
        | E => "e"
        | P => "p"
    }
}

type stmtId = string

type proofStatus = Ready | Waiting | NoJstf | JstfIsIncorrect

type userStmt = {
    id: stmtId,

    label: string,
    labelEditMode: bool,
    typ: userStmtType,
    typEditMode: bool,
    cont: stmtCont,
    contEditMode: bool,
    
    jstfText: string,
    jstfEditMode: bool,

    stmtErr: option<string>,

    expr: option<expr>,
    jstf: option<jstf>,
    proof: option<(proofTreeDto, proofNodeDto)>,
    proofStatus: option<proofStatus>,
}

type editorState = {
    settingsV:int,
    settings:settings,
    typeColors: Belt_HashMapString.t<string>,

    preCtxV: int,
    preCtx: mmContext,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    preCtxColors: Belt_HashMapString.t<string>,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkCtx: option<mmContext>,
    wrkCtxColors: Belt_HashMapString.t<string>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<stmtId>,

    unifyAllIsRequiredCnt: int
}

type wrkSubsErr =
    | CommonVar({var1:int, var2:int, commonVar:int})
    | TypeMismatch({var:int, subsExpr:expr, typeExpr:expr})

type wrkSubs = {
    newDisj: disjMutable,
    subs: Belt_MapInt.t<expr>,
    mutable err: option<wrkSubsErr>,
}

let createEmptyUserStmt = (id, typ, label):userStmt => {
    { 
        id, 
        label, labelEditMode:false, 
        typ, typEditMode:false, 
        cont:Text([]), contEditMode:true,
        jstfText:"", jstfEditMode:false,
        stmtErr: None,
        expr:None, jstf:None, proof:None, proofStatus:None,
    }
}

let userStmtToRootStmt = (stmt:userStmt):rootStmt => {
    {
        isHyp:stmt.typ == E,
        label:stmt.label,
        expr:
            switch stmt.expr {
                | None => raise(MmException({msg:`Expr must be set for a userStmt before converting to rootStmt.`}))
                | Some(expr) => expr
            },
        jstf: stmt.jstf,
    }
}

let getStmtByIdExn = (st:editorState,id:stmtId):userStmt => {
    switch st.stmts->Js_array2.find(stmt => stmt.id == id) {
        | None => raise(MmException({msg:`getStmtByIdExn: Cannot find a statement by statement id.`}))
        | Some(stmt) => stmt
    }
}

let getStmtIdx = (st:editorState,id:stmtId):int => {
    let res = ref(-1)
    st.stmts->Js_array2.forEachi((stmt,i) => {
        if (stmt.id == id) {
            res.contents = i
        }
    })
    res.contents
}

let updateStmt = (st:editorState,id,update):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let isStmtChecked = (st,id) => {
    st.checkedStmtIds->Js.Array2.includes(id)
}

let toggleStmtChecked = (st,id) => {
    if (isStmtChecked(st,id)) {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.filter(checkedId => checkedId != id)
        }
    } else {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.concat([id])
        }
    }
}

let checkAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: st.stmts->Js.Array2.map(stmt => stmt.id)
    }
}

let uncheckAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: []
    }
}

let deleteCheckedStmts = (st:editorState):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.filter(stmt => !isStmtChecked(st,stmt.id)),
        checkedStmtIds: [],
    }
}

let deleteStmt = (st:editorState, id:stmtId):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.filter(stmt => stmt.id != id),
        checkedStmtIds: st.checkedStmtIds->Js_array2.filter(checkedId => checkedId != id),
    }
}

let canMoveCheckedStmts = (st:editorState, up):bool => {
    let len = st.stmts->Js_array2.length
    len != 0 && st.checkedStmtIds->Js_array2.length != 0 && (
        (up && !isStmtChecked(st,st.stmts[0].id)) || (!up && !isStmtChecked(st,st.stmts[len-1].id))
    )
}

let moveCheckedStmts = (st:editorState,up):editorState => {
    if (!canMoveCheckedStmts(st,up)) {
        st
    } else {
        let len = st.stmts->Js_array2.length
        let res = st.stmts->Js.Array2.copy
        if up {
            let maxI = len-2
            for i in 0 to maxI {
                if (!isStmtChecked(st,res[i].id) && isStmtChecked(st,res[i+1].id)) {
                    let tmp = res[i]
                    res[i] = res[i+1]
                    res[i+1] = tmp
                }
            }
        } else {
            for i in len-1 downto 1 {
                if (isStmtChecked(st,res[i-1].id) && !isStmtChecked(st,res[i].id)) {
                    let tmp = res[i]
                    res[i] = res[i-1]
                    res[i-1] = tmp
                }
            }
        }
        {
            ...st,
            stmts: res,
        }
    }
}

let getAllStmtsUpToChecked = (st):array<userStmt> => {
    let checkedAdded = ref(false)
    let res = []
    let i = ref(0)
    let stmtsLen = st.stmts->Js_array2.length
    while (i.contents < stmtsLen && !checkedAdded.contents) {
        let stmt = st.stmts[i.contents]
        res->Js.Array2.push(stmt)->ignore
        checkedAdded.contents = st.checkedStmtIds->Js.Array2.includes(stmt.id)
        i.contents = i.contents + 1
    }
    res
}

let editorGetStmtById = (st,id) => st.stmts->Js_array2.find(stmt => stmt.id == id)

let getRootStmtsForUnification = (st):array<userStmt> => {
    st->getAllStmtsUpToChecked
}

let getRootProvablesForUnification = (st):array<rootStmt> => {
    st->getRootStmtsForUnification
        ->Js_array2.filter(stmt => stmt.typ == P)
        ->Js_array2.map(userStmtToRootStmt)
}

let createNewLabel = (st:editorState, prefix:string):string => {
    let isLabelDefinedInCtx = label => {
        switch st.wrkCtx {
            | Some(wrkCtx) => wrkCtx->isHyp(label) || wrkCtx->isAsrt(label)
            | None => false
        }
    }
    
    let usedLabels = st.stmts->Js_array2.map(stmt=>stmt.label)
    let i = ref(1)
    let newLabel = ref(prefix ++ i.contents->Belt_Int.toString)
    while (usedLabels->Js.Array2.includes(newLabel.contents) || isLabelDefinedInCtx(newLabel.contents)) {
        i.contents = i.contents + 1
        newLabel.contents = prefix ++ i.contents->Belt_Int.toString
    }
    newLabel.contents
}

let addNewStmt = (st:editorState):(editorState,stmtId) => {
    let newId = st.nextStmtId->Belt_Int.toString
    let newLabel = createNewLabel(st, newLabelPrefix)
    let idToAddBefore = st.stmts->Js_array2.find(stmt => st.checkedStmtIds->Js_array2.includes(stmt.id))->Belt_Option.map(stmt => stmt.id)
    (
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                switch idToAddBefore {
                    | Some(idToAddBefore) => {
                        st.stmts->Js_array2.map(stmt => {
                            if (stmt.id == idToAddBefore) {
                                [createEmptyUserStmt(newId,P,newLabel), stmt]
                            } else {
                                [stmt]
                            }
                        })->Belt_Array.concatMany
                    }
                    | None => st.stmts->Js_array2.concat([createEmptyUserStmt(newId, P, newLabel)])
                }
        },
        newId
    )
}

let addNewStmtAtIdx = (st:editorState, idx:int):(editorState,stmtId) => {
    let savedCheckedStmtIds = st.checkedStmtIds
    let st = st->uncheckAllStmts
    let st = if (0 <= idx && idx < st.stmts->Js_array2.length) {
        st->toggleStmtChecked(st.stmts[idx].id)
    } else {
        st
    }
    let (st,stmtId) = st->addNewStmt
    let st = {...st, checkedStmtIds:savedCheckedStmtIds}
    (st,stmtId)
}

let isSingleStmtChecked = st => st.checkedStmtIds->Js_array2.length == 1

let duplicateCheckedStmt = st => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let newLabel = createNewLabel(st, newLabelPrefix)
        let idToAddAfter = st.checkedStmtIds[0]
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                st.stmts->Js_array2.map(stmt => {
                    if (stmt.id == idToAddAfter) {
                        [stmt, {...stmt, id:newId, label:newLabel, jstfText:""}]
                    } else {
                        [stmt]
                    }
                })->Belt_Array.concatMany,
            checkedStmtIds: [newId],
        }
    }
}

let canGoEditModeForStmt = (st:editorState,stmtId) => {
    !(st.stmts->Js_array2.some(stmt => 
        stmt.id == stmtId && (stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode)
    ))
}

let setVarsEditMode = st => {
    {
        ...st,
        varsEditMode: true
    }
}

let setDisjEditMode = st => {
    {
        ...st,
        disjEditMode: true
    }
}

let completeDisjEditMode = (st, newDisjText) => {
    {
        ...st,
        disjText:newDisjText,
        disjEditMode: false
    }
}

let setLabelEditMode = (st:editorState, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, labelEditMode:true})
    } else {
        st
    }
}

let completeLabelEditMode = (st, stmtId, newLabel):editorState => {
    updateStmt(st, stmtId, stmt => {
        if (newLabel->Js_string2.trim != "") {
            {
                ...stmt,
                label:newLabel,
                labelEditMode: false
            }
        } else {
            stmt
        }
    })
}

let setContEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, contEditMode:true})
    } else {
        st
    }
}

let completeContEditMode = (st, stmtId, newContText):editorState => {
    updateStmt(st, stmtId, stmt => {
        if (newContText->Js_string2.trim == "") {
            stmt
        } else {
            {
                ...stmt,
                cont:strToCont(newContText, ~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors, ()),
                contEditMode: false
            }
        }
    })
}

let setTypEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, typEditMode:true})
    } else {
        st
    }
}

let completeTypEditMode = (st, stmtId, newTyp) => {
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            typ:newTyp,
            typEditMode: false
        }
    })
}

let setJstfEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, jstfEditMode:true})
    } else {
        st
    }
}

let completeJstfEditMode = (st, stmtId, newJstf) => {
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            jstfText:newJstf,
            jstfEditMode: false
        }
    })
}

let incUnifyAllIsRequiredCnt = st => {
    {
        ...st,
        unifyAllIsRequiredCnt: st.unifyAllIsRequiredCnt + 1
    }
}

let getTypeAndVarFromVarsTextLine = (str):option<(string,string)> => {
    let arr = getSpaceSeparatedValuesAsArray(str)
    if (arr->Js_array2.length != 3) {
        None
    } else {
        Some((arr[1], arr[2]))
    }
}

let newLineRegex = %re("/[\n\r]/")
let extractVarColorsFromVarsText = (varsText, typeColors:Belt_HashMapString.t<string>):Belt_HashMapString.t<string> => {
    let res = Belt_HashMapString.make(~hintSize=16)
    varsText->Js_string2.splitByRe(newLineRegex)->Js_array2.forEach(lineOpt => {
        switch lineOpt {
            | None => ()
            | Some(line) => {
                switch getTypeAndVarFromVarsTextLine(line->Js_string2.trim) {
                    | None => ()
                    | Some((typeStr,varStr)) => {
                        switch typeColors->Belt_HashMapString.get(typeStr) {
                            | None => ()
                            | Some(color) => {
                                res->Belt_HashMapString.set(varStr, color)
                            }
                        }
                    }
                }
            }
        }
    })
    res
}

let recalcTypeColors = (st:editorState):editorState => {
    {
        ...st,
        typeColors: st.settings.typeSettings
            ->Js_array2.map(ts => (ts.typ, ts.color))
            ->Belt_HashMapString.fromArray
    }
}

let recalcPreCtxColors = (st:editorState):editorState => {
    let varColorsArr = []
    st.preCtx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F) {
            switch st.preCtx->ctxIntToSym(hyp.expr[0]) {
                | None => ()
                | Some(typeStr) => {
                    switch st.typeColors->Belt_HashMapString.get(typeStr) {
                        | None => ()
                        | Some(color) => {
                            varColorsArr->Js_array2.push((
                                st.preCtx->ctxIntToSymExn(hyp.expr[1]),
                                color,
                            ))->ignore
                        }
                    }
                }
            }
        }
        None
    })->ignore
    {
        ...st,
        preCtxColors: Belt_HashMapString.fromArray(varColorsArr),
    }
}

let recalcWrkCtxColors = (st:editorState):editorState => {
    {
        ...st,
        wrkCtxColors: extractVarColorsFromVarsText(st.varsText, st.typeColors),
    }
}

let recalcAllColors = st => {
    let st = recalcTypeColors(st)
    let st = recalcPreCtxColors(st)
    let st = recalcWrkCtxColors(st)
    st
}

let updateColorsInAllStmts = st => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => {
            ...stmt,
            cont: stmt.cont->contToStr->strToCont(~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors, ())
        })
    }
}

let completeVarsEditMode = (st, newVarsText) => {
    let st = {
        ...st,
        varsText:newVarsText,
        varsEditMode: false
    }
    let st = recalcWrkCtxColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let setSettings = (st, settingsV, settings) => {
    let st = { 
        ...st, 
        settingsV, 
        settings,
    }
    let st = recalcAllColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let setPreCtx = (st, preCtxV, preCtx) => {
    let st = { 
        ...st, 
        preCtxV, 
        preCtx, 
        frms: prepareFrmSubsData(~ctx=preCtx, ()),
        parenCnt: parenCntMake(prepareParenInts(preCtx, st.settings.parens), ())
    }
    let st = recalcPreCtxColors(st)
    let st = recalcWrkCtxColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let stableSortStmts = (st, comp: (userStmt,userStmt)=>int) => {
    let stmtsLen = st.stmts->Js.Array2.length
    if (stmtsLen < 2) {
        st
    } else {
        let newStmts = st.stmts->Js.Array2.copy
        let changed = ref(true)
        let e = ref(stmtsLen - 2)
        while (e.contents >= 0 && changed.contents) {
            changed.contents = false
            for i in 0 to e.contents {
                if (comp(newStmts[i], newStmts[i+1]) > 0) {
                    let tmp = newStmts[i]
                    newStmts[i] = newStmts[i+1]
                    newStmts[i+1] = tmp
                    changed.contents = true
                }
            }
            e.contents = e.contents - 1
        }
        {
            ...st,
            stmts: newStmts
        }
    }
}

let sortStmtsByType = st => {
    let stmtToInt = stmt => {
        switch stmt.typ {
            | E => 1
            | P => 2
        }
    }
    st->stableSortStmts((a,b) => stmtToInt(a) - stmtToInt(b))
}

let removeAllTempData = st => {
    {
        ...st,
        varsErr: None,
        disjErr: None,
        stmts: st.stmts->Js_array2.map(stmt => {
            {
                ...stmt, 
                stmtErr: None,
                expr: None, 
                jstf: None, 
                proof: None, 
                proofStatus: None
            }
        })
    }
}

let userStmtHasErrors = stmt => {
    stmt.stmtErr->Belt_Option.isSome
}

let editorStateHasErrors = st => {
    st.varsErr->Belt_Option.isSome ||
        st.disjErr->Belt_Option.isSome ||
        st.stmts->Js_array2.some(userStmtHasErrors)
}

let parseWrkCtxErr = (st:editorState, wrkCtxErr:wrkCtxErr):editorState => {
    let st = switch wrkCtxErr.varsErr {
        | None => st
        | Some(msg) => {
            {...st, varsErr:Some(msg)}
        }
    }
    let st = switch wrkCtxErr.disjErr {
        | None => st
        | Some(msg) => {
            {...st, disjErr:Some(msg)}
        }
    }
    let st = switch wrkCtxErr.hypErr {
        | None => st
        | Some((stmtId, msg)) => {
            st->updateStmt(stmtId, stmt => {...stmt, stmtErr:Some(msg)})
        }
    }
    st
}

let refreshWrkCtx = (st:editorState):editorState => {
    let st = sortStmtsByType(st)
    let st = removeAllErrorsInEditorState(st)
    let wrkCtxRes = createWrkCtx(
        ~preCtx=st.preCtx,
        ~varsText=st.varsText,
        ~disjText=st.disjText,
        ~hyps=st.stmts
            ->Js_array2.filter( stmt => stmt.typ == E)
            ->Js_array2.map( stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
    )
    let st = switch wrkCtxRes {
        | Error(wrkCtxErr) => parseWrkCtxErr(st, wrkCtxErr)
        | Ok(wrkCtx) => {
            let st = {...st, wrkCtx:Some(wrkCtx)}
            let st = st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (stmt.typ == E) {
                        st->updateStmt(stmt.id, stmt => {...stmt, expr:Some(wrkCtx->ctxSymsToIntsExn(stmt.cont->contToArrStr))})
                    } else {
                        st
                    }
                },
                st
            )
            st
        }
    }
    st
}

let parseJstf = (jstfText:string):result<option<jstf>,string> => {
    let jstfTrim = jstfText->Js_string2.trim
    if (jstfTrim->Js_string2.length == 0) {
        Ok(None)
    } else {
        let argsAndAsrt = jstfTrim->Js_string2.split(":")
        if (argsAndAsrt->Js_array2.length != 2) {
            Error(`Cannot parse justification: '${jstfText}' [1].`)
        } else {
            Ok(Some({
                args: argsAndAsrt[0]->getSpaceSeparatedValuesAsArray,
                label: argsAndAsrt[1]->Js_string2.trim
            }))
        }
    }
}

let setExprAndJstf = (stmt:userStmt,wrkCtx:mmContext):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        try {
            {
                ...stmt,
                expr: Some(wrkCtx->ctxSymsToIntsExn(stmt.cont->contToArrStr)),
                jstf: switch parseJstf(stmt.jstfText) {
                    | Error(msg) => raise(MmException({msg:msg}))
                    | Ok(jstf) => jstf
                }
            }
        } catch {
            | MmException({msg}) => {...stmt, stmtErr:Some(msg)}
        }
    }
}

let isLabelDefined = (~label:string, ~wrkCtx:mmContext, ~definedUserLabels:Belt_HashSetString.t) => {
    definedUserLabels->Belt_HashSetString.has(label) || wrkCtx->isHyp(label) || wrkCtx->isAsrt(label)
}

let validateJstfRefs = (stmt:userStmt, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch stmt.jstf {
            | None => stmt
            | Some({args,label}) => {
                switch args->Js_array2.find(ref => !isLabelDefined(ref,wrkCtx,usedLabels)) {
                    | Some(ref) => {
                        {...stmt, stmtErr:Some(`The reference '${ref}' is not defined.`)}
                    }
                    | None => {
                        if (!(wrkCtx->isAsrt(label))) {
                            {...stmt, stmtErr:Some(`The label '${label}' doesn't refer to any assertion.`)}
                        } else {
                            stmt
                        }
                    }
                }
            }
        }
    }
}

let validateStmtLabel = (stmt:userStmt, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        if (isLabelDefined(stmt.label,wrkCtx,usedLabels)) {
            {...stmt, stmtErr:Some(`Cannot reuse label '${stmt.label}'.`)}
        } else {
            stmt
        }
    }
}

let validateStmtIsUnique = (
    wrkCtx:mmContext,
    stmt:userStmt, 
    usedLabels:Belt_HashSetString.t,
    usedExprs:Belt_HashMap.t<expr,string,ExprHash.identity>,
):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        if (isLabelDefined(stmt.label,wrkCtx,usedLabels)) {
            {...stmt, stmtErr:Some(`Cannot reuse label '${stmt.label}'.`)}
        } else {
            stmt
        }
    }
    let stmt = switch stmt.expr {
        | None => raise(MmException({msg:`Cannot validateStmtIsUnique without expr.`}))
        | Some(expr) => {
            switch wrkCtx->getHypByExpr(expr) {
                | Some(hyp) if stmt.typ != E => {
                    {...stmt, stmtErr:Some(`This statement is the same as the previously defined` 
                        ++ ` hypothesis - '${hyp.label}'`)}
                }
                | _ => {
                    switch declaredStmts->Belt_HashMap.get(expr) {
                        | Some(prevStmtLabel) => {
                            {...stmt, stmtErr:Some(`This statement is the same as the previous` 
                                ++ ` one - '${prevStmtLabel}'`)}
                        }
                        | None => {
                            declaredStmts->Belt_HashMap.set(expr, stmt.label)
                            stmt
                        }
                    }
                }
            }
        }
    }
}

let checkAllStmtsAreUnique = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot checkAllStmtsAreUnique without wrkCtx.`}))
        | Some(wrkCtx) => {
            let declaredStmts = Belt_HashMap.make(~id=module(ExprHash), ~hintSize=st.stmts->Js.Array2.length)
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (editorStateHasErrors(st)) {
                        st
                    } else {
                        let stmt = switch stmt.expr {
                            | None => raise(MmException({msg:`Cannot checkAllStmtsAreUnique without expr.`}))
                            | Some(expr) => {
                                switch wrkCtx->getHypByExpr(expr) {
                                    | Some(hyp) if stmt.typ != E => {
                                        {...stmt, stmtErr:Some(`This statement is the same as the previously defined` 
                                            ++ ` hypothesis - '${hyp.label}'`)}
                                    }
                                    | _ => {
                                        switch declaredStmts->Belt_HashMap.get(expr) {
                                            | Some(prevStmtLabel) => {
                                                {...stmt, stmtErr:Some(`This statement is the same as the previous` 
                                                    ++ ` one - '${prevStmtLabel}'`)}
                                            }
                                            | None => {
                                                declaredStmts->Belt_HashMap.set(expr, stmt.label)
                                                stmt
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        st->updateStmt(stmt.id, _ => stmt)
                    }
                },
                st
            )
        }
    }
}

let prepareUserStmtsForUnification = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot prepareUserStmtsForUnification without wrkCtx.`}))
        | Some(wrkCtx) => {
            let usedLabels = Belt_HashMapString.make()
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (editorStateHasErrors(st)) {
                        st
                    } else {
                        let stmt = setExprAndJstf(stmt, wrkCtx)
                        let stmt = setExprAndJstf(stmt, wrkCtx)
                        let stmt = validateJstfRefs(stmt, wrkCtx, usedLabels)
                        let stmt = validateStmtLabel(stmt, wrkCtx, usedLabels)
                        usedLabels->Belt_MutableSetString.add(stmt.label)
                        st->updateStmt(stmt.id, _ => stmt)
                    }
                },
                st
            )
        }
    }
}

let prepareEditorForUnification = st => {
    [
        removeAllTempData,
        refreshWrkCtx,
        prepareUserStmtsForUnification,
        checkAllStmtsAreUnique,
    ]->Js.Array2.reduce(
        (st,act) => {
            if (editorStateHasErrors(st)) {
                st
            } else {
                act(st)
            }
        },
        st
    )
}

let getTopmostSelectedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Js.Array2.length == 0) {
        None
    } else {
        st.stmts->Js_array2.find(stmt => st.checkedStmtIds->Js_array2.includes(stmt.id))
    }
}

let getTheOnlySelectedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Js.Array2.length != 1) {
        None
    } else {
        let idToFind = st.checkedStmtIds[0]
        switch st.stmts->Js_array2.find(stmt => stmt.id == idToFind) {
            | None => None
            | Some(stmt) => Some(stmt)
        }
    }
}

let createNewVars = (st:editorState, varTypes:array<int>):(editorState,array<int>) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot create new variables without wrkCtx.`}))
        | Some(wrkCtx) => {
            let numOfVars = varTypes->Js_array2.length
            if (numOfVars == 0) {
                (st,[])
            } else {
                let typeToPrefix = Belt_MapString.fromArray(
                    st.settings.typeSettings->Js_array2.map(ts => (ts.typ, ts.prefix))
                )
                let newVarNames = generateNewVarNames(
                    ~ctx=wrkCtx,
                    ~types=varTypes, 
                    ~typeToPrefix,
                    ()
                )
                let newHypLabels = generateNewLabels(
                    ~ctx=wrkCtx,
                    ~prefix="var", 
                    ~amount=numOfVars,
                    ()
                )
                wrkCtx->applySingleStmt(Var({symbols:newVarNames}))
                let varTypeNames = wrkCtx->ctxIntsToSymsExn(varTypes)
                newHypLabels->Js.Array2.forEachi((label,i) => {
                    wrkCtx->applySingleStmt(Floating({label, expr:[varTypeNames[i], newVarNames[i]]}))
                })
                let newVarInts = wrkCtx->ctxSymsToIntsExn(newVarNames)
                let newVarsText = newHypLabels->Js.Array2.mapi((label,i) => {
                    `${label} ${varTypeNames[i]} ${newVarNames[i]}`
                })->Js_array2.joinWith("\n")
                let st = {
                    ...st,
                    varsText: st.varsText 
                                ++ (if (st.varsText->Js.String2.trim->Js.String2.length != 0) {"\n"} else {""})
                                ++ newVarsText
                }
                let st = recalcWrkCtxColors(st)
                ( st, newVarInts )
            }
            
        }
    }
}

let createNewDisj = (st:editorState, newDisj:disjMutable):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot create new disjoints without wrkCtx.`}))
        | Some(wrkCtx) => {
            newDisj->disjForEachArr(varInts => {
                wrkCtx->applySingleStmt(Disj({vars:wrkCtx->ctxIntsToSymsExn(varInts)}))
            })
            let newDisjTextLines = []
            newDisj->disjForEachArr(varInts => {
                newDisjTextLines->Js.Array2.push(wrkCtx->ctxIntsToSymsExn(varInts)->Js.Array2.joinWith(","))->ignore
            })
            if (newDisjTextLines->Js.Array2.length == 0) {
                st
            } else {
                let newDisjText = newDisjTextLines->Js.Array2.joinWith("\n")
                {
                    ...st,
                    disjText: st.disjText 
                                ++ (if (st.disjText->Js.String2.trim->Js.String2.length != 0) {"\n"} else {""})
                                ++ newDisjText
                }
            }
        }
    }
}

let stmtsHaveSameExpr = (
    ~ctx:mmContext, 
    ~stmt:userStmt, 
    ~stmtDto:stmtDto,
    ~dtoVarToCtxVar: Belt_MutableMapInt.t,
):bool => {
    stmtDto.expr
        ->Js_array2.map(i => dtoVarToCtxVar->Belt_MutableMapInt.getWithDefault(i,i))
        ->exprEq(stmt.cont->contToStr->ctxStrToIntsExn(ctx, _))
}

let insertExpr = (
    st:editorState, 
    ~exprText:string, 
    ~jstfText:string, 
    ~before:option<stmtId>,
    ~placeAtMaxIdxByDefault:bool,
):(editorState,option<string>) => {
    let maxIdx = switch before {
        | None => st.stmts->Js_array2.length
        | Some(stmtId) => {
            switch st.stmts->Js_array2.findIndex(stmt => stmt.id == stmtId) {
                | -1 => st.stmts->Js_array2.length
                | idx => idx
            }
        }
    }
    let newJstf:result<option<jstf>,_> = parseJstf(jstfText)
    let minIdx = switch newJstf {
        | Ok(Some({args})) => {
            let remainingLabels = Belt_HashSetString.fromArray(args)
            let minIdx = st.stmts->Js_array2.reducei(
                (minIdx,stmt,idx) => {
                    if (remainingLabels->Belt_HashSetString.isEmpty) {
                        minIdx
                    } else {
                        remainingLabels->Belt_HashSetString.remove(stmt.label)
                        idx + 1
                    }
                },
                0
            )
            if (maxIdx < minIdx) { maxIdx } else { minIdx }
        }
        | _ => 0
    }

    let jstfEqNewJstf = jstf => {
        switch newJstf {
            | Ok(Some(newJstf)) => jstfEq(newJstf, jstf)
            | _ => false
        }
    }

    let updateExistingStmt = (st,existingStmt,newJstf):(editorState,stmtId,string) => {
        let st = switch newJstf {
            | Ok(Some(newJstf)) => {
                st->updateStmt(existingStmt.id, stmt => {
                    {
                        ...stmt,
                        jstfText: jstfToStr(newJstf)
                    }
                })
            }
            | _ => st
        }
        (st, existingStmt.id, existingStmt.label)
    }

    let insertNewStmt = (st,existingStmt:option<userStmt>):(editorState,stmtId,string) => {
        let newIdx = switch existingStmt {
            | None => if placeAtMaxIdxByDefault {maxIdx} else {minIdx}
            | Some(existingStmt) => {
                let newIdx = st->getStmtIdx(existingStmt.id) + 1
                if (minIdx <= newIdx && newIdx <= maxIdx) { newIdx } else { maxIdx }
            }
        }
        let (st,newStmtId) = st->addNewStmtAtIdx(newIdx)
        let st = st->updateStmt(newStmtId, stmt => {
            {
                ...stmt,
                typ: P,
                cont: strToCont(exprText, ~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors, ()),
                contEditMode: false,
                jstfText,
            }
        })
        (st, newStmtId, (st->getStmtByIdExn(newStmtId)).label)
    }

    switch st.stmts->Js_array2.find(stmt => stmt.cont->contToStr == exprText) {
        | None => insertNewStmt(st,None)
        | Some(existingStmt) => {
            if (existingStmt.typ == E) {
                (st, existingStmt.id, existingStmt.label)
            } else {
                switch parseJstf(existingStmt.jstfText) {
                    | Ok(None) => updateExistingStmt(st,existingStmt,newJstf)
                    | Ok(Some(existingJstf)) => {
                        if (existingJstf->jstfEqNewJstf) {
                            (st, existingStmt.id, existingStmt.label)
                        } else {
                            insertNewStmt(st,Some(existingStmt))
                        }
                    }
                    | Error(_) => {
                        insertNewStmt(st,Some(existingStmt))
                    }
                }
            }
        }
    }
}

let addNewStatements = (st:editorState, newStmts:stmtsDto):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot add new statements without wrkCtx.`}))
        | Some(wrkCtx) => {
            let (st, newCtxVarInts) = createNewVars(st,newStmts.newVarTypes)
            let newStmtsVarToCtxVar = Belt_MutableMapInt.make()
            newStmts.newVars->Js.Array2.forEachi((newStmtsVarInt,i) => {
                newStmtsVarToCtxVar->Belt_MutableMapInt.set(newStmtsVarInt, newCtxVarInts[i])
            })
            let newCtxDisj = disjMutableMake()
            newStmts.newDisj->disjForEach((n,m) => {
                newCtxDisj->disjAddPair(
                    newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(n,n), 
                    newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(m,m), 
                )
            })
            let st = createNewDisj(st, newCtxDisj)
            let selectionWasEmpty = st.checkedStmtIds->Js.Array2.length == 0
            let st = if (!selectionWasEmpty || st.stmts->Js.Array2.length == 0) {
                st
            } else {
                st->toggleStmtChecked(st.stmts[0].id)
            }
            let checkedStmt = st->getTopmostSelectedStmt
            let newStmtsLabelToCtxLabel = Belt_MutableMapString.make()

            let getJstfTextFromStmtDto = (stmt:stmtDto, defaultJstfText:string):string => {
                switch stmt.jstf {
                    | None => defaultJstfText
                    | Some({args, label}) => {
                        jstfToStr({
                            args: args->Js_array2.map(argLabel => {
                                newStmtsLabelToCtxLabel->Belt_MutableMapString.getWithDefault(argLabel,argLabel)
                            }), 
                            label
                        })
                    }
                }
            }

            let stmtsHaveSameExpr = (userStmt, stmtDto) => {
                stmtsHaveSameExpr(
                    ~ctx = wrkCtx, 
                    ~stmt = userStmt, 
                    ~stmtDto,
                    ~dtoVarToCtxVar = newStmtsVarToCtxVar
                )
            }

            let placeAtMinIdxByDefault = newStmts.stmts->Js_array2.some(stmtDto => {
                st.stmts->Js_array2.some(rootStmt => stmtsHaveSameExpr(rootStmt, stmtDto))
            })
            let stMut = ref(st)
            newStmts.stmts->Js_array2.forEach(stmtDto => {
                let newJstfText = getJstfTextFromStmtDto(stmtDto, "")
                switch checkedStmt {
                    | Some(checkedStmt) if stmtsHaveSameExpr(checkedStmt, stmtDto) => {
                        stMut.contents = updateStmt(stMut.contents, checkedStmt.id, stmtToUpdate => {
                            {
                                ...stmtToUpdate,
                                jstfText: newJstfText
                            }
                        })
                        newStmtsLabelToCtxLabel->Belt_MutableMapString.set(stmtDto.label,checkedStmt.label)
                    }
                    | _ => {
                        let exprText = stmtDto.expr
                            ->Js_array2.map(i => newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(i,i))
                            ->ctxIntsToStrExn(wrkCtx, _)
                        let (st, _, ctxLabel) = insertProvable(stMut.contents,
                            ~exprText, 
                            ~jstfText = newJstfText, 
                            ~before = checkedStmt->Belt_Option.map(stmt => stmt.id),
                            ~placeAtMaxIdxByDefault = !placeAtMinIdxByDefault
                        )
                        stMut.contents = st
                        newStmtsLabelToCtxLabel->Belt_MutableMapString.set(stmtDto.label,ctxLabel)
                    }
                    
                }
            })
            let st = stMut.contents
            if (selectionWasEmpty) {
                st->uncheckAllStmts
            } else {
                st
            }
        }
    }
}

let verifyTypesForSubstitution = (~settings, ~ctx, ~frms, ~wrkSubs):unit => {
    let varToExprArr = wrkSubs.subs->Belt_MapInt.toArray
    let typesToProve = varToExprArr->Js_array2.map(((var,expr)) => 
        [ctx->getTypeOfVarExn(var)]->Js.Array2.concat(expr)
    )
    let proofTree = proveFloatings(
        ~ctx,
        ~frms,
        ~floatingsToProve=typesToProve,
        ~parenCnt=parenCntMake(prepareParenInts(ctx, settings.parens), ()),
        ()
    )
    varToExprArr->Js_array2.forEachi(((var,expr), i) =>
        if (wrkSubs.err->Belt_Option.isNone) {
            let typeExpr = typesToProve[i]
            if (proofTree->ptGetNode(typeExpr)->pnGetProof->Belt_Option.isNone) {
                wrkSubs.err = Some(TypeMismatch({ var, subsExpr:expr, typeExpr, }))
            }
        }
    )
}

let convertSubsToWrkSubs = (~subs, ~tmpFrame, ~ctx):wrkSubs => {
    let frameVarToCtxVar = frameVar => {
        switch tmpFrame.frameVarToSymb->Belt_Array.get(frameVar) {
            | None => raise(MmException({msg:`Cannot convert frameVar to ctxVar.`}))
            | Some(ctxSym) => ctx->ctxSymToIntExn(ctxSym)
        }
    }
    let res = Belt_Array.range(1,tmpFrame.numOfVars)
        ->Js.Array2.map(v => {
            (
                frameVarToCtxVar(v-1),
                applySubs(
                    ~frmExpr=[v-1],
                    ~subs,
                    ~createWorkVar = 
                        _ => raise(MmException({msg:`Work variables are not supported in convertSubsToWrkSubs().`}))
                )
            )
        })
        ->Belt_MutableMapInt.fromArray
    let maxVar = ctx->getNumOfVars-1
    for v in 0 to maxVar {
        if (!(res->Belt_MutableMapInt.has(v))) {
            res->Belt_MutableMapInt.set(v, [v])
        }
    }
    {
        subs: res->Belt_MutableMapInt.toArray->Belt_MapInt.fromArray,
        newDisj: disjMutableMake(),
        err: None,
    }
    
}

let verifyDisjoints = (~wrkSubs:wrkSubs, ~disj:disjMutable):unit => {
    let varToSubVars = Belt_MutableMapInt.make()

    let getSubVars = var => {
        switch varToSubVars->Belt_MutableMapInt.get(var) {
            | None => {
                varToSubVars->Belt_MutableMapInt.set(
                    var, 
                    switch wrkSubs.subs->Belt_MapInt.get(var) {
                        | None => []
                        | Some(expr) => expr->Js_array2.filter(s => s >= 0)
                    }
                )
                varToSubVars->Belt_MutableMapInt.getExn(var)
            }
            | Some(arr) => arr
        }
    }

    disj->disjForEach((n,m) => {
        if (wrkSubs.err->Belt_Option.isNone) {
            getSubVars(n)->Js_array2.forEach(nv => {
                if (wrkSubs.err->Belt_Option.isNone) {
                    getSubVars(m)->Js_array2.forEach(mv => {
                        if (wrkSubs.err->Belt_Option.isNone) {
                            if (nv == mv) {
                                wrkSubs.err = Some(CommonVar({
                                    var1:n,
                                    var2:m,
                                    commonVar:nv
                                }))
                            }
                            if (wrkSubs.err->Belt_Option.isNone && !(disj->disjContains(nv,mv))) {
                                wrkSubs.newDisj->disjAddPair(nv,mv)
                            }
                        }
                    })
                }
            })
        }
    })
}

let findPossibleSubs = (st, frmExpr, expr):array<wrkSubs> => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot search for substitutions without wrkCtx.`}))
        | Some(wrkCtx) => {
            let axLabel = generateNewLabels(~ctx=wrkCtx, ~prefix="temp-ax-", ~amount=1, ())[0]
            let tmpFrame = createFrame(
                ~ctx=wrkCtx, ~label=axLabel, ~exprStr=wrkCtx->ctxIntsToSymsExn(frmExpr), ~proof=None,
                ~skipHyps=true, ~skipFirstSymCheck=true, ()
            )
            let frm = prepareFrmSubsDataForFrame(tmpFrame)
            let disj = wrkCtx->getAllDisj
            let foundSubs = []
            iterateSubstitutions(
                ~frmExpr=tmpFrame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                ~constParts = frm.constParts[frm.numOfHypsE], 
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt=parenCntMake(prepareParenInts(wrkCtx, st.settings.parens), ()),
                ~consumer = subs => {
                    let wrkSubs = convertSubsToWrkSubs(~subs, ~tmpFrame, ~ctx=wrkCtx)
                    verifyDisjoints(~wrkSubs, ~disj)
                    if (wrkSubs.err->Belt_Option.isNone) {
                        verifyTypesForSubstitution(~settings=st.settings, ~ctx=wrkCtx, ~frms=st.frms, ~wrkSubs)
                    }
                    foundSubs->Js_array2.push(wrkSubs)->ignore
                    Continue
                }
            )->ignore
            foundSubs
        }
    }
}

let applyWrkSubs = (expr, wrkSubs): expr => {
    let resultSize = ref(0)
    expr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            switch wrkSubs.subs->Belt_MapInt.get(s) {
                | None => raise(MmException({msg:`Cannot find a substitution for ${s->Belt_Int.toString} in applyWrkSubs.`}))
                | Some(expr) => resultSize.contents = resultSize.contents + expr->Js_array2.length
            }
        }
    })
    let res = Expln_utils_common.createArray(resultSize.contents)
    let e = ref(0)
    let r = ref(0)
    while (r.contents < resultSize.contents) {
        let s = expr[e.contents]
        if (s < 0) {
            res[r.contents] = s
            r.contents = r.contents + 1
        } else {
            let subExpr = wrkSubs.subs->Belt_MapInt.getExn(s)
            let len = subExpr->Js_array2.length
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=0, ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let applySubstitutionForStmt = (st:editorState, ctx:mmContext, stmt:userStmt, wrkSubs:wrkSubs):userStmt => {
    let expr = ctx->ctxSymsToIntsExn(stmt.cont->contToArrStr)
    let newExpr = applyWrkSubs(expr, wrkSubs)
    {
        ...stmt,
        cont: ctx->ctxIntsToStrExn(newExpr)->strToCont(~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors, ())
    }
}

let applySubstitutionForEditor = (st, wrkSubs:wrkSubs):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot apply substitution without wrkCtx.`}))
        | Some(wrkCtx) => {
            let st = createNewDisj(st, wrkSubs.newDisj)
            {
                ...st,
                stmts: st.stmts->Js_array2.map(stmt => applySubstitutionForStmt(st, wrkCtx,stmt,wrkSubs))
            }
        }
    }
}

let removeUnusedVars = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot remove unused variables without wrkCtx.`}))
        | Some(wrkCtx) => {
            let usedSymbols = st.stmts->Expln_utils_common.arrFlatMap(stmt=>stmt.cont->contToArrStr)->Belt_SetString.fromArray
            let unusedVars = wrkCtx->getLocalVars->Js_array2.filter(var => !(usedSymbols->Belt_SetString.has(var)))
            if (unusedVars->Js_array2.length == 0) {
                st
            } else {
                let unusedVarInts = wrkCtx->ctxSymsToIntsExn(unusedVars)
                let usedVarsStr = wrkCtx->getLocalHyps
                    ->Js_array2.filter(hyp => hyp.typ == F && !(unusedVarInts->Js_array2.includes(hyp.expr[1])))
                    ->Js_array2.map(hyp => `${hyp.label} ${wrkCtx->ctxIntToSymExn(hyp.expr[0])} ${wrkCtx->ctxIntToSymExn(hyp.expr[1])}`)
                    ->Js_array2.joinWith("\n")
                let st = completeVarsEditMode(st, usedVarsStr)
                let newDisj = disjMutableMake()
                wrkCtx->getAllDisj->disjForEach((n,m) => {
                    if (!(unusedVarInts->Js_array2.includes(n)) && !(unusedVarInts->Js_array2.includes(m))) {
                        newDisj->disjAddPair(n,m)
                    }
                })
                let newDisjStrArr = []
                newDisj->disjForEachArr(varInts => {
                    newDisjStrArr->Js.Array2.push(wrkCtx->ctxIntsToSymsExn(varInts)->Js_array2.joinWith(","))->ignore
                })
                let st = completeDisjEditMode(st, newDisjStrArr->Js.Array2.joinWith("\n"))
                prepareEditorForUnification(st)
            }
        }
    }
}

let exprSrcToJstf = (wrkCtx, proofTree:proofTreeDto, exprSrc:exprSrcDto, exprToUserStmt):option<jstf> => {
    switch exprSrc {
        | Assertion({args, label:asrtLabel}) => {
            switch wrkCtx->getFrame(asrtLabel) {
                | None => None
                | Some(frame) => {
                    let argLabels = []
                    let argLabelsValid = ref(true)
                    frame.hyps->Js_array2.forEachi((hyp,i) => {
                        if (hyp.typ == E) {
                            switch args->Belt_Array.get(i) {
                                | None => argLabelsValid.contents = false
                                | Some(argIdx) => {
                                    switch exprToUserStmt->Belt_Map.get(proofTree.nodes[argIdx].expr) {
                                        | None => argLabelsValid.contents = false
                                        | Some(userStmt) => argLabels->Js_array2.push(userStmt.label)->ignore
                                    }
                                }
                            }
                        }
                    })
                    if (argLabelsValid.contents) {
                        Some({
                            args: argLabels,
                            label: asrtLabel
                        })
                    } else {
                        None
                    }
                }
            }
        }
        | _ => None
    }
}

let userStmtSetJstfTextAndProof = (stmt, wrkCtx, proofTree:proofTreeDto, proofNode:proofNodeDto, exprToUserStmt):userStmt => {
    switch proofNode.proof {
        | Some(proofSrc) => {
            switch exprSrcToJstf(wrkCtx, proofTree, proofSrc, exprToUserStmt) {
                | None => stmt
                | Some(jstfFromProof) => {
                    switch stmt.jstf {
                        | None => {
                            {
                                ...stmt,
                                jstfText: jstfToStr(jstfFromProof),
                                proof: Some((proofTree, proofNode)),
                            }
                        }
                        | Some(existingJstf) => {
                            if (jstfFromProof == existingJstf) {
                                {
                                    ...stmt,
                                    proof: Some((proofTree, proofNode)),
                                }
                            } else {
                                stmt
                            }
                        }
                    }
                }
            }
        }
        | None => stmt
    }
    
}

let userStmtSetProofStatus = (stmt, wrkCtx, proofTree:proofTreeDto, proofNode:proofNodeDto, exprToUserStmt):userStmt => {
    let parentEqJstf = (parentSrc, jstf) => {
        switch exprSrcToJstf(wrkCtx, proofTree, parentSrc, exprToUserStmt) {
            | None => false
            | Some(parentJstf) => parentJstf == jstf
        }
    }

    switch stmt.proof {
        | Some(_) => {...stmt, proofStatus:Some(Ready)}
        | None => {
            switch stmt.jstf {
                | None => {...stmt, proofStatus:Some(NoJstf)}
                | Some(jstf) => {
                    switch proofNode.parents->Js.Array2.find(parentEqJstf(_, jstf)) {
                        | Some(_) => {...stmt, proofStatus:Some(Waiting)}
                        | None => {...stmt, proofStatus:Some(JstfIsIncorrect)}
                    }
                }
            }
        }
    }
}

let applyUnifyAllResults = (st,proofTreeDto) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot applyUnifyAllResults without wrkCtx.`}))
        | Some(wrkCtx) => {
            let nodes = proofTreeDto.nodes
                ->Js_array2.map(node => (node.expr,node))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let exprToUserStmt = st.stmts
                                    ->Js_array2.filter(stmt => stmt.expr->Belt_Option.isSome)
                                    ->Js_array2.map(stmt => (stmt.expr->Belt_Option.getExn, stmt))
                                    ->Belt_Map.fromArray(~id=module(ExprCmp))
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    let stmt = {...stmt, proof:None, proofStatus: None}
                    if (stmt.typ == P) {
                        st->updateStmt(stmt.id, stmt => {
                            switch stmt.expr {
                                | None => stmt
                                | Some(expr) => {
                                    switch nodes->Belt_HashMap.get(expr) {
                                        | None => stmt
                                        | Some(node) => {
                                            let stmt = userStmtSetJstfTextAndProof(stmt,wrkCtx,proofTreeDto,node,exprToUserStmt)
                                            let stmt = userStmtSetProofStatus(stmt,wrkCtx,proofTreeDto,node,exprToUserStmt)
                                            stmt
                                        }
                                    }
                                }
                            }
                        })
                    } else {
                        st
                    }
                },
                st
            )
        }
    }
}

let updateEditorStateWithPostupdateActions = (st, update:editorState=>editorState) => {
    let st = update(st)
    let st = prepareEditorForUnification(st)
    if (st.wrkCtx->Belt_Option.isSome) {
        removeUnusedVars(st)
    } else {
        st
    }
}

let splitIntoChunks = (str, chunkMaxSize): array<string> => {
    let len = str->Js_string2.length
    if (len <= chunkMaxSize) {
        [str]
    } else {
        let res = []
        let numberOfChunks = Js.Math.ceil_int(len->Belt_Int.toFloat /. chunkMaxSize->Belt_Int.toFloat)
        for i in 1 to numberOfChunks {
            let begin = (i-1)*chunkMaxSize
            res->Js_array2.push(str->Js_string2.substrAtMost(~from=begin, ~length=chunkMaxSize))->ignore
        }
        res
    }
}

let proofToText = (ctx:mmContext,stmt:userStmt,proof:proof):string => {
    switch proof {
        | Compressed({labels, compressedProofBlock}) => {
            let blk = splitIntoChunks(compressedProofBlock, 50)->Js_array2.joinWith(" ")
            let asrt = `${stmt.label} $p ${stmt.cont->contToStr} $= ( ${labels->Js_array2.joinWith(" ")} ) ${blk} $.`
            let localVars = ctx->getLocalVars
            let localDisj = ctx->getLocalDisj
            let localHyps = ctx->getLocalHyps
            let blockIsRequired = localHyps->Js.Array2.length > 0 || !(localDisj->disjIsEmpty)
            let result = []
            if (blockIsRequired) {
                result->Js.Array2.push("${")->ignore
            }
            if (localVars->Js.Array2.length > 0) {
                result->Js.Array2.push("$v " ++ localVars->Js.Array2.joinWith(" ") ++ " $.")->ignore
            }
            localDisj->disjForEachArr(vars => {
                result->Js.Array2.push("$d " ++ ctx->ctxIntsToStrExn(vars) ++ " $.")->ignore
            })
            localHyps->Js.Array2.forEach(hyp => {
                let hypTypStr = if (hyp.typ == F) {
                    " $f "
                } else {
                    " $e "
                }
                result->Js.Array2.push(hyp.label ++ hypTypStr ++ ctx->ctxIntsToStrExn(hyp.expr) ++ " $.")->ignore
            })
            result->Js.Array2.push(asrt)->ignore
            if (blockIsRequired) {
                result->Js.Array2.push("$}")->ignore
            }
            result->Js.Array2.joinWith("\r\n")
        }
        | _ => "Error: only compressed proofs are supported."
    }
}

let generateCompressedProof = (st, stmtId):option<string> => {
    switch st.wrkCtx {
        | None => None
        | Some(wrkCtx) => {
            switch st.stmts->Js.Array2.find(stmt => stmt.id == stmtId) {
                | None => None
                | Some(stmt) => {
                    switch stmt.proof {
                        | None => None
                        | Some((proofTable,proofNode)) => {
                            let proofTable = createProofTable(proofTable,proofNode)
                            let proof = MM_proof_table.createProof(wrkCtx, proofTable, proofTable->Js_array2.length-1)
                            Some(proofToText(wrkCtx,stmt,proof))
                        }
                    }
                }
            }
        }
    }
}

let replaceRef = (st,~replaceWhat,~replaceWith):result<editorState,string> => {
    st.stmts->Js_array2.reduce(
        (res,stmt) => {
            switch res {
                | Error(_) => res
                | Ok(st) => {
                    switch parseJstf(stmt.jstfText) {
                        | Error(_) => Error(`Cannot parse justification '${stmt.jstfText}' for ${stmt.label}`)
                        | Ok(None) => Ok(st)
                        | Ok(Some(jstf)) => {
                            if (jstf.args->Js.Array2.includes(replaceWhat)) {
                                let newJstf = {
                                    ...jstf,
                                    args: jstf.args
                                        ->Js_array2.map(ref => if (ref == replaceWhat) {replaceWith} else {ref})
                                }
                                Ok(
                                    st->updateStmt(stmt.id, stmt => {
                                        {
                                            ...stmt,
                                            jstfText: jstfToStr(newJstf)
                                        }
                                    })
                                )
                            } else {
                                Ok(st)
                            }
                        }
                    }
                }
            }
        },
        Ok(st)
    )
}

let mergeStmts = (st:editorState,id1:string,id2:string):result<editorState,string> => {
    switch st->editorGetStmtById(id1) {
        | None => Error(`Cannot find a statement with id = '${id1}'`)
        | Some(stmt1) => {
            switch st->editorGetStmtById(id2) {
                | None => Error(`Cannot find a statement with id = '${id2}'`)
                | Some(stmt2) => {
                    if (stmt1.cont->contToStr != stmt2.cont->contToStr) {
                        Error(`Statements to merge must have identical expressions.`)
                    } else {
                        switch replaceRef(st, ~replaceWhat=stmt2.label, ~replaceWith=stmt1.label) {
                            | Error(msg) => Error(msg)
                            | Ok(st) => {
                                let st = st->deleteStmt(id2)
                                Ok(st)
                            }
                        }
                    }
                }
            }
        }
    }
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->Js_string2.replaceByRe(symbolsNotAllowedInLabelRegex, "")

let renameStmt = (st:editorState, stmtId:string, newLabel:string):result<editorState,string> => {
    let newLabel = newLabel->removeSymbolsNotAllowedInLabel
    if (newLabel == "") {
        Error(`label must not be empty.`)
    } else {
        switch st.stmts->Js_array2.find(stmt => stmt.id != stmtId && stmt.label == newLabel) {
            | Some(_) => Error(`label '${newLabel}' is used by another statement.`)
            | None => {
                switch st->editorGetStmtById(stmtId) {
                    | None => Ok(st)
                    | Some(stmt) => {
                        if (stmt.label == newLabel) {
                            Ok(st)
                        } else {
                            switch replaceRef(st, ~replaceWhat=stmt.label, ~replaceWith=newLabel) {
                                | Error(msg) => Error(msg)
                                | Ok(st) => {
                                    Ok(st->updateStmt(stmtId, stmt => {...stmt, label:newLabel}))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let findStmtsToMerge = (st:editorState):result<(userStmt,userStmt),string> => {
    if (st.checkedStmtIds->Js.Array2.length == 1) {
        switch st->editorGetStmtById(st.checkedStmtIds[0]) {
            | None => Error("One statement should be selected.")
            | Some(stmt1) => {
                let contStr = stmt1.cont->contToStr
                switch st.stmts->Js.Array2.find(stmt => stmt.id != stmt1.id && stmt.cont->contToStr == contStr) {
                    | None => Error("Cannot find another statement to merge with.")
                    | Some(stmt2) => {
                        if (stmt1.cont->contToStr != stmt2.cont->contToStr) {
                            Error("Statements to merge must have identical expressions.")
                        } else {
                            Ok((stmt1, stmt2))
                        }
                    }
                }
            }
        }
    } else {
        Error("One statement should be selected.")
    }
}