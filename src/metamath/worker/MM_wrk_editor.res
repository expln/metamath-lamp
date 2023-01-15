open MM_context
open MM_parser
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings
open MM_asrt_apply
open MM_parenCounter
open MM_substitution
open MM_wrk_ctx
open MM_provers

type stmtCont =
    | Text(array<string>)
    | Tree(syntaxTreeNode)

let contIsEmpty = cont => {
    switch cont {
        | Text(arr) => arr->Js_array2.length == 0
        | Tree(syntaxTreeNode) => syntaxTreeIsEmpty(syntaxTreeNode)
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text(arr) => arr
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let contToStr = cont => {
    cont->contToArrStr->Js_array2.joinWith(" ")
}

let strToCont = str => {
    Text(getSpaceSeparatedValuesAsArray(str))
}

type userStmtType = [ #e | #p ]

let userStmtTypeFromStr = str => {
    switch str {
        | "e" => #e
        | "p" => #p
        | _ => raise(MmException({msg:`Cannot convert '${str}' to userStmtType`}))
    }
}

type proofStatus = [ #ready | #waiting | #noJstf | #jstfIsIncorrect ]

type userStmt = {
    id: string,

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
    jstf: option<justification>,
    proof: option<proofNode>,
    proofStatus: option<proofStatus>,
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

type editorState = {
    settingsV:int,
    settings:settings,

    preCtxV: int,
    preCtx: mmContext,
    frms: Belt_MapString.t<frmSubsData>,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkCtx: option<mmContext>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

type wrkSubs = Belt_MapInt.t<expr>

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
    let newStmts = st.stmts->Js_array2.filter(stmt => !isStmtChecked(st,stmt.id))
    let newNextStmtId = if (newStmts->Js_array2.length == 0) { 0 } else { st.nextStmtId }
    {
        ...st,
        stmts: newStmts,
        checkedStmtIds: [],
        nextStmtId: newNextStmtId,
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

let addNewStmt = (st:editorState):(editorState,string) => {
    let newId = st.nextStmtId->Belt_Int.toString
    let newLabel = createNewLabel(st, "stmt")
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
                                [createEmptyUserStmt(newId,#p,newLabel), stmt]
                            } else {
                                [stmt]
                            }
                        })->Belt_Array.concatMany
                    }
                    | None => st.stmts->Js_array2.concat([createEmptyUserStmt(newId, #p, newLabel)])
                }
        },
        newId
    )
}

let isSingleStmtChecked = st => st.checkedStmtIds->Js_array2.length == 1

let duplicateCheckedStmt = st => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let newLabel = createNewLabel(st, "stmt")
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

let completeVarsEditMode = (st, newVarsText) => {
    {
        ...st,
        varsText:newVarsText,
        varsEditMode: false
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

let completeLabelEditMode = (st, stmtId, newLabel) => {
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

let completeContEditMode = (st, stmtId, newCont) => {
    updateStmt(st, stmtId, stmt => {
        if (contIsEmpty(newCont)) {
            stmt
        } else {
            {
                ...stmt,
                cont:newCont,
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

let setSettings = (st, settingsV, settings) => {
    { ...st, settingsV, settings }
}

let setPreCtx = (st, preCtxV, preCtx) => {
    { ...st, preCtxV, preCtx, frms: prepareFrmSubsData(preCtx) }
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
            | #e => 1
            | #p => 2
        }
    }
    st->stableSortStmts((a,b) => stmtToInt(a) - stmtToInt(b))
}

let removeAllErrorsInUserStmt = stmt => {
    {
        ...stmt,
        stmtErr: None,
    }
}

let removeAllErrorsInEditorState = st => {
    {
        ...st,
        varsErr: None,
        disjErr: None,
        stmts: st.stmts->Js_array2.map(removeAllErrorsInUserStmt)
    }
}

let removeAllProofData = st => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => {...stmt, expr: None, jstf: None, proof: None, proofStatus: None})
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
            ->Js_array2.filter( stmt => stmt.typ == #e)
            ->Js_array2.map( stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
    )
    let st = switch wrkCtxRes {
        | Error(wrkCtxErr) => parseWrkCtxErr(st, wrkCtxErr)
        | Ok(wrkCtx) => {
            let st = {...st, wrkCtx:Some(wrkCtx)}
            let st = st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (stmt.typ == #e) {
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

let parseJstf = jstfText => {
    let jstfTrim = jstfText->Js_string2.trim
    if (jstfTrim->Js_string2.length == 0) {
        None
    } else {
        let argsAndAsrt = jstfTrim->Js_string2.split(":")
        if (argsAndAsrt->Js_array2.length != 2) {
            raise(MmException({msg:`Cannot parse justification: '${jstfText}' [1].`}))
        }
        Some({
            args: argsAndAsrt[0]->getSpaceSeparatedValuesAsArray,
            asrt: argsAndAsrt[1]->Js_string2.trim
        })
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
                jstf: parseJstf(stmt.jstfText)
            }
        } catch {
            | MmException({msg}) => {...stmt, stmtErr:Some(msg)}
        }
    }
}

let isLabelDefined = (label:string, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t) => {
    usedLabels->Belt_MutableSetString.has(label) || wrkCtx->isHyp(label) || wrkCtx->isAsrt(label)
}

let validateJstfRefs = (stmt:userStmt, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch stmt.jstf {
            | None => stmt
            | Some({args,asrt}) => {
                switch args->Js_array2.find(ref => !isLabelDefined(ref,wrkCtx,usedLabels)) {
                    | Some(ref) => {
                        {...stmt, stmtErr:Some(`The reference '${ref}' is not defined.`)}
                    }
                    | None => {
                        if (!(wrkCtx->isAsrt(asrt))) {
                            {...stmt, stmtErr:Some(`The label '${asrt}' doesn't refer to any assertion.`)}
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

let checkAllStmtsAreUnique = (st:editorState):editorState => {
    let declaredStmts = Belt_MutableMap.make(~id=module(ExprCmp))
    st.stmts->Js_array2.reduce(
        (st,stmt) => {
            if (editorStateHasErrors(st)) {
                st
            } else {
                let stmt = switch stmt.expr {
                    | None => raise(MmException({msg:`Cannot checkAllStmtsAreUnique without expr.`}))
                    | Some(expr) => {
                        switch declaredStmts->Belt_MutableMap.get(expr) {
                            | Some(prevStmtLabel) => {
                                {...stmt, stmtErr:Some(`This statement is the same as the previous one - '${prevStmtLabel}'`)}
                            }
                            | None => {
                                declaredStmts->Belt_MutableMap.set(expr, stmt.label)
                                stmt
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

let prepareProvablesForUnification = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot prepareProvablesForUnification without wrkCtx.`}))
        | Some(wrkCtx) => {
            let usedLabels = Belt_MutableSetString.make()
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (editorStateHasErrors(st) || stmt.typ != #p) {
                        st
                    } else {
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
    let st = removeAllErrorsInEditorState(st)
    [
        removeAllProofData,
        refreshWrkCtx,
        prepareProvablesForUnification,
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

let createNewVars = (st:editorState, varTypes:array<int>):(editorState,array<int>) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot create new variables without wrkCtx.`}))
        | Some(wrkCtx) => {
            let numOfVars = varTypes->Js_array2.length
            if (numOfVars == 0) {
                (st,[])
            } else {
                let newVarNames = wrkCtx->generateNewVarNames(varTypes)
                let newHypLabels = wrkCtx->generateNewLabels(~prefix="var", ~amount=numOfVars)
                wrkCtx->applySingleStmt(Var({symbols:newVarNames}))
                let varTypeNames = wrkCtx->ctxIntsToSymsExn(varTypes)
                newHypLabels->Js.Array2.forEachi((label,i) => {
                    wrkCtx->applySingleStmt(Floating({label, expr:[varTypeNames[i], newVarNames[i]]}))
                })
                let newVarInts = wrkCtx->ctxSymsToIntsExn(newVarNames)
                let newVarsText = newHypLabels->Js.Array2.mapi((label,i) => {
                    `${label} ${varTypeNames[i]} ${newVarNames[i]}`
                })->Js_array2.joinWith("\n")
                (
                    {
                        ...st,
                        varsText: st.varsText ++ (if (st.varsText->Js.String2.length != 0) {"\n"} else {""}) ++ newVarsText
                    },
                    newVarInts
                )
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
                newDisjTextLines->Js.Array2.push(wrkCtx->ctxIntsToSymsExn(varInts))->ignore
            })
            if (newDisjTextLines->Js.Array2.length == 0) {
                st
            } else {
                let newDisjText = newDisjTextLines->Js.Array2.joinWith("\n")
                {
                    ...st,
                    disjText: st.disjText ++ "\n" ++ newDisjText
                }
            }
        }
    }
}

let addAsrtSearchResult = (st:editorState, applRes:applyAssertionResult):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot add assertion search result without wrkCtx.`}))
        | Some(wrkCtx) => {
            switch st.frms->Belt_MapString.get(applRes.asrtLabel) {
                | None => raise(MmException({msg:`Cannot find assertion with label '${applRes.asrtLabel}'.`}))
                | Some(frm) => {
                    let (st, newCtxVarInts) = createNewVars(st,applRes.newVarTypes)
                    let applResVarToCtxVar = Belt_MutableMapInt.make()
                    applRes.newVars->Js.Array2.forEachi((applResVarInt,i) => {
                        applResVarToCtxVar->Belt_MutableMapInt.set(applResVarInt, newCtxVarInts[i])
                    })
                    let newCtxDisj = disjMutableMake()
                    applRes.newDisj->disjForEach((n,m) => {
                        let newN = switch applResVarToCtxVar->Belt_MutableMapInt.get(n) {
                            | None => n
                            | Some(newN) => newN
                        }
                        let newM = switch applResVarToCtxVar->Belt_MutableMapInt.get(m) {
                            | None => m
                            | Some(newM) => newM
                        }
                        newCtxDisj->disjAddPair(newN, newM)
                    })
                    let st = createNewDisj(st, newCtxDisj)
                    let selectionWasEmpty = st.checkedStmtIds->Js.Array2.length == 0
                    let st = if (!selectionWasEmpty || st.stmts->Js.Array2.length == 0) {
                        st
                    } else {
                        st->toggleStmtChecked(st.stmts[0].id)
                    }
                    let mainStmtLabel = createNewLabel(st, "stmt")
                    let argLabels = []
                    let stMut = ref(st)
                    frm.frame.hyps->Js_array2.forEach(hyp => {
                        if (hyp.typ == E) {
                            let argLabel = createNewLabel(stMut.contents, mainStmtLabel ++ "-" ++ hyp.label)
                            argLabels->Js.Array2.push(argLabel)->ignore
                            let argExprText = applySubs(
                                ~frmExpr=hyp.expr,
                                ~subs=applRes.subs,
                                ~createWorkVar=_=>raise(MmException({msg:`Cannot create a work variable in addAsrtSearchResult [1].`}))
                            )
                                ->Js.Array2.map(appResInt => {
                                    switch applResVarToCtxVar->Belt_MutableMapInt.get(appResInt) {
                                        | None => appResInt
                                        | Some(ctxVar) => ctxVar
                                    }
                                })
                                ->ctxIntsToStrExn(wrkCtx, _)
                            let (st, newStmtId) = addNewStmt(stMut.contents)
                            stMut.contents = st
                            stMut.contents = updateStmt(stMut.contents, newStmtId, stmt => {
                                {
                                    ...stmt,
                                    typ: #p,
                                    label: argLabel,
                                    cont: strToCont(argExprText),
                                    contEditMode: false,
                                }
                            })
                        }
                    })
                    let st = stMut.contents
                    let asrtExprText = applySubs(
                        ~frmExpr=frm.frame.asrt,
                        ~subs=applRes.subs,
                        ~createWorkVar=_=>raise(MmException({msg:`Cannot create a work variable in addAsrtSearchResult [2].`}))
                    )
                        ->Js.Array2.map(appResInt => {
                            switch applResVarToCtxVar->Belt_MutableMapInt.get(appResInt) {
                                | None => appResInt
                                | Some(ctxVar) => ctxVar
                            }
                        })
                        ->ctxIntsToStrExn(wrkCtx, _)
                    let (st, newStmtId) = addNewStmt(st)
                    let jstfText = argLabels->Js.Array2.joinWith(" ") ++ ": " ++ applRes.asrtLabel
                    let st = updateStmt(st, newStmtId, stmt => {
                        {
                            ...stmt,
                            typ: #p,
                            label: mainStmtLabel,
                            cont: strToCont(asrtExprText),
                            contEditMode: false,
                            jstfText,
                        }
                    })
                    if (selectionWasEmpty) {
                        st->uncheckAllStmts
                    } else {
                        st
                    }
                }
            }
        }
    }
}

let verifyTypesForSubstitution = (~settings, ~ctx, ~frms, ~wrkSubs):bool => {
    let typesToProve = wrkSubs
        ->Belt_MapInt.toArray
        ->Js_array2.map(((var,expr)) => [ctx->getTypeOfVarExn(var)]->Js.Array2.concat(expr))
    let proofTree = proveFloatings(
        ~ctx,
        ~frms,
        ~stmts=typesToProve,
        ~parenCnt=parenCntMake(prepareParenInts(ctx, settings.parens)),
        ()
    )
    typesToProve->Js_array2.every(typeExpr => {
        switch proofTree->ptGetNodeByExpr(typeExpr) {
            | None => raise(MmException({msg:`Unexpected condition met: the proofTree was expected to contain nodes for each typeExpr.`}))
            | Some(node) => node->pnGetProof->Belt_Option.isSome
        }
    })
}

let convertSubsToWrkSubs = (subs, frame, ctx):wrkSubs => {
    let frameVarToCtxVar = frameVar => {
        switch frame.frameVarToSymb->Belt_MapInt.get(frameVar) {
            | None => raise(MmException({msg:`Cannot convert frameVar to ctxVar.`}))
            | Some(ctxSym) => ctx->ctxSymToIntExn(ctxSym)
        }
    }
    let res = Belt_Array.range(1,frame.numOfVars)
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
    res->Belt_MutableMapInt.toArray->Belt_MapInt.fromArray
}

let verifyDisjoints = (~wrkSubs:wrkSubs, ~disj:disjMutable) => {
    let varToSubVars = Belt_MutableMapInt.make()

    let getSubVars = var => {
        switch varToSubVars->Belt_MutableMapInt.get(var) {
            | None => {
                varToSubVars->Belt_MutableMapInt.set(
                    var, 
                    switch wrkSubs->Belt_MapInt.get(var) {
                        | None => []
                        | Some(expr) => expr->Js_array2.filter(s => s >= 0)
                    }
                )
                varToSubVars->Belt_MutableMapInt.getExn(var)
            }
            | Some(arr) => arr
        }
    }

    let res = ref(true)
    disj->disjForEach((n,m) => {
        if (res.contents) {
            getSubVars(n)->Js_array2.forEach(nv => {
                if (res.contents) {
                    getSubVars(m)->Js_array2.forEach(mv => {
                        if (res.contents) {
                            res.contents = nv != mv && disj->disjContains(nv,mv)
                        }
                    })
                }
            })
        }
    })
    res.contents
}

let findPossibleSubs = (st, frmExpr, expr):array<wrkSubs> => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot search for substitutions without wrkCtx.`}))
        | Some(wrkCtx) => {
            let axLabel = (wrkCtx->generateNewLabels(~prefix="temp-ax-", ~amount=1))[0]
            let frame = wrkCtx->createFrame(axLabel, wrkCtx->ctxIntsToSymsExn(frmExpr), ~skipHyps=true, ~skipFirstSymCheck=true, ())
            let frm = prepareFrmSubsDataForFrame(frame)
            let disj = wrkCtx->getAllDisj
            let foundSubs = []
            iterateSubstitutions(
                ~frmExpr=frame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                ~constParts = frm.constParts[frm.numOfHypsE], 
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt=parenCntMake(prepareParenInts(wrkCtx, st.settings.parens)),
                ~consumer = subs => {
                    let wrkSubs = convertSubsToWrkSubs(subs, frame, wrkCtx)
                    if (verifyDisjoints(~wrkSubs, ~disj) 
                        && verifyTypesForSubstitution(~settings=st.settings, ~ctx=wrkCtx, ~frms=st.frms, ~wrkSubs)
                    ) {
                        foundSubs->Js_array2.push(wrkSubs)->ignore
                    }
                    Continue
                }
            )->ignore
            foundSubs
        }
    }
}

let applyWrkSubs = (expr, subs): expr => {
    let resultSize = ref(0)
    expr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            switch subs->Belt_MapInt.get(s) {
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
            let subExpr = subs->Belt_MapInt.getExn(s)
            let len = subExpr->Js_array2.length
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=0, ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let applySubstitutionForStmt = (ctx:mmContext, stmt:userStmt, subs:wrkSubs):userStmt => {
    let expr = ctx->ctxSymsToIntsExn(stmt.cont->contToArrStr)
    let newExpr = applyWrkSubs(expr, subs)
    {
        ...stmt,
        cont: Text(ctx->ctxIntsToSymsExn(newExpr))
    }
}

let applySubstitutionForEditor = (st, subs:wrkSubs):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot apply substitution without wrkCtx.`}))
        | Some(wrkCtx) => {
            {
                ...st,
                stmts: st.stmts->Js_array2.map(stmt => applySubstitutionForStmt(wrkCtx,stmt,subs))
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

let exprSrcToJstf = (wrkCtx, exprSrc:exprSource, exprToUserStmt):option<justification> => {
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
                                | Some(argNode) => {
                                    switch exprToUserStmt->Belt_Map.get(argNode->pnGetExpr) {
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
                            asrt: asrtLabel
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

let userStmtSetJstfTextAndProof = (stmt,wrkCtx,proofNode:proofNode,exprToUserStmt):userStmt => {
    switch proofNode->pnGetProof {
        | Some(proofSrc) => {
            switch exprSrcToJstf(wrkCtx,proofSrc,exprToUserStmt) {
                | None => stmt
                | Some(jstfFromProof) => {
                    switch stmt.jstf {
                        | None => {
                            {
                                ...stmt,
                                jstfText: jstfFromProof.args->Js_array2.joinWith(" ") ++ " : " ++ jstfFromProof.asrt,
                                proof: Some(proofNode),
                            }
                        }
                        | Some(existingJstf) => {
                            if (jstfFromProof == existingJstf) {
                                {
                                    ...stmt,
                                    proof: Some(proofNode)
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

let userStmtSetProofStatus = (stmt, wrkCtx, proofNode:proofNode, exprToUserStmt):userStmt => {
    let parentEqJstf = (parentSrc, jstf) => {
        switch exprSrcToJstf(wrkCtx, parentSrc, exprToUserStmt) {
            | None => false
            | Some(parentJstf) => parentJstf == jstf
        }
    }

    switch stmt.proof {
        | Some(_) => {...stmt, proofStatus:Some(#ready)}
        | None => {
            switch stmt.jstf {
                | None => {...stmt, proofStatus:Some(#noJstf)}
                | Some(jstf) => {
                    switch proofNode->pnGetParents {
                        | None => {...stmt, proofStatus:Some(#jstfIsIncorrect)}
                        | Some(parents) => {
                            switch parents->Js.Array2.find(parentEqJstf(_, jstf)) {
                                | Some(_) => {...stmt, proofStatus:Some(#waiting)}
                                | None => {...stmt, proofStatus:Some(#jstfIsIncorrect)}
                            }
                        }
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
                ->Js_array2.map(node => (node->pnGetExpr,node))
                ->Belt_MutableMap.fromArray(~id=module(ExprCmp))
            let exprToUserStmt = st.stmts
                                    ->Js_array2.filter(stmt => stmt.expr->Belt_Option.isSome)
                                    ->Js_array2.map(stmt => (stmt.expr->Belt_Option.getExn, stmt))
                                    ->Belt_Map.fromArray(~id=module(ExprCmp))
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    let stmt = {...stmt, proof:None, proofStatus: None}
                    if (stmt.typ == #p) {
                        st->updateStmt(stmt.id, stmt => {
                            switch stmt.expr {
                                | None => stmt
                                | Some(expr) => {
                                    switch nodes->Belt_MutableMap.get(expr) {
                                        | None => stmt
                                        | Some(node) => {
                                            let stmt = userStmtSetJstfTextAndProof(stmt,wrkCtx,node,exprToUserStmt)
                                            let stmt = userStmtSetProofStatus(stmt,wrkCtx,node,exprToUserStmt)
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