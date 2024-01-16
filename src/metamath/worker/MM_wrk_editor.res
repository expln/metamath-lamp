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
open Common
open MM_unification_debug
open MM_wrk_pre_ctx_data

type mmFileSourceType = Local | Web

type readInstr = ReadAll | StopBefore | StopAfter

type webSource = {
    alias:string,
    url:string,
}

type mmFileSource =
    | Local({fileName:string})
    | Web(webSource)

type stmtSym = {
    sym: string,
    color: option<string>,
}

type stmtContTreeData = {
    text:string, 
    exprTyp:string, 
    root:syntaxTreeNode, 
    clickedNodeId:option<(int,Js_date.t)>, 
    expLvl:int
}

type stmtCont =
    | Text({text:string, syms:array<stmtSym>})
    | Tree(stmtContTreeData)

let contIsEmpty = cont => {
    switch cont {
        | Text({text}) | Tree({text}) => text->Js_string2.length == 0
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text({text}) | Tree({text}) => getSpaceSeparatedValuesAsArray(text)
    }
}

let contToStr = cont => {
    switch cont {
        | Text({text}) | Tree({text}) => text
    }
}

let strToCont = (
    str:string,
    ~preCtxColors: option<Belt_HashMapString.t<string>>=?,
    ~wrkCtxColors: option<Belt_HashMapString.t<string>>=?,
    ()
):stmtCont => {
    let symsArr = getSpaceSeparatedValuesAsArray(str)
    Text({
        text: symsArr->Js_array2.joinWith(" "),
        syms: symsArr->Js.Array2.map(sym => {
            {
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
    })
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

let readInstrToStr = ri => {
    switch ri {
        | ReadAll => "ReadAll"
        | StopBefore => "StopBefore"
        | StopAfter => "StopAfter"
    }
}

let readInstrFromStr = str => {
    switch str {
        | "ReadAll" => ReadAll
        | "StopBefore" => StopBefore
        | "StopAfter" => StopAfter
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to a readInstr.`}))
    }
}

let mmFileSourceTypeToStr = (src:mmFileSourceType):string => {
    switch src {
        | Local => "Local"
        | Web => "Web"
    }
}

let mmFileSourceTypeFromStr = (str:string):mmFileSourceType => {
    switch str {
        | "Local" => Local
        | "Web" => Web
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to an mmFileSourceType.`}))
    }
}

let mmFileSourceTypeFromStrOpt = (str:string):option<mmFileSourceType> => {
    switch str {
        | "Local" => Some(Local)
        | "Web" => Some(Web)
        | _ => None
    }
}

type stmtId = string

type proofStatus = Ready | Waiting | NoJstf | JstfIsIncorrect

type stmtGenericError = {
    code:int,
    msg:string,
}
let someStmtErrCode = -1
let duplicatedStmtErrCode = 1

type userStmt = {
    id: stmtId,

    label: string,
    labelEditMode: bool,
    typ: userStmtType,
    typEditMode: bool,
    isGoal: bool,
    cont: stmtCont,
    contEditMode: bool,
    isDuplicated: bool,
    
    jstfText: string,
    jstfEditMode: bool,

    stmtErr: option<stmtGenericError>,

    expr: option<expr>,
    jstf: option<jstf>,
    proofTreeDto: option<proofTreeDto>,
    src: option<exprSrcDto>,
    proof: option<proofNodeDto>,
    proofStatus: option<proofStatus>,
    unifErr: option<string>,
    syntaxErr: option<string>,
}

type editorState = {
    settingsV:int,
    settings:settings,
    typeColors: Belt_HashMapString.t<string>,

    srcs: array<mmCtxSrcDto>,
    preCtxV: int,
    preCtx: mmContext,
    frms: frms,
    parenCnt: parenCnt,
    preCtxColors: Belt_HashMapString.t<string>,
    allTypes: array<int>,
    syntaxTypes: array<int>,
    parensMap: Belt_HashMapString.t<string>,
    typeOrderInDisj:Belt_HashMapInt.t<int>,

    descr: string,
    descrEditMode: bool,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,
    wrkCtxColors: Belt_HashMapString.t<string>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,

    wrkCtx: option<mmContext>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<(stmtId,Js_date.t)>,

    unifyAllIsRequired: bool,
    continueMergingStmts: bool,
}

type wrkSubsErr =
    | CommonVar({var1:int, var2:int, commonVar:int})
    | TypeMismatch({var:int, subsExpr:expr, typeExpr:expr})

type wrkSubs = {
    newDisj: disjMutable,
    subs: Belt_MapInt.t<expr>,
    mutable err: option<wrkSubsErr>,
}

let createEmptyUserStmt = (id, typ, label, isGoal):userStmt => {
    { 
        id, 
        label, labelEditMode:false, 
        typ, typEditMode:false, 
        isGoal,
        cont:Text({text:"", syms:[]}), contEditMode:true,
        isDuplicated:false,
        jstfText:"", jstfEditMode:false,
        stmtErr: None,
        expr:None, jstf:None, proofTreeDto:None, src:None, proof:None, proofStatus:None, unifErr:None, syntaxErr:None,
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

let unselectStmt = (stmt:userStmt):userStmt => {
    switch stmt.cont {
        | Text(_) => stmt
        | Tree(treeData) => {
            switch treeData.clickedNodeId {
                | None => stmt
                | Some(_) => {
                    {
                        ...stmt,
                        cont: Tree({...treeData, clickedNodeId: None})
                    }
                }
            }
        }
    }
}

let editorGetStmtById = (st:editorState,id:stmtId):option<userStmt> => st.stmts->Js_array2.find(stmt => stmt.id == id)
let editorGetStmtByLabel = (st:editorState,label:string):option<userStmt> => {
    st.stmts->Js_array2.find(stmt => stmt.label == label)
}

let editorGetStmtByIdExn = (st:editorState,id:stmtId):userStmt => {
    switch editorGetStmtById(st,id) {
        | None => raise(MmException({msg:`editorGetStmtByIdExn: Cannot find a step by id.`}))
        | Some(stmt) => stmt
    }
}

let getStmtIdx = (st:editorState,id:stmtId):int => {
    st.stmts->Js_array2.findIndex(stmt => stmt.id == id)
}

let updateStmt = (st:editorState,id,update):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let isStmtChecked = (st:editorState,stmtId:stmtId):bool => {
    st.checkedStmtIds->Js.Array2.some(((id,_)) => id == stmtId)
}

let toggleStmtChecked = (st,stmtId:stmtId) => {
    if (isStmtChecked(st,stmtId)) {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.filter(((checkedId,_)) => checkedId != stmtId)
        }
    } else {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.concat([(stmtId,Js_date.make())])
        }
    }
}

let checkAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: st.stmts->Js.Array2.map(stmt => (stmt.id, Js_date.make()))
    }
}

let uncheckAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: [],
        stmts: st.stmts->Js.Array2.map(unselectStmt)
    }
}

let deleteCheckedStmts = (st:editorState):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.filter(stmt => !isStmtChecked(st,stmt.id)),
        checkedStmtIds: [],
    }
}

let deleteStmt = (st:editorState, stmtId:stmtId):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.filter(stmt => stmt.id != stmtId),
        checkedStmtIds: st.checkedStmtIds->Js_array2.filter(((checkedId,_)) => checkedId != stmtId),
    }
}

let deleteStmts = (st:editorState, stmtIds:array<stmtId>):editorState => {
    {
        ...st,
        stmts: st.stmts->Js_array2.filter(stmt => !(stmtIds->Js_array2.includes(stmt.id))),
        checkedStmtIds: st.checkedStmtIds->Js_array2.filter(((checkedId,_)) => {
            !(stmtIds->Js_array2.includes(checkedId))
        }),
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

let getAllStmtsUpToChecked = (st:editorState):array<userStmt> => {
    let checkedAdded = ref(false)
    let res = []
    let i = ref(0)
    let stmtsLen = st.stmts->Js_array2.length
    while (i.contents < stmtsLen && !checkedAdded.contents) {
        let stmt = st.stmts[i.contents]
        res->Js.Array2.push(stmt)->ignore
        checkedAdded.contents = st->isStmtChecked(stmt.id)
        i.contents = i.contents + 1
    }
    res
}

let getRootStmtsForUnification = (st):array<userStmt> => {
    st->getAllStmtsUpToChecked
}

let createNewLabel = (st:editorState, ~prefix:option<string>=?, ~forHyp:bool=false, ()):string => {
    let reservedLabels = Belt_HashSetString.fromArray(st.stmts->Js_array2.map(stmt=>stmt.label))
    switch textToVarDefs(st.varsText) {
        | Error(_) => ()
        | Ok(varDefs) => {
            varDefs->Js_array2.forEach(varDef => {
                reservedLabels->Belt_HashSetString.add(varDef[0])
            })
        }
    }

    let labelIsReserved = (label:string):bool => {
        reservedLabels->Belt_HashSetString.has(label) || st.preCtx->isHyp(label) ||
            forHyp && st.preCtx->getTokenType(label)->Belt.Option.isSome
    }

    let prefixToUse = switch prefix {
        | Some(prefix) => prefix
        | None => {
            if (forHyp) {
                switch st.stmts->Js.Array2.find(stmt => stmt.isGoal) {
                    | None => ""
                    | Some(goal) => goal.label ++ "."
                }
            } else {
                ""
            }
        }
    }
    
    let cnt = ref(1)
    let newLabel = ref(prefixToUse ++ cnt.contents->Belt_Int.toString)
    while (labelIsReserved(newLabel.contents)) {
        cnt.contents = cnt.contents + 1
        newLabel.contents = prefixToUse ++ cnt.contents->Belt_Int.toString
    }
    newLabel.contents
}

let getTopmostCheckedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Js.Array2.length == 0) {
        None
    } else {
        st.stmts->Js_array2.find(stmt => isStmtChecked(st,stmt.id))
    }
}

let addNewStmt = (st:editorState):(editorState,stmtId) => {
    let newId = st.nextStmtId->Belt_Int.toString
    let pCnt = st.stmts->Js.Array2.reduce(
        (cnt,stmt) => if (stmt.typ == P) {cnt + 1} else {cnt},
        0
    )
    let defaultStmtLabel = st.settings.defaultStmtLabel->Js.String2.trim
    let newLabel = 
        if (pCnt == 0 && defaultStmtLabel->Js.String2.length > 0) {
            if (st.stmts->Js.Array2.some(stmt => stmt.label == defaultStmtLabel)) {
                createNewLabel(st, ~prefix=defaultStmtLabel, ~forHyp=false, ())
            } else {
                defaultStmtLabel
            }
        } else {
            createNewLabel(st, ~prefix="", ~forHyp=false, ())
        }
    let isGoal = pCnt == 0 && st.settings.initStmtIsGoal
    let idToAddBefore = getTopmostCheckedStmt(st)->Belt_Option.map(stmt => stmt.id)
    (
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                switch idToAddBefore {
                    | Some(idToAddBefore) => {
                        st.stmts->Js_array2.map(stmt => {
                            if (stmt.id == idToAddBefore) {
                                [createEmptyUserStmt(newId,P,newLabel,isGoal), stmt]
                            } else {
                                [stmt]
                            }
                        })->Belt_Array.concatMany
                    }
                    | None => st.stmts->Js_array2.concat([createEmptyUserStmt(newId, P, newLabel, isGoal)])
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

let duplicateCheckedStmt = (st:editorState, top:bool) => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let (idToAddAfter,_) = st.checkedStmtIds[0]
        let st = {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                st.stmts->Js_array2.map(stmt => {
                    if (stmt.id == idToAddAfter) {
                        [
                            stmt, 
                            {
                                ...stmt, 
                                id:newId, 
                                label:createNewLabel(st, ~forHyp = stmt.typ == E, ()),
                                isGoal:false, 
                                jstfText:"",
                                isDuplicated:true,
                            }
                        ]
                    } else {
                        [stmt]
                    }
                })->Belt_Array.concatMany,
            checkedStmtIds: [(newId,Js_date.make())],
        }
        if (top) {
            st->moveCheckedStmts(true)
        } else {
            st
        }
    }
}

let canGoEditModeForStmt = (st:editorState,stmtId) => {
    !(st.stmts->Js_array2.some(stmt => 
        stmt.id == stmtId && (stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode)
    ))
}

let setDescrEditMode = st => {
    {
        ...st,
        descrEditMode: true
    }
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
                contEditMode: false,
                isDuplicated: false,
            }
        }
    })
}

let setStmtCont = (st, stmtId, stmtCont):editorState => {
    let newContStr = stmtCont->contToStr
    let isDuplicated = st.settings.autoMergeStmts && st.stmts->Js.Array2.some(stmt => stmt.cont->contToStr == newContStr)
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            cont:stmtCont,
            isDuplicated,
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

let setJstfEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, jstfEditMode:true})
    } else {
        st
    }
}

let setUnifyAllIsRequired = (st:editorState, required:bool) => {
    {
        ...st,
        unifyAllIsRequired: required
    }
}

let setContinueMergingStmts = (st:editorState, continue:bool) => {
    {
        ...st,
        continueMergingStmts: continue
    }
}

let extractVarColorsFromVarsText = (varsText:string, typeColors:Belt_HashMapString.t<string>):Belt_HashMapString.t<string> => {
    let res = Belt_HashMapString.make(~hintSize=16)
    switch textToVarDefs(varsText) {
        | Error(_) => ()
        | Ok(varDefs) => {
            varDefs->Js_array2.forEach(varDef => {
                let varName = varDef[2]
                let typ = varDef[1]
                switch typeColors->Belt_HashMapString.get(typ) {
                    | None => ()
                    | Some(color) => res->Belt_HashMapString.set(varName, color)
                }
            })
        }
    }
    res
}

let recalcTypeColors = (st:editorState):editorState => {
    {
        ...st,
        typeColors: st.settings->settingsGetTypeColors
    }
}

let createSymbolColors = (~ctx:mmContext, ~typeColors: Belt_HashMapString.t<string>):Belt_HashMapString.t<string> => {
    let symbolColors = Belt_HashMapString.make(~hintSize=100)
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F) {
            switch ctx->ctxIntToSym(hyp.expr[0]) {
                | None => ()
                | Some(typeStr) => {
                    switch typeColors->Belt_HashMapString.get(typeStr) {
                        | None => ()
                        | Some(color) => {
                            symbolColors->Belt_HashMapString.set(
                                ctx->ctxIntToSymExn(hyp.expr[1]),
                                color
                            )
                        }
                    }
                }
            }
        }
        None
    })->ignore
    symbolColors
}

let recalcPreCtxColors = (st:editorState):editorState => {
    {
        ...st,
        preCtxColors: createSymbolColors(~ctx=st.preCtx, ~typeColors=st.typeColors)
    }
}

let recalcWrkCtxColors = (st:editorState):editorState => {
    {
        ...st,
        wrkCtxColors: extractVarColorsFromVarsText(st.varsText, st.typeColors),
    }
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

let setPreCtxData = (st:editorState, preCtxData:preCtxData):editorState => {
    let settings = preCtxData.settingsV.val
    let preCtx = preCtxData.ctxV.val->ctxOptimizeForProver(
        ~parens=settings.parens, ~removeAsrtDescr=true, ~removeProofs=true, ()
    )
    let parenInts = prepareParenInts(preCtx, settings.parens)
    let numOfParens = parenInts->Js_array2.length / 2
    let parensMap = Belt_HashMapString.make(~hintSize=numOfParens)
    for i in 0 to numOfParens-1 {
        parensMap->Belt_HashMapString.set(
            preCtx->ctxIntToSymExn(parenInts[2*i]), 
            preCtx->ctxIntToSymExn(parenInts[2*i+1])
        )
    }
    let typeOrderInDisj = createTypeOrderFromStr(
        ~sortDisjByType=settings.sortDisjByType, 
        ~typeNameToInt=preCtx->ctxSymToInt
    )
    let st = {
        ...st, 
        settingsV:preCtxData.settingsV.ver, 
        settings,
        srcs:preCtxData.srcs,
        preCtxV:preCtxData.ctxV.ver, 
        preCtx, 
        frms:preCtxData.frms,
        parenCnt:preCtxData.parenCnt,
        allTypes:preCtxData.allTypes,
        syntaxTypes:preCtxData.syntaxTypes,
        parensMap,
        typeOrderInDisj,
    }
    let st = recalcTypeColors(st)
    let st = recalcPreCtxColors(st)
    let st = recalcWrkCtxColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let completeDescrEditMode = (st, newDescr) => {
    {
        ...st,
        descr:newDescr,
        descrEditMode: false
    }
}

let recalcWrkColors = (st:editorState):editorState => {
    let st = recalcWrkCtxColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let completeVarsEditMode = (st, newVarsText) => {
    let st = {
        ...st,
        varsText:newVarsText,
        varsEditMode: false
    }
    let st = recalcWrkColors(st)
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
            | P => {
                if (st.settings.stickGoalToBottom) {
                    if (stmt.isGoal) {3} else {2}
                } else {
                    2
                }
            }
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

let isEditMode = (st:editorState): bool => {
    st.descrEditMode || st.varsEditMode || st.disjEditMode ||
        st.stmts->Js.Array2.some(stmt => 
            stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode 
        )
}

let userStmtHasErrors = stmt => {
    stmt.stmtErr->Belt_Option.isSome
}

let editorStateHasErrors = st => {
    st.varsErr->Belt_Option.isSome 
        || st.disjErr->Belt_Option.isSome 
        || st.stmts->Js_array2.some(userStmtHasErrors)
}

let editorStateHasDuplicatedStmts = (st:editorState):bool => {
    st.stmts->Js.Array2.some(stmt => {
        stmt.stmtErr->Belt_Option.map(err => err.code == duplicatedStmtErrCode)->Belt.Option.getWithDefault(false)
    })
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
    st
}

let refreshWrkCtx = (st:editorState):editorState => {
    let wrkCtxRes = createWrkCtx(
        ~preCtx=st.preCtx,
        ~varsText=st.varsText,
        ~disjText=st.disjText,
    )
    let st = switch wrkCtxRes {
        | Error(wrkCtxErr) => parseWrkCtxErr(st, wrkCtxErr)
        | Ok(wrkCtx) => {
            let st = {...st, wrkCtx:Some(wrkCtx)}
            st
        }
    }
    st
}

let parseJstf = (jstfText:string):result<option<jstf>,string> => {
    let jstfText = jstfText->Js_string2.trim
    if (jstfText->Js_string2.length == 0) {
        Ok(None)
    } else {
        let argsAndAsrt = jstfText->Js_string2.split(":")
        if (argsAndAsrt->Js_array2.length != 2) {
            Error(`Cannot parse justification: '${jstfText}'. A justification must contain exactly one colon symbol.`)
        } else if (argsAndAsrt[1]->Js_string2.trim == "") {
            Error(`Cannot parse justification: '${jstfText}'. Reference must not be empty.`)
        } else {
            Ok(Some({
                args: argsAndAsrt[0]->getSpaceSeparatedValuesAsArray,
                label: argsAndAsrt[1]->Js_string2.trim
            }))
        }
    }
}

let setStmtExpr = (stmt:userStmt,wrkCtx:mmContext):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        try {
            {
                ...stmt,
                expr: Some(wrkCtx->ctxSymsToIntsExn(stmt.cont->contToArrStr)),
            }
        } catch {
            | MmException({msg}) => {...stmt, stmtErr:Some({code:someStmtErrCode, msg})}
        }
    }
}

let setStmtJstf = (stmt:userStmt):userStmt => {
    if (userStmtHasErrors(stmt) || stmt.typ == E) {
        stmt
    } else {
        switch parseJstf(stmt.jstfText) {
            | Error(msg) => {...stmt, stmtErr:Some({code:someStmtErrCode, msg})}
            | Ok(jstf) => {...stmt, jstf}
        }
    }
}

let isLabelDefined = (label:string, wrkCtx:mmContext, definedUserLabels:Belt_HashSetString.t) => {
    definedUserLabels->Belt_HashSetString.has(label) || wrkCtx->isHyp(label)
}

let validateStmtJstf = (
    stmt:userStmt, 
    wrkCtx:mmContext, 
    definedUserLabels:Belt_HashSetString.t,
    frms: frms,
):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch stmt.jstf {
            | None => stmt
            | Some({args,label}) => {
                switch args->Js_array2.find(ref => !isLabelDefined(ref,wrkCtx,definedUserLabels) ) {
                    | Some(jstfArgLabel) => {
                        {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`The label '${jstfArgLabel}' is not defined.`})}
                    }
                    | None => {
                        if (!(wrkCtx->isAsrt(label))) {
                            {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`The label '${label}' doesn't refer to any assertion.`})}
                        } else {
                            switch frms->frmsGetByLabel(label) {
                                | None => raise(MmException({msg:`Could not get frame by label '${label}'`}))
                                | Some(frm) => {
                                    let expectedNumberOfArgs = frm.numOfHypsE
                                    let providedNumberOfArgs = args->Js_array2.length
                                    if (providedNumberOfArgs != expectedNumberOfArgs) {
                                        let eHypsText = if (expectedNumberOfArgs == 1) {
                                            "essential hypothesis"
                                        } else {
                                            "essential hypotheses"
                                        }
                                        let isAreText = if (providedNumberOfArgs == 1) {
                                            "is"
                                        } else {
                                            "are"
                                        }
                                        {
                                            ...stmt, 
                                            stmtErr:Some({
                                                code:someStmtErrCode, 
                                                msg:`'${label}' assertion expects ${expectedNumberOfArgs->Belt_Int.toString} ${eHypsText} but`
                                                    ++ ` ${providedNumberOfArgs->Belt_Int.toString} ${isAreText} provided.`
                                            })
                                        }
                                    } else {
                                        stmt
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let validateStmtLabel = (stmt:userStmt, wrkCtx:mmContext, definedUserLabels:Belt_HashSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        if (stmt.typ == E || stmt.typ == P && stmt.isGoal) {
            switch wrkCtx->getTokenType(stmt.label) {
                | Some(tokenType) => {
                    if (stmt.typ == E) {
                        {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`[1] Cannot reuse label '${stmt.label}'.`})}
                    } else {
                        switch tokenType {
                            | C | V | F | E => {
                                {
                                    ...stmt, 
                                    stmtErr:Some({code:someStmtErrCode, msg:`[2] Cannot reuse label '${stmt.label}'.`})
                                }
                            }
                            | A | P => stmt
                        }
                    }
                }
                | None => {
                    if (isLabelDefined(stmt.label,wrkCtx,definedUserLabels)) {
                        {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`[3] Cannot reuse label '${stmt.label}'.`})}
                    } else {
                        stmt
                    }
                }
            }
        } else {
            if (isLabelDefined(stmt.label,wrkCtx,definedUserLabels)) {
                {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`[4] Cannot reuse label '${stmt.label}'.`})}
            } else {
                stmt
            }
        }
    }
}

let validateStmtExpr = (
    stmt:userStmt, 
    wrkCtx:mmContext,
    definedUserExprs:Belt_HashMap.t<expr,string,ExprHash.identity>,
):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch stmt.expr {
            | None => raise(MmException({msg:`Cannot validateStmtExpr without expr.`}))
            | Some(expr) => {
                switch wrkCtx->getHypByExpr(expr) {
                    | Some(hyp) => {
                        {...stmt, stmtErr:Some({code:duplicatedStmtErrCode, 
                                                msg:`This statement is the same as the previously defined` 
                                                    ++ ` hypothesis - '${hyp.label}'`})}
                    }
                    | _ => {
                        switch definedUserExprs->Belt_HashMap.get(expr) {
                            | Some(prevStmtLabel) => {
                                {...stmt, stmtErr:Some({code:duplicatedStmtErrCode, 
                                                        msg:`This statement is the same as the previous` 
                                                            ++ ` one - '${prevStmtLabel}'`})}
                            }
                            | None => {
                                if (!stmt.contEditMode && (expr->Js_array2.length == 0 || expr[0] >= 0)) {
                                    {...stmt, stmtErr:Some({code:someStmtErrCode, 
                                        msg:`Any statement must begin with a constant.`})}
                                } else {
                                    stmt
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let validateStmtIsGoal = (
    stmt:userStmt, 
    goalLabel:ref<option<string>>,
):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch goalLabel.contents {
            | None => stmt
            | Some(goalLabel) => {
                if (stmt.isGoal) {
                    {...stmt, stmtErr:Some({code:someStmtErrCode, msg:`Cannot re-define the goal. ` 
                                    ++ `Previously defined goal is the step labeled` 
                                    ++ ` '${goalLabel}'`})}
                } else {
                    stmt
                }
            }
        }
    }
}

let prepareUserStmtsForUnification = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot prepareUserStmtsForUnification without wrkCtx.`}))
        | Some(wrkCtx) => {
            let stmtsLen = st.stmts->Js_array2.length
            let definedUserLabels = Belt_HashSetString.make(~hintSize=stmtsLen)
            let definedUserExprs = Belt_HashMap.make(~hintSize=stmtsLen, ~id=module(ExprHash))
            let goalLabel = ref(None)
            let actions = [
                validateStmtLabel(_, wrkCtx, definedUserLabels),
                setStmtExpr(_, wrkCtx),
                validateStmtIsGoal(_, goalLabel),
                setStmtJstf,
                validateStmtJstf(_, wrkCtx, definedUserLabels, st.frms),
                validateStmtExpr(_, wrkCtx, definedUserExprs),
            ]
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (editorStateHasErrors(st)) {
                        st
                    } else {
                        let stmt = actions->Js_array2.reduce(
                            (stmt,action) => {
                                if (userStmtHasErrors(stmt)) {
                                    stmt
                                } else {
                                    action(stmt)
                                }
                            },
                            stmt
                        )

                        definedUserLabels->Belt_HashSetString.add(stmt.label)
                        if (!userStmtHasErrors(stmt)) {
                            switch stmt.expr {
                                | None => raise(MmException({msg:`Expr must be set in prepareUserStmtsForUnification.`}))
                                | Some(expr) => definedUserExprs->Belt_HashMap.set(expr, stmt.label)
                            }
                            if (stmt.isGoal) {
                                goalLabel := Some(stmt.label)
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

let prepareEditorForUnification = st => {
    let st = removeAllTempData(st)
    [
        sortStmtsByType,
        refreshWrkCtx,
        prepareUserStmtsForUnification,
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

let getTheOnlyCheckedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Js.Array2.length != 1) {
        None
    } else {
        getTopmostCheckedStmt(st)
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
                wrkCtx->applySingleStmt(Var({symbols:newVarNames}), ())
                let varTypeNames = wrkCtx->ctxIntsToSymsExn(varTypes)
                newHypLabels->Js.Array2.forEachi((label,i) => {
                    wrkCtx->applySingleStmt(Floating({label, expr:[varTypeNames[i], newVarNames[i]]}), ())
                })
                let newVarInts = wrkCtx->ctxSymsToIntsExn(newVarNames)
                let newVarsText = newHypLabels->Js.Array2.mapi((label,i) => {
                    `${label} ${varTypeNames[i]} ${newVarNames[i]}`
                })->Js_array2.joinWith("\n")
                let st = {
                    ...st,
                    varsText: [st.varsText, newVarsText]->Js.Array2.joinWith("\n")->Js.String2.trim
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
            let newDisjTextLines = []
            newDisj->disjForEachArr(varInts => {
                let varsStr = wrkCtx->ctxIntsToSymsExn(varInts)
                wrkCtx->applySingleStmt(Disj({vars:varsStr}), ())
                newDisjTextLines->Js.Array2.push(varsStr->Js.Array2.joinWith(","))->ignore
            })
            if (newDisjTextLines->Js.Array2.length == 0) {
                st
            } else {
                let newDisjText = newDisjTextLines->Js.Array2.joinWith("\n")
                {
                    ...st,
                    disjText: [st.disjText, newDisjText]->Js.Array2.joinWith("\n")->Js.String2.trim
                }
            }
        }
    }
}

let stmtsHaveSameExpr = ( stmt:userStmt, stmtDto:stmtDto ):bool => {
    switch stmt.expr {
        | None => raise(MmException({msg:`Cannot compare steps without expr.`}))
        | Some(expr) => expr->exprEq(stmtDto.expr)
    }
}

let getUserStmtByExpr = (st, expr):option<userStmt> => {
    st.stmts->Js_array2.find(stmt => {
        switch stmt.expr {
            | None => raise(MmException({msg:`Cannot getUserStmtByExpr without stmt.expr.`}))
            | Some(stmtExpr) => stmtExpr->exprEq(expr)
        }
    })
}

let insertStmt = (
    st:editorState, 
    ~expr:expr, 
    ~jstf:option<jstf>, 
    ~before:option<stmtId>,
    ~placeAtMaxIdxByDefault:bool,
):(editorState,string) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot insertStmt without wrkCtx.`}))
        | Some(wrkCtx) => {
            switch wrkCtx->getHypByExpr(expr) {
                | Some(hyp) => (st,hyp.label)
                | None => {
                    let maxIdx = switch before {
                        | None => st.stmts->Js_array2.length
                        | Some(stmtId) => {
                            switch st.stmts->Js_array2.findIndex(stmt => stmt.id == stmtId) {
                                | -1 => st.stmts->Js_array2.length
                                | idx => idx
                            }
                        }
                    }
                    let minIdx = switch jstf {
                        | None => 0
                        | Some({args}) => {
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
                    }

                    let updateExistingStmt = (st,existingStmt):(editorState,string) => {
                        let st = switch jstf {
                            | None => st
                            | Some(jstf) => {
                                st->updateStmt(existingStmt.id, stmt => { ...stmt, jstfText: jstfToStr(jstf) })
                            }
                        }
                        (st, existingStmt.label)
                    }

                    let insertNewStmt = (st,existingStmt:option<userStmt>):(editorState,string) => {
                        let newIdx = switch existingStmt {
                            | None => if placeAtMaxIdxByDefault {maxIdx} else {minIdx}
                            | Some(existingStmt) => {
                                let newIdx = st->getStmtIdx(existingStmt.id) + 1
                                if (minIdx <= newIdx && newIdx <= maxIdx) { newIdx } else { minIdx }
                            }
                        }
                        let (st,newStmtId) = st->addNewStmtAtIdx(newIdx)
                        let st = st->updateStmt(newStmtId, stmt => {
                            {
                                ...stmt,
                                typ: P,
                                cont: strToCont( 
                                    wrkCtx->ctxIntsToStrExn(expr), 
                                    ~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors, ()
                                ),
                                contEditMode: false,
                                jstfText: jstf->Belt_Option.mapWithDefault("", jstfToStr),
                                expr:Some(expr),
                            }
                        })
                        (st, (st->editorGetStmtByIdExn(newStmtId)).label)
                    }

                    let existingJstfEqNewJstf = existingJstf => {
                        switch jstf {
                            | Some(jstf) => jstfEq(existingJstf, jstf)
                            | _ => false
                        }
                    }

                    switch getUserStmtByExpr(st,expr) {
                        | None => insertNewStmt(st,None)
                        | Some(existingStmt) => {
                            if (existingStmt.typ == E || jstf->Belt.Option.isNone) {
                                (st, existingStmt.label)
                            } else {
                                switch parseJstf(existingStmt.jstfText) {
                                    | Ok(None) => updateExistingStmt(st,existingStmt)
                                    | Ok(Some(existingJstf)) => {
                                        if (existingJstf->existingJstfEqNewJstf) {
                                            (st, existingStmt.label)
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
            }
        }
    }
}

let replaceDtoVarsWithCtxVarsInExprs = (newStmts:stmtsDto, newStmtsVarToCtxVar:Belt_MutableMapInt.t<int>):stmtsDto => {
    {
        ...newStmts,
        stmts: newStmts.stmts->Js_array2.map(stmt => {
            {
                ...stmt,
                expr: stmt.expr->Js_array2.map(i => newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(i,i))
            }
        })
    }
}

let addNewStatements = (st:editorState, newStmts:stmtsDto):editorState => {
    let (st, newCtxVarInts) = createNewVars(st,newStmts.newVarTypes)
    let newStmtsVarToCtxVar = Belt_MutableMapInt.make()
    newStmts.newVars->Js.Array2.forEachi((newStmtsVarInt,i) => {
        newStmtsVarToCtxVar->Belt_MutableMapInt.set(newStmtsVarInt, newCtxVarInts[i])
    })
    let newStmts = replaceDtoVarsWithCtxVarsInExprs(newStmts, newStmtsVarToCtxVar)

    let newCtxDisj = disjMake()
    newStmts.newDisj->disjForEach((n,m) => {
        newCtxDisj->disjAddPair(
            newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(n,n), 
            newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(m,m), 
        )
    })
    let st = createNewDisj(st, newCtxDisj)

    let checkedStmt = st->getTopmostCheckedStmt
    let newStmtsLabelToCtxLabel = Belt_MutableMapString.make()

    let replaceDtoLabelsWithCtxLabels = jstf => {
        {
            ...jstf,
            args: jstf.args->Js_array2.map(arg => 
                newStmtsLabelToCtxLabel->Belt_MutableMapString.getWithDefault(arg,arg)
            )
        }
    }

    let mergeWillBeNeeded = newStmts.stmts->Js_array2.some(stmtDto => {
        st.stmts->Js_array2.some(userStmt => stmtsHaveSameExpr(userStmt, stmtDto))
    })
    let placeAtMaxIdxByDefault = checkedStmt->Belt.Option.isSome && !mergeWillBeNeeded

    let stMut = ref(st)
    newStmts.stmts->Js_array2.forEach(stmtDto => {
        if (checkedStmt->Belt.Option.isSome && stmtsHaveSameExpr(checkedStmt->Belt.Option.getExn, stmtDto)) {
            let checkedStmt = checkedStmt->Belt.Option.getExn
            stMut.contents = updateStmt(stMut.contents, checkedStmt.id, stmtToUpdate => {
                {
                    ...stmtToUpdate,
                    jstfText:
                        switch stmtDto.jstf {
                            | None => stmtToUpdate.jstfText
                            | Some(jstf) => jstf->replaceDtoLabelsWithCtxLabels->jstfToStr
                        }
                }
            })
            newStmtsLabelToCtxLabel->Belt_MutableMapString.set(stmtDto.label,checkedStmt.label)
        } else {
            let (st, ctxLabel) = stMut.contents->insertStmt(
                ~expr=stmtDto.expr, 
                ~jstf=stmtDto.jstf->Belt_Option.map(replaceDtoLabelsWithCtxLabels), 
                ~before = checkedStmt->Belt_Option.map(stmt => stmt.id),
                ~placeAtMaxIdxByDefault
            )
            stMut.contents = st
            newStmtsLabelToCtxLabel->Belt_MutableMapString.set(stmtDto.label,ctxLabel)
        }
    })
    stMut.contents
}

let removeUnusedVars = (st:editorState):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot remove unused variables without wrkCtx.`}))
        | Some(wrkCtx) => {
            let usedSymbols = st.stmts
                ->Expln_utils_common.arrFlatMap(stmt=>stmt.cont->contToArrStr)
                ->Belt_HashSetString.fromArray
            wrkCtx->getLocalHyps->Js_array2.forEach(hyp => {
                if (hyp.typ == F && hyp.label->Js_string2.startsWith(".")) {
                    usedSymbols->Belt_HashSetString.add(wrkCtx->ctxIntToSymExn(hyp.expr[1]))
                }
            })
            let unusedVars = wrkCtx->getLocalVars->Js_array2.filter(var => !(usedSymbols->Belt_HashSetString.has(var)))
            let st = if (unusedVars->Js_array2.length == 0) {
                st
            } else {
                let unusedVarInts = wrkCtx->ctxSymsToIntsExn(unusedVars)->Belt_HashSetInt.fromArray
                let usedLocalVarsStr = wrkCtx->getLocalHyps
                    ->Js_array2.filter(hyp => hyp.typ == F && !(unusedVarInts->Belt_HashSetInt.has(hyp.expr[1])))
                    ->Js_array2.map(hyp => 
                        `${hyp.label} ${wrkCtx->ctxIntToSymExn(hyp.expr[0])} ${wrkCtx->ctxIntToSymExn(hyp.expr[1])}`
                    )
                    ->Js_array2.joinWith("\n")
                completeVarsEditMode(st, usedLocalVarsStr)
            }
            let newDisj = disjMake()
            wrkCtx->getLocalDisj->disjForEach((n,m) => {
                if (usedSymbols->Belt_HashSetString.has(wrkCtx->ctxIntToSymExn(n)) 
                    && usedSymbols->Belt_HashSetString.has(wrkCtx->ctxIntToSymExn(m))) {
                    newDisj->disjAddPair(n,m)
                }
            })
            let newDisjText = newDisj->disjToArr(
                ~sortByTypeAndName=true,
                ~varIntToVarName=wrkCtx->ctxIntToSym,
                ~varIntToVarType=wrkCtx->getTypeOfVar,
                ~typeOrder=st.typeOrderInDisj,
                ()
            )
                ->Js_array2.map(dgrp => wrkCtx->ctxIntsToSymsExn(dgrp)->Js_array2.joinWith(","))
                ->Js.Array2.joinWith("\n")
            let st = if (st.disjText != newDisjText) {
                completeDisjEditMode(st, newDisjText)
            } else {
                st
            }
            st
        }
    }
}

let srcToJstf = (wrkCtx, proofTree:proofTreeDto, exprSrc:exprSrcDto, exprToUserStmt):option<jstf> => {
    switch exprSrc {
        | Assertion({args, label}) => {
            switch wrkCtx->getFrame(label) {
                | None => raise(MmException({msg:`Cannot find an assertion '${label}' in srcToJstf.`}))
                | Some(frame) => {
                    let argLabels = []
                    let argLabelsValid = ref(true)
                    frame.hyps->Js_array2.forEachi((hyp,i) => {
                        if (hyp.typ == E && argLabelsValid.contents) {
                            switch args->Belt_Array.get(i) {
                                | None => raise(MmException({msg:`Too few arguments for '${label}' in srcToJstf.`}))
                                | Some(nodeIdx) => {
                                    switch exprToUserStmt->Belt_HashMap.get(proofTree.nodes[nodeIdx].expr) {
                                        | None => argLabelsValid.contents = false //todo: return a meaningful error from here
                                        | Some(userStmt) => argLabels->Js_array2.push(userStmt.label)->ignore
                                    }
                                }
                            }
                        }
                    })
                    if (argLabelsValid.contents) {
                        Some({ args: argLabels, label })
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
    let stmt = switch proofNode.proof {
        | None => stmt
        | Some(proofSrc) => {
            switch srcToJstf(wrkCtx, proofTree, proofSrc, exprToUserStmt) {
                | None => stmt
                | Some(jstfFromProof) => {
                    switch stmt.jstf {
                        | None => {
                            {
                                ...stmt,
                                jstfText: jstfToStr(jstfFromProof),
                                proofTreeDto: Some(proofTree),
                                proof: Some(proofNode),
                            }
                        }
                        | Some(existingJstf) => {
                            if (jstfFromProof->jstfEq(existingJstf)) {
                                {
                                    ...stmt,
                                    proofTreeDto: Some(proofTree),
                                    proof: Some(proofNode),
                                }
                            } else {
                                stmt
                            }
                        }
                    }
                }
            }
        }
    }
    {...stmt, proofTreeDto: Some(proofTree)}
}

let userStmtSetProofStatus = (stmt, wrkCtx, proofTree:proofTreeDto, proofNode:proofNodeDto, exprToUserStmt):userStmt => {
    let srcEqJstf = (src, jstf) => {
        switch srcToJstf(wrkCtx, proofTree, src, exprToUserStmt) {
            | None => false
            | Some(jstfFromSrc) => jstf->jstfEq(jstfFromSrc)
        }
    }

    switch stmt.proof {
        | Some(proofNode) => {...stmt, proofStatus:Some(Ready), src:proofNode.proof}
        | None => {
            switch stmt.jstf {
                | None => {
                    let partialAsrtLabels = proofNode.parents->Js.Array2.map(src => {
                        switch src {
                            | VarType | Hypothesis(_) | Assertion(_) => None
                            | AssertionWithErr({label, err}) => {
                                switch err {
                                    | UnifErr | DisjCommonVar(_) | Disj(_) | UnprovedFloating(_) 
                                                | NoUnifForAsrt(_) | NoUnifForArg(_) | NewVarsAreDisabled(_) => None
                                    | TooManyCombinations(_) => Some(label)
                                }
                            }
                        }
                    })->Js.Array2.filter(Belt_Option.isSome)->Js.Array2.map(Belt_Option.getExn)
                    let unifErr = if (partialAsrtLabels->Js.Array2.length > 0) {
                        Some(unifErrToStr(
                            TooManyCombinations({frmLabels:Some(partialAsrtLabels)}),
                            ~exprToStr = wrkCtx->ctxIntsToStrExn,
                            ~frmExprToStr = _ => "<frmExprToStr is not defined>"
                        ))
                    } else {
                        None
                    }
                    {
                        ...stmt, 
                        proofStatus:Some(NoJstf),
                        unifErr: stmt.unifErr->Belt.Option.orElse(unifErr)
                    }
                }
                | Some(jstf) => {
                    switch proofNode.parents->Js.Array2.find(srcEqJstf(_, jstf)) {
                        | Some(src) => {...stmt, proofStatus:Some(Waiting), src:Some(src)}
                        | None => {
                            let errors = proofNode.parents->Js.Array2.map(src => {
                                switch src {
                                    | VarType | Hypothesis(_) | Assertion(_) => None
                                    | AssertionWithErr({label, err}) => Some((label,err))
                                }
                            })->Js.Array2.filter(Belt_Option.isSome)->Js.Array2.map(Belt_Option.getExn)
                            let unifErr = if (errors->Js.Array2.length > 0) {
                                Some(
                                    errors->Js_array2.map(((asrtLabel,err)) => {
                                        unifErrToStr(
                                            err,
                                            ~exprToStr = wrkCtx->ctxIntsToStrExn,
                                            ~frmExprToStr = 
                                                expr => wrkCtx->frmIntsToStrExn(wrkCtx->getFrameExn(asrtLabel),expr)
                                        )
                                    })->Js.Array2.joinWith("\n\n")
                                )
                            } else {
                                None
                            }
                            {
                                ...stmt, 
                                proofStatus:Some(JstfIsIncorrect),
                                unifErr: stmt.unifErr->Belt.Option.orElse(unifErr)
                            }
                        }
                    }
                }
            }
        }
    }
}

let getColorForSymbol = (
    ~sym:string,
    ~preCtxColors:option<Belt_HashMapString.t<string>>,
    ~wrkCtxColors:option<Belt_HashMapString.t<string>>,
):option<string> => {
    switch preCtxColors {
        | None => wrkCtxColors->Belt_Option.flatMap(wrkCtxColors => wrkCtxColors->Belt_HashMapString.get(sym))
        | Some(preCtxColors) => {
            switch preCtxColors->Belt_HashMapString.get(sym) {
                | Some(color) => Some(color)
                | None => wrkCtxColors->Belt_Option.flatMap(wrkCtxColors => wrkCtxColors->Belt_HashMapString.get(sym))
            }
        }
    }
}

let rec addColorsToSyntaxTree = (
    ~tree:syntaxTreeNode,
    ~preCtxColors:option<Belt_HashMapString.t<string>>=?,
    ~wrkCtxColors:option<Belt_HashMapString.t<string>>=?,
    ()
):syntaxTreeNode => {
    {
        ...tree,
        children: tree.children->Js.Array2.map(child => {
            switch child {
                | Subtree(syntaxTreeNode) => {
                    Subtree(addColorsToSyntaxTree(~tree=syntaxTreeNode, ~preCtxColors?, ~wrkCtxColors?, ()))
                }
                | Symbol(symData) => {
                    Symbol({ ...symData, color:getColorForSymbol(~sym=symData.sym, ~preCtxColors, ~wrkCtxColors)})
                }
            }
        })
    }
}

let checkParensMatch = (expr,parenCnt):bool => {
    let parenState = ref(Balanced)
    parenCnt->parenCntReset
    let i = ref(0)
    while (i.contents < expr->Js_array2.length && parenState.contents != Failed) {
        parenState := parenCnt->parenCntPut(expr[i.contents])
        i := i.contents + 1
    }
    parenState.contents == Balanced
}

let stmtSetSyntaxTree = (
    ~st:editorState,
    ~stmt:userStmt,
    ~expr:expr,
    ~wrkCtx:mmContext,
    ~syntaxNodes:Belt_HashMap.t<expr,proofNodeDto,ExprHash.identity>,
    ~proofTreeDto:proofTreeDto
):userStmt => {
    switch stmt.cont {
        | Tree(_) => stmt
        | Text({text, syms}) => {
            let syntaxTree = switch syntaxNodes->Belt_HashMap.get(expr->Js_array2.sliceFrom(1)) {
                | None => None
                | Some(nodeDto) => Some(buildSyntaxTreeFromProofTreeDto(~ctx=wrkCtx, ~proofTreeDto, ~typeStmt=nodeDto.expr))
            }
            switch syntaxTree {
                | None => {
                    if (st.settings.checkSyntax) {
                        {
                            ...stmt,
                            syntaxErr: Some(if (checkParensMatch(expr, st.parenCnt)) {""} else {"parentheses mismatch"}),
                        }
                    } else {
                        stmt
                    }
                }
                | Some(Error(msg)) => {
                    {
                        ...stmt,
                        syntaxErr: Some(msg),
                    }
                }
                | Some(Ok(syntaxTree)) => {
                    {
                        ...stmt,
                        cont: Tree({
                            text,
                            exprTyp: syms[0].sym, 
                            root: addColorsToSyntaxTree( 
                                ~tree=syntaxTree, 
                                ~preCtxColors=st.preCtxColors, 
                                ~wrkCtxColors=st.wrkCtxColors, 
                                ()
                            ), 
                            clickedNodeId: None,
                            expLvl:0,
                        })
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
            let exprToNode = proofTreeDto.nodes
                ->Js_array2.map(node => (node.expr,node))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let exprToUserStmt = st.stmts
                ->Js_array2.map(stmt => {
                    switch stmt.expr {
                        | None => 
                            raise(MmException({
                                msg:`Cannot applyUnifyAllResults without stmt.expr [1].`
                            }))
                        | Some(expr) => (expr, stmt)
                    }
                })
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let syntaxNodes = proofTreeDto.syntaxProofs->Belt_HashMap.fromArray(~id=module(ExprHash))
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    let stmt = {...stmt, proof:None, proofStatus: None}
                    st->updateStmt(stmt.id, stmt => {
                        switch stmt.expr {
                            | None => raise(MmException({ msg:`Cannot applyUnifyAllResults without stmt.expr [2].`}))
                            | Some(expr) => {
                                let stmt = stmtSetSyntaxTree(
                                    ~st,
                                    ~stmt,
                                    ~expr,
                                    ~wrkCtx,
                                    ~syntaxNodes,
                                    ~proofTreeDto
                                )
                                let stmt = if (stmt.typ == P) {
                                    switch exprToNode->Belt_HashMap.get(expr) {
                                        | None => raise(MmException({ msg:`exprToNode->Belt_HashMap.get(expr) is None`}))
                                        | Some(node) => {
                                            let stmt = userStmtSetJstfTextAndProof(stmt,wrkCtx,proofTreeDto,node,exprToUserStmt)
                                            let stmt = userStmtSetProofStatus(stmt,wrkCtx,proofTreeDto,node,exprToUserStmt)
                                            stmt
                                        }
                                    }
                                } else {
                                    stmt
                                }
                                stmt
                            }
                        }
                    })
                },
                st
            )
        }
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

let proofToText = (
    ~wrkCtx:mmContext,
    ~typeOrderInDisj:Belt_HashMapInt.t<int>,
    ~newHyps:array<hypothesis>,
    ~newDisj:disjMutable,
    ~descr:string,
    ~stmt:userStmt,
    ~proof:proof
):string => {
    switch proof {
        | Compressed({labels, compressedProofBlock}) => {
            let blk = splitIntoChunks(compressedProofBlock, 50)->Js_array2.joinWith(" ")
            let asrt = `${stmt.label} $p ${stmt.cont->contToStr} $= ( ${labels->Js_array2.joinWith(" ")} ) ${blk} $.`
            let descrIsEmpty = descr->Js_string2.trim->Js_string2.length == 0
            let blockIsRequired = newHyps->Js.Array2.length > 0 || !(newDisj->disjIsEmpty) || !descrIsEmpty
            let result = []
            if (blockIsRequired) {
                result->Js.Array2.push("${")->ignore
            }
            let varsArrStr = newHyps->Js_array2.filter(hyp => hyp.typ == F)
                ->Js.Array2.map(hyp => wrkCtx->ctxIntToSymExn(hyp.expr[1]))
            if (varsArrStr->Js.Array2.length > 0) {
                result->Js.Array2.push("$v " ++ varsArrStr->Js.Array2.joinWith(" ") ++ " $.")->ignore
            }
            newHyps->Js.Array2.forEach(hyp => {
                if (hyp.typ == F) {
                    result->Js.Array2.push(hyp.label ++ " $f " ++ wrkCtx->ctxIntsToStrExn(hyp.expr) ++ " $.")->ignore
                }
            })
            newDisj->disjForEachArr(
                ~sortByTypeAndName=true,
                ~varIntToVarType=wrkCtx->getTypeOfVar,
                ~varIntToVarName=wrkCtx->ctxIntToSym,
                ~typeOrder=typeOrderInDisj,
                vars => {
                    result->Js.Array2.push("$d " ++ wrkCtx->ctxIntsToStrExn(vars) ++ " $.")->ignore
                }
            )
            newHyps->Js.Array2.forEach(hyp => {
                if (hyp.typ == E) {
                    result->Js.Array2.push(hyp.label ++ " $e " ++ wrkCtx->ctxIntsToStrExn(hyp.expr) ++ " $.")->ignore
                }
            })
            if (!descrIsEmpty) {
                result->Js.Array2.push("$( " ++ descr ++ " $)")->ignore
            }
            result->Js.Array2.push(asrt)->ignore
            if (blockIsRequired) {
                result->Js.Array2.push("$}")->ignore
            }
            result->Js.Array2.joinWith("\r\n")
        }
        | _ => "Error: only compressed proofs are supported."
    }
}

let generateCompressedProof = (st, stmtId):option<(string,string,string)> => {
    switch st.wrkCtx {
        | None => None
        | Some(wrkCtx) => {
            switch st->editorGetStmtById(stmtId) {
                | None => None
                | Some(stmt) => {
                    switch stmt.proofTreeDto {
                        | None => None
                        | Some(proofTreeDto) => {
                            switch stmt.proof {
                                | None => None
                                | Some(proofNode) => {
                                    let preCtx = st.preCtx
                                    let expr = userStmtToRootStmt(stmt).expr
                                    let proofTableWithTypes = createProofTable(~tree=proofTreeDto, ~root=proofNode, ())
                                    let proofTableWithoutTypes = createProofTable(
                                        ~tree=proofTreeDto, ~root=proofNode, ~essentialsOnly=true, ~ctx=wrkCtx, ()
                                    )
                                    let exprsUsedInProof = proofTableWithTypes->Js.Array2.map(r => r.expr)
                                        ->Belt_HashSet.fromArray(~id=module(ExprHash))
                                    let rootStmts = st.stmts->Js_array2.map(userStmtToRootStmt)
                                    let proofCtx = createProofCtx(
                                        wrkCtx,
                                        rootStmts->Js_array2.filter(stmt => {
                                            stmt.isHyp && exprsUsedInProof->Belt_HashSet.has(stmt.expr)
                                        })
                                    )

                                    let mandHyps = proofCtx->getMandHyps(expr)
                                    let proof = MM_proof_table.createProof(
                                        mandHyps, proofTableWithTypes, proofTableWithTypes->Js_array2.length-1
                                    )

                                    let newHyps = []
                                    mandHyps->Js.Array2.forEach(hyp => {
                                        if (preCtx->getHypByExpr(hyp.expr)->Belt.Option.isNone) {
                                            newHyps->Js.Array2.push(hyp)->ignore
                                        }
                                    })
                                    let varsUsedInProof = Belt_HashSetInt.make(~hintSize=16)
                                    exprsUsedInProof->Belt_HashSet.forEach(expr => {
                                        expr->Js_array2.forEach(s => {
                                            if (s >= 0) {
                                                varsUsedInProof->Belt_HashSetInt.add(s)
                                            }
                                        })
                                    })
                                    let mandVars = mandHyps->Js.Array2.map(hyp => hyp.expr[1])->Belt_HashSetInt.fromArray
                                    wrkCtx->getLocalHyps->Js.Array2.forEach(hyp => {
                                        if (hyp.typ == F) {
                                            let var = hyp.expr[1]
                                            if (varsUsedInProof->Belt_HashSetInt.has(var) 
                                                && !(mandVars->Belt_HashSetInt.has(var))) {
                                                newHyps->Js.Array2.push(hyp)->ignore
                                            }
                                        }
                                    })

                                    let newDisj = disjMake()
                                    MM_proof_verifier.verifyProof(
                                        ~ctx=proofCtx,
                                        ~expr,
                                        ~proof,
                                        ~isDisjInCtx = (n,m) => {
                                            if (!(wrkCtx->isDisj(n,m))) {
                                                raise(MmException({msg:`!(wrkCtx->isDisj(n,m))`}))
                                            } else {
                                                if (!(preCtx->isDisj(n,m))) {
                                                    newDisj->disjAddPair(n,m)
                                                }
                                                true
                                            }
                                        }
                                    )->ignore
                                    
                                    Some((
                                        proofToText( 
                                            ~wrkCtx=wrkCtx, ~typeOrderInDisj=st.typeOrderInDisj,
                                            ~newHyps, ~newDisj, ~descr=st.descr, ~stmt, ~proof 
                                        ),
                                        MM_proof_table.proofTableToStr(wrkCtx, proofTableWithTypes, stmt.label),
                                        MM_proof_table.proofTableToStr(wrkCtx, proofTableWithoutTypes, stmt.label),
                                    ))
                                }
                            }
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
                    if (stmt.typ == E) {
                        Ok(st)
                    } else {
                        switch parseJstf(stmt.jstfText) {
                            | Error(_) => Error(`Cannot parse justification '${stmt.jstfText}' ` 
                                                    ++ `for the step '${stmt.label}'`)
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
            }
        },
        Ok(st)
    )
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->Js_string2.replaceByRe(symbolsNotAllowedInLabelRegex, "")

let renameStmt = (st:editorState, stmtId:stmtId, newLabel:string):result<editorState,string> => {
    let newLabel = newLabel->removeSymbolsNotAllowedInLabel
    if (newLabel == "") {
        Error(`label must not be empty.`)
    } else {
        switch st.stmts->Js_array2.find(stmt => stmt.id != stmtId && stmt.label == newLabel) {
            | Some(_) => Error(`label '${newLabel}' is used by another step.`)
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

let mergeStmts = (st:editorState,idToUse:stmtId,idToDelete:stmtId):result<editorState,string> => {
    switch st->editorGetStmtById(idToUse) {
        | None => Error(`Cannot find a step with id = '${idToUse}'`)
        | Some(stmtToUse) => {
            switch st->editorGetStmtById(idToDelete) {
                | None => Error(`Cannot find a step with id = '${idToDelete}'`)
                | Some(stmtToDelete) => {
                    if (stmtToUse.cont->contToStr != stmtToDelete.cont->contToStr) {
                        Error(`Steps to merge must have identical expressions.`)
                    } else {
                        switch replaceRef(st, ~replaceWhat=stmtToDelete.label, ~replaceWith=stmtToUse.label) {
                            | Error(msg) => Error(msg)
                            | Ok(st) => {
                                let st = st->deleteStmt(idToDelete)
                                if (stmtToDelete.typ == P && stmtToDelete.isGoal && stmtToUse.typ != E) {
                                    let st = st->updateStmt(idToUse, stmt => {...stmt, typ:P, isGoal:true})
                                    st->renameStmt(idToUse, stmtToDelete.label)
                                } else {
                                    Ok(st)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let onlyDigitsPattern = %re("/^\d+$/")

let containsOnlyDigits = (label:string):bool => label->Js_string2.match_(onlyDigitsPattern)->Belt_Option.isSome

let renameHypToMatchGoal = (st:editorState, oldStmt:userStmt, newStmt:userStmt):editorState => {
    if (oldStmt.id != newStmt.id) {
        raise(MmException({msg:`renameHypToMatchGoal: oldStmt.id != newStmt.id`}))
    }
    let stmtId = oldStmt.id
    if (oldStmt.typ == P && newStmt.typ == E && newStmt.label->containsOnlyDigits) {
        let newLabel = 
            if (
                st.stmts->Js.Array2.find(stmt => stmt.isGoal)->Belt.Option.isSome
                || st.preCtx->getTokenType(newStmt.label)->Belt.Option.isSome
            ) {
                createNewLabel(st, ~forHyp=true, ())
            } else {
                newStmt.label
            }
        if (newLabel == newStmt.label) {
            st
        } else {
            switch st->renameStmt(stmtId, newLabel) {
                | Error(_) => st
                | Ok(st) => st
            }
        }
    } else {
        st
    }
}

let completeTypEditMode = (st, stmtId, newTyp, newIsGoal) => {
    let oldStmt = st->editorGetStmtByIdExn(stmtId)
    let st = updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            typ:newTyp,
            isGoal: 
                switch newTyp {
                    | E => false
                    | P => newIsGoal
                },
            typEditMode: false
        }
    })
    let newStmt = st->editorGetStmtByIdExn(stmtId)
    renameHypToMatchGoal(st, oldStmt, newStmt)
}

let defaultJstfForHyp = "HYP"

let completeJstfEditMode = (st, stmtId, newJstfInp):editorState => {
    let oldStmt = st->editorGetStmtByIdExn(stmtId)
    let st = updateStmt(st, stmtId, stmt => {
        let jstfTrimUpperCase = newJstfInp->Js.String2.trim->Js.String2.toLocaleUpperCase
        let newTyp = if (jstfTrimUpperCase == defaultJstfForHyp) {E} else {P}
        let newJstf = if (jstfTrimUpperCase == defaultJstfForHyp) {""} else {newJstfInp->Js.String2.trim}

        let pCnt = st.stmts->Js.Array2.reduce(
            (cnt,stmt) => {
                if (stmt.id != stmtId && stmt.typ == P) {
                    cnt + 1
                } else {
                    cnt
                }
            },
            0
        )
        
        let newIsGoal = if (newTyp == E) { false } else { stmt.isGoal || st.settings.initStmtIsGoal && pCnt == 0 }
        let newLabel = if (newIsGoal && !stmt.isGoal && st.settings.defaultStmtLabel->Js.String2.length > 0) {
            st.settings.defaultStmtLabel
        } else { 
            stmt.label
        }
        
        {
            ...stmt,
            typ: newTyp,
            isGoal: newIsGoal,
            label:newLabel,
            jstfText:newJstf,
            jstfEditMode: false,
        }
    })
    let newStmt = st->editorGetStmtByIdExn(stmtId)
    renameHypToMatchGoal(st, oldStmt, newStmt)
}

let findStmtsToMerge = (st:editorState):result<(userStmt,userStmt),string> => {
    let stmt1 = if (st.checkedStmtIds->Js.Array2.length == 0) {
        st.stmts->Js_array2.find(stmt => {
            stmt.stmtErr->Belt_Option.map(err => err.code == duplicatedStmtErrCode)->Belt.Option.getWithDefault(false)
        })
    } else {
        let (checkedStmtId,_) = st.checkedStmtIds[0]
        st->editorGetStmtById(checkedStmtId)
    }
    switch stmt1 {
        | None => Error("[1] Cannot determine a duplicated step.")
        | Some(stmt1) => {
            let contStr = stmt1.cont->contToStr
            switch st.stmts->Js.Array2.find(stmt => stmt.id != stmt1.id && stmt.cont->contToStr == contStr) {
                | None => Error("[2] Cannot find another step to merge with.")
                | Some(stmt2) => {
                    let idx1 = st.stmts->Js.Array2.findIndex(stmt => stmt.id == stmt1.id)
                    let idx2 = st.stmts->Js.Array2.findIndex(stmt => stmt.id == stmt2.id)
                    if (idx1 < idx2) {
                        Ok((stmt1, stmt2))
                    } else {
                        Ok((stmt2, stmt1))
                    }
                }
            }
        }
    }
}

let findFirstDuplicatedStmt = (st:editorState):option<userStmt> => {
    st.stmts->Js_array2.find(stmt => 
        stmt.typ != E
        && !stmt.isDuplicated
        && stmt.stmtErr->Belt_Option.map(err => err.code == duplicatedStmtErrCode)
            ->Belt_Option.getWithDefault(false)
    )
}

let findSecondDuplicatedStmt = (st:editorState, stmt1:userStmt):option<userStmt> => {
    let contStr = stmt1.cont->contToStr
    st.stmts->Js.Array2.find(stmt2 => {
        stmt2.typ != E && !stmt2.isDuplicated && stmt2.id != stmt1.id && stmt2.cont->contToStr == contStr
    })
}

let autoMergeDuplicatedStatements = (st:editorState):editorState => {
    let resultState = ref(st)
    let continue = ref(true)
    while (continue.contents) {
        switch resultState.contents->findFirstDuplicatedStmt {
            | None => continue := false
            | Some(stmt1) => {
                switch resultState.contents->findSecondDuplicatedStmt(stmt1) {
                    | None => continue := false
                    | Some(stmt2) => {
                        let jstf1 = stmt1.jstfText->Js_string2.trim
                        let jstf2 = stmt2.jstfText->Js_string2.trim
                        if (jstf1 != "" && jstf2 == "") {
                            switch resultState.contents->mergeStmts(stmt1.id, stmt2.id) {
                                | Error(msg) => {
                                    Js.Console.log2(`err1 msg`, msg)
                                    continue := false
                                }
                                | Ok(stateAfterMerge) => resultState := stateAfterMerge->prepareEditorForUnification
                            }
                        } else if (jstf2 != "" && (jstf1 == "" || jstf1 == jstf2)) {
                            switch resultState.contents->mergeStmts(stmt2.id, stmt1.id) {
                                | Error(msg) => {
                                    Js.Console.log2(`err2 msg`, msg)
                                    continue := false
                                }
                                | Ok(stateAfterMerge) => resultState := stateAfterMerge->prepareEditorForUnification
                            }
                        } else {
                            continue := false
                        }
                    }
                }
            }
        }
    }
    resultState.contents
}

let updateEditorStateWithPostupdateActions = (st, update:editorState=>editorState) => {
    let st = update(st)
    let st = prepareEditorForUnification(st)
    if (st.wrkCtx->Belt_Option.isSome) {
        let st = removeUnusedVars(st)
        let st = if (st.settings.autoMergeStmts) {
            autoMergeDuplicatedStatements(st)
        } else {
            st
        }
        let st = prepareEditorForUnification(st)
        st
    } else {
        st
    }
}

let getSelectedSubtree = (treeData:stmtContTreeData):option<childNode> => {
    switch treeData.clickedNodeId {
        | None => None
        | Some((nodeId,_)) => {
            switch treeData.root->getNodeById(nodeId) {
                | None => None
                | Some(Subtree(_)) => None //this should never happen because a Subtree cannot be clicked
                | Some(Symbol({id, parent, symInt, sym, color, isVar})) => {
                    if (treeData.expLvl == 0) {
                        Some(Symbol({id, parent, symInt, sym, color, isVar}))
                    } else {
                        let curParent = ref(Some(parent))
                        let curLvl = ref(treeData.expLvl)
                        while (curLvl.contents > 1 && curParent.contents->Belt_Option.isSome) {
                            curLvl := curLvl.contents - 1
                            curParent := (curParent.contents->Belt_Option.getExn).parent
                        }
                        switch curParent.contents {
                            | Some(parent) => Some(Subtree(parent))
                            | None => Some(Subtree(treeData.root))
                        }
                    }
                }
            }
        }
    }
}

let getStmtContTreeData = (stmtCont:stmtCont):option<stmtContTreeData> => {
    switch stmtCont {
        | Text(_) => None
        | Tree(treeData) => Some(treeData)
    }
}

let getSelectedSubtreeFromStmtCont = (stmtCont:stmtCont):option<childNode> => {
    stmtCont->getStmtContTreeData->Belt_Option.flatMap(getSelectedSubtree)
}

let getNumberOfSelectedSymbols = (treeData:stmtContTreeData):int => {
    treeData->getSelectedSubtree->Belt.Option.map(syntaxTreeGetNumberOfSymbols)->Belt.Option.getWithDefault(0)
}

let getIdsOfSelectedNodes = (stmtCont:stmtCont):(int,Belt_SetInt.t) => {
    switch stmtCont {
        | Text(_) => (-1,Belt_SetInt.empty)
        | Tree(treeData) => {
            switch treeData.clickedNodeId {
                | None => (-1,Belt_SetInt.empty)
                | Some((nodeId,_)) => {
                    switch getSelectedSubtree(treeData) {
                        | None => (-1,Belt_SetInt.empty)
                        | Some(selectedSubtree) => (nodeId,syntaxTreeGetIdsOfAllChildSymbols(selectedSubtree))
                    }
                }
            }
        }
    }
}

let getNodeIdBySymIdx = (
    ~symIdx:int,
    ~tree:syntaxTreeNode,
):option<int> => {
    let cnt = ref(0)
    syntaxTreeForEachNode(Subtree(tree), node => {
        switch node {
            | Subtree(_) => None
            | Symbol({id}) => {
                cnt := cnt.contents + 1
                if (cnt.contents == symIdx) {
                    Some(id)
                } else {
                    None
                }
            }
        }
    })
}

let hasSelectedText = (stmtCont:stmtCont):bool => {
    switch stmtCont {
        | Text(_) | Tree({clickedNodeId:None}) => false
        | Tree({clickedNodeId:Some(_)}) => true
    }
}

let getSelectedText = (stmtCont:stmtCont):option<string> => {
    switch stmtCont {
        | Text(_) => None
        | Tree(treeData) => treeData->getSelectedSubtree->Belt.Option.map(syntaxTreeToText)
    }
}

let incExpLvl = (treeData:stmtContTreeData):stmtContTreeData => {
    {
        ...treeData, 
        expLvl: Js_math.min_int(treeData.expLvl + 1, treeData.root.height)
    }
}

let decExpLvl = (treeData:stmtContTreeData):stmtContTreeData => {
    {
        ...treeData, 
        expLvl: Js_math.max_int(treeData.expLvl - 1, 0)
    }
}

let getAllExprsToSyntaxCheck = (st:editorState, rootStmts:array<rootStmt>):array<expr> => {
    let res = []
    st.stmts->Js.Array2.forEachi((stmt,i) => {
        switch stmt.cont {
            | Tree(_) => ()
            | Text(_) => res->Js.Array2.push(rootStmts[i].expr->Js_array2.sliceFrom(1))->ignore
        }
    })
    res
}

let updateExpLevel = (treeData:stmtContTreeData, inc:bool):stmtContTreeData => {
    let update = if (inc) {incExpLvl} else {decExpLvl}
    let prevTreeData = ref(treeData)
    let prevNum = ref(getNumberOfSelectedSymbols(prevTreeData.contents))
    let newTreeData = ref(update(prevTreeData.contents))
    let newNum = ref(getNumberOfSelectedSymbols(newTreeData.contents))
    while (
        prevNum.contents == newNum.contents
        && (
            inc && newTreeData.contents.expLvl < newTreeData.contents.root.height
            || !inc && newTreeData.contents.expLvl > 0
        )
    ) {
        prevTreeData := newTreeData.contents
        prevNum := getNumberOfSelectedSymbols(prevTreeData.contents)
        newTreeData := update(prevTreeData.contents)
        newNum := getNumberOfSelectedSymbols(newTreeData.contents)
    }
    newTreeData.contents
}

let incExpLvlIfConstClicked = (treeData:stmtContTreeData):stmtContTreeData => {
    if (treeData.expLvl == 0) {
        switch treeData.clickedNodeId {
            | None => treeData
            | Some((clickedNodeId,_)) => {
                switch treeData.root->getNodeById(clickedNodeId) {
                    | None => treeData
                    | Some(Symbol({parent})) => {
                        if (syntaxTreeGetNumberOfSymbols(Subtree(parent)) == 1) {
                            /* if size == 1 then the clicked symbol is a variable in the syntax definition */
                            treeData
                        } else {
                            treeData->updateExpLevel(true)
                        }
                    }
                    | Some(Subtree(_)) => treeData
                }
            }
        }
    } else {
        treeData
    }
}

let renumberSteps = (state:editorState, ~isStmtToRenumber:userStmt=>bool, ~prefix:string, ~forHyp:bool):result<editorState, string> => {
    let state = state->prepareEditorForUnification
    if (state->editorStateHasErrors) {
        Error(
            `Cannot perform renumbering because there is an error in the editor content.`
                ++ ` Please resolve the error before renumbering.`
        )
    } else {
        let tmpPrefix = "###tmp###"
        let idsToRenumberArr = state.stmts
            ->Js.Array2.filter(isStmtToRenumber)
            ->Js.Array2.map(stmt => stmt.id)
        let idsToRenumberSet = idsToRenumberArr->Belt_HashSetString.fromArray

        //step 1: assign temporary labels to all the renumberable statements in order to make all numeric labels not used
        let res = state.stmts->Js.Array2.reduce(
            (res,stmt) => {
                switch res {
                    | Ok(st) => {
                        if (idsToRenumberSet->Belt_HashSetString.has(stmt.id)) {
                            st->renameStmt(stmt.id, tmpPrefix ++ stmt.label)
                        } else {
                            Ok(st)
                        }
                    }
                    | err => err
                }
            },
            Ok(state)
        )

        //step 2: assign final labels to each renumberable statement
        let res = idsToRenumberArr->Js.Array2.reduce(
            (res,stmtId) => {
                switch res {
                    | Ok(st) => st->renameStmt(stmtId, st->createNewLabel(~prefix, ~forHyp, ()))
                    | err => err
                }
            },
            res
        )

        switch res {
            | Error(_) => res
            | Ok(state) => {
                let state = state->prepareEditorForUnification
                if (state->editorStateHasErrors) {
                    Error( `Cannot renumber steps: there was an internal error during renumbering.` )
                } else {
                    Ok(state)
                }
            }
        }
    }
}

let renumberProvableSteps = (state:editorState):result<editorState, string> => {
    state->renumberSteps(
        ~isStmtToRenumber = stmt => stmt.typ == P && stmt.label->containsOnlyDigits,
        ~prefix="",
        ~forHyp=false,
    )
}

let renumberHypothesisSteps = (state:editorState, ~goalLabel:string):result<editorState, string> => {
    state->renumberSteps(
        ~isStmtToRenumber = stmt => stmt.typ == E,
        ~prefix=goalLabel ++ ".",
        ~forHyp=true,
    )
}

let textToSyntaxProofTable = (
    ~wrkCtx:mmContext,
    ~syms:array<array<string>>,
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
):result<array<result<MM_proof_table.proofTable,string>>,string> => {
    if (syntaxTypes->Js_array2.length == 0) {
        Error(`Could not determine syntax types.`)
    } else {
        let findUndefinedSym = (syms:array<string>):option<string> => 
            syms->Js.Array2.find(sym => wrkCtx->ctxSymToInt(sym)->Belt_Option.isNone)
        switch Belt_Array.concatMany(syms)->findUndefinedSym {
            | Some(unrecognizedSymbol) => Error(`Unrecognized symbol: '${unrecognizedSymbol}'`)
            | None => {
                let lastSyntaxTypeInt = lastSyntaxType->Belt.Option.flatMap(wrkCtx->ctxSymToInt)->Belt.Option.getWithDefault(0)
                let syntaxTypes = syntaxTypes->Js.Array2.copy->Js.Array2.sortInPlaceWith((a,b) => {
                    if (a == lastSyntaxTypeInt) {
                        -1
                    } else if (b == lastSyntaxTypeInt) {
                        1
                    } else {
                        a - b
                    }
                })
                let exprs = syms->Js_array2.map(wrkCtx->ctxSymsToIntsExn)
                let proofTree = MM_provers.proveSyntaxTypes(
                    ~wrkCtx=wrkCtx, ~frms, ~parenCnt, ~exprs, ~syntaxTypes, ~frameRestrict, ()
                )
                let typeStmts = exprs->Js.Array2.map(expr => {
                    switch proofTree->ptGetSyntaxProof(expr) {
                        | None => None
                        | Some(node) => Some(node->pnGetExpr)
                    }
                })
                let proofTreeDto = proofTree->MM_proof_tree_dto.proofTreeToDto(
                    typeStmts->Js_array2.filter(Belt_Option.isSome)->Js_array2.map(Belt_Option.getExn)
                )
                switch typeStmts->Js_array2.find(Belt_Option.isSome) {
                    | None => ()
                    | Some(None) => ()
                    | Some(Some(typeStmt)) => {
                        switch lastSyntaxType {
                            | None => wrkCtx->ctxIntToSym(typeStmt[0])->Belt_Option.forEach(onLastSyntaxTypeChange)
                            | Some(lastSyntaxType) => {
                                wrkCtx->ctxIntToSym(typeStmt[0])->Belt_Option.forEach(provedSyntaxType => {
                                    if (lastSyntaxType != provedSyntaxType) {
                                        onLastSyntaxTypeChange(provedSyntaxType)
                                    }
                                })
                            }
                        }
                    }
                }
                Ok(
                    typeStmts->Js.Array2.map(typeStmt => {
                        switch typeStmt {
                            | None => {
                                Error(
                                    `Could not prove this statement is of any of the types: ` 
                                        ++ `${wrkCtx->ctxIntsToSymsExn(syntaxTypes)->Js.Array2.joinWith(", ")}`
                                )
                            }
                            | Some(typeStmt) => {
                                buildSyntaxProofTableFromProofTreeDto( ~ctx=wrkCtx, ~proofTreeDto, ~typeStmt, )
                            }
                        }
                    })
                )
            }
        }
    }
}

let textToSyntaxTree = (
    ~wrkCtx:mmContext,
    ~syms:array<array<string>>,
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
):result<array<result<syntaxTreeNode,string>>,string> => {
    let syntaxProofTables = textToSyntaxProofTable(
        ~wrkCtx,
        ~syms,
        ~syntaxTypes,
        ~frms,
        ~frameRestrict,
        ~parenCnt,
        ~lastSyntaxType,
        ~onLastSyntaxTypeChange,
    )
    switch syntaxProofTables {
        | Error(msg) => Error(msg)
        | Ok(proofTables) => {
            Ok(
                proofTables->Js_array2.map(proofTable => {
                    switch proofTable {
                        | Error(msg) => Error(msg)
                        | Ok(proofTable) => buildSyntaxTree(wrkCtx, proofTable, proofTable->Js_array2.length-1)
                    }
                })
            )
        }
    }
}

let resetEditorContent = (st:editorState):editorState => {
    {
        ...st,

        descr: "",
        descrEditMode: false,

        varsText: "",
        varsEditMode: false,

        disjText: "",
        disjEditMode: false,

        stmts: [],
        checkedStmtIds: [],
    }
}

let deleteUnrelatedSteps = (state:editorState, ~stepIdsToKeep:array<stmtId>):result<editorState, string> => {
    let state = state->prepareEditorForUnification
    if (state->editorStateHasErrors) {
        Error(
            `Cannot perform deletion because there is an error in the editor content.`
                ++ ` Please resolve the error before deleting steps.`
        )
    } else {
        let unprocessedIds = Belt.MutableQueue.fromArray(stepIdsToKeep)
        state.stmts->Js.Array2.forEach(stmt => {
            if (stmt.typ == E || stmt.isGoal) {
                unprocessedIds->Belt_MutableQueue.add(stmt.id)
            }
        })
        let idsToKeep = Belt_HashSetString.make(~hintSize = stepIdsToKeep->Js_array2.length*5)
        while (!(unprocessedIds->Belt_MutableQueue.isEmpty)) {
            let idToKeep = unprocessedIds->Belt.MutableQueue.pop->Belt.Option.getExn
            if (!(idsToKeep->Belt_HashSetString.has(idToKeep))) {
                idsToKeep->Belt_HashSetString.add(idToKeep)
                switch state->editorGetStmtById(idToKeep) {
                    | None => ()
                    | Some(stmtToProcess) => {
                        switch stmtToProcess.jstf {
                            | None => ()
                            | Some({args}) => {
                                args->Js.Array2.forEach(label => {
                                    switch state->editorGetStmtByLabel(label) {
                                        | None => ()
                                        | Some(stmt) => unprocessedIds->Belt_MutableQueue.add(stmt.id)
                                    }
                                })
                            }
                        }
                    }
                }
            }
        }
        let idsToRemove = state.stmts
            ->Js_array2.map(stmt => stmt.id)
            ->Js.Array2.filter(id => !(idsToKeep->Belt_HashSetString.has(id)))
        Ok(state->deleteStmts(idsToRemove))
    }
}