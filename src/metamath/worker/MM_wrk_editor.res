open MM_context
open MM_parser
open MM_proof_tree
open MM_proof_tree_dto
open MM_syntax_tree
open MM_wrk_syntax_tree
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
    clickedNodeId:option<(int,Date.t)>,
    expLvl:int
}

type stmtCont =
    | Text({text:string, syms:array<stmtSym>})
    | Tree(stmtContTreeData)

let contIsEmpty = cont => {
    switch cont {
        | Text({text}) | Tree({text}) => text->String.length == 0
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text({text}) | Tree({text}) => getSpaceSeparatedValuesAsArray(text)
    }
}

let contToStr = (cont:stmtCont):string => {
    switch cont {
        | Text({text}) | Tree({text}) => text
    }
}

let strToCont = (
    str:string,
    ~preCtxColors: option<Belt_HashMapString.t<string>>=?,
    ~wrkCtxColors: option<Belt_HashMapString.t<string>>=?
):stmtCont => {
    let symsArr = getSpaceSeparatedValuesAsArray(str)
    Text({
        text: symsArr->Array.joinUnsafe(" "),
        syms: symsArr->Array.map(sym => {
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
type userStmtTypeExtended = H | P | G

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

let userStmtTypeExtendedFromStrExn = (str:string):userStmtTypeExtended => {
    switch str {
        | "h" => H
        | "p" => P
        | "g" => G
        | _ => raise(MmException({msg:`Cannot convert '${str}' to userStmtTypeExtended. Possible values are h, p, g.`}))
    }
}

let userStmtTypeExtendedToUserStmtType = (typExt:userStmtTypeExtended):(userStmtType,bool) => {
    switch typExt {
        | H => (E,false)
        | P => (P,false)
        | G => (P,true)
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
    isBkm: bool,
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

type userStmtDtoOpt = {
    id: option<stmtId>,
    label: option<string>,
    typ: option<userStmtTypeExtended>,
    cont: option<string>,
    jstf: option<string>,
    isBkm: option<bool>,
}

type editorStateAction = 
    | UnifyAll({nextAction:unit=>unit})
    | MergeNextDuplicate
    | Action(unit=>unit)

type editorState = {
    preCtxData:preCtxData,

    tabTitle: string,

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
    checkedStmtIds: array<(stmtId,Date.t)>,

    nextAction:option<editorStateAction>,
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
        isBkm:false,
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

let editorGetStmtById = (st:editorState,id:stmtId):option<userStmt> => st.stmts->Array.find(stmt => stmt.id == id)
let editorGetStmtByLabel = (st:editorState,label:string):option<userStmt> => {
    st.stmts->Array.find(stmt => stmt.label == label)
}

let editorGetStmtByIdExn = (st:editorState,id:stmtId):userStmt => {
    switch editorGetStmtById(st,id) {
        | None => raise(MmException({msg:`editorGetStmtByIdExn: Cannot find a step by id.`}))
        | Some(stmt) => stmt
    }
}

let getStmtIdx = (st:editorState,id:stmtId):int => {
    st.stmts->Array.findIndex(stmt => stmt.id == id)
}

let updateStmt = (st:editorState,id,update):editorState => {
    {
        ...st,
        stmts: st.stmts->Array.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let isStmtChecked = (st:editorState,stmtId:stmtId):bool => {
    st.checkedStmtIds->Array.some(((id,_)) => id == stmtId)
}

let toggleStmtChecked = (st:editorState, stmtId:stmtId) => {
    if (isStmtChecked(st,stmtId)) {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Array.filter(((checkedId,_)) => checkedId != stmtId)
        }
    } else {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Array.concat([(stmtId,Date.make())])
        }
    }
}

let getStmtIdsBetweenIncluding = (stmtIds:array<stmtId>, stmtId1:stmtId, stmtId2:stmtId):array<stmtId> => {
    let (_,res) = stmtIds->Array.reduce((false,[]), ((inBetween,res),stmtId) => {
        if (stmtId == stmtId1 || stmtId == stmtId2) {
            res->Array.push(stmtId)
            (!inBetween,res)
        } else {
            if (inBetween) {
                res->Array.push(stmtId)
            }
            (inBetween,res)
        }
    })
    res
}

let toggleStmtCheckedWithShift = (st:editorState, stmtId:stmtId, ~showBkmOnly:bool) => {
    if (isStmtChecked(st,stmtId)) {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Array.filter(((checkedId,_)) => checkedId != stmtId)
        }
    } else {
        let visibleStmtIds = st.stmts
                ->Array.filter(stmt => !showBkmOnly || stmt.isBkm)
                ->Array.map(stmt => stmt.id)
        let lastCheckedStmtId = st.checkedStmtIds->Array.toSorted(((_,date1),(_,date2)) => {
            let t1:float = date1->Date.getTime
            let t2:float = date2->Date.getTime
            //sort desc
            if (t1 < t2) {
                1.0
            } else if (t1 > t2) {
                -1.0
            } else {
                0.0
            }
        })->Array.at(0)
        let stmtIdsToCheck = switch lastCheckedStmtId {
            | None => [stmtId]
            | Some((lastCheckedStmtId,_)) => getStmtIdsBetweenIncluding(visibleStmtIds, lastCheckedStmtId, stmtId)
        }
        //To avoid having duplicated ids in checkedStmtIds, first uncheck all stmts which will be checked
        let st = {
            ...st,
            checkedStmtIds: st.checkedStmtIds
                ->Array.filter(((checkedId,_)) => !(stmtIdsToCheck->Array.includes(checkedId)))
        }
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Array.concat(
                stmtIdsToCheck->Array.map(id=>(id,Date.make()))
            )
        }
    }
}

let checkAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: st.stmts->Array.map(stmt => (stmt.id, Date.make()))
    }
}

let uncheckAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: [],
        stmts: st.stmts->Array.map(unselectStmt)
    }
}

let deleteCheckedStmts = (st:editorState):editorState => {
    {
        ...st,
        stmts: st.stmts->Array.filter(stmt => !isStmtChecked(st,stmt.id)),
        checkedStmtIds: [],
    }
}

let deleteStmt = (st:editorState, stmtId:stmtId):editorState => {
    {
        ...st,
        stmts: st.stmts->Array.filter(stmt => stmt.id != stmtId),
        checkedStmtIds: st.checkedStmtIds->Array.filter(((checkedId,_)) => checkedId != stmtId),
    }
}

let deleteStmts = (st:editorState, stmtIds:array<stmtId>):editorState => {
    {
        ...st,
        stmts: st.stmts->Array.filter(stmt => !(stmtIds->Array.includes(stmt.id))),
        checkedStmtIds: st.checkedStmtIds->Array.filter(((checkedId,_)) => {
            !(stmtIds->Array.includes(checkedId))
        }),
    }
}

let canMoveCheckedStmts = (st:editorState, up):bool => {
    let len = st.stmts->Array.length
    len != 0 && st.checkedStmtIds->Array.length != 0 && (
        (up && !isStmtChecked(st,(st.stmts->Array.getUnsafe(0)).id)) || (!up && !isStmtChecked(st,(st.stmts->Array.getUnsafe(len-1)).id))
    )
}

let moveCheckedStmts = (st:editorState,up):editorState => {
    if (!canMoveCheckedStmts(st,up)) {
        st
    } else {
        let len = st.stmts->Array.length
        let res = st.stmts->Array.copy
        if up {
            let maxI = len-2
            for i in 0 to maxI {
                if (!isStmtChecked(st,(res->Array.getUnsafe(i)).id) && isStmtChecked(st,(res->Array.getUnsafe(i+1)).id)) {
                    let tmp = res->Array.getUnsafe(i)
                    res[i] = res->Array.getUnsafe(i+1)
                    res[i+1] = tmp
                }
            }
        } else {
            for i in len-1 downto 1 {
                if (isStmtChecked(st,(res->Array.getUnsafe(i-1)).id) && !isStmtChecked(st,(res->Array.getUnsafe(i)).id)) {
                    let tmp = res->Array.getUnsafe(i)
                    res[i] = res->Array.getUnsafe(i-1)
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

let moveCheckedStmtsToTop = (st:editorState):editorState => {
    let checkedStmts = st.stmts->Array.filter(stmt => st->isStmtChecked(stmt.id))
    let uncheckedStmts = st.stmts->Array.filter(stmt => !(st->isStmtChecked(stmt.id)))
    {
        ...st,
        stmts: Belt_Array.concatMany([checkedStmts, uncheckedStmts])
    }
}

let moveCheckedStmtsToBottom = (st:editorState):editorState => {
    let checkedStmts = st.stmts->Array.filter(stmt => st->isStmtChecked(stmt.id))
    let uncheckedStmts = st.stmts->Array.filter(stmt => !(st->isStmtChecked(stmt.id)))
    {
        ...st,
        stmts: Belt_Array.concatMany([uncheckedStmts, checkedStmts])
    }
}

let getRootStmtsForUnification = (st):array<userStmt> => {
    let checkedStmtIds = st.checkedStmtIds->Array.map(((stmtId,_)) => stmtId)->Belt_HashSetString.fromArray
    if (checkedStmtIds->Belt_HashSetString.size == 0) {
        st.stmts
    } else {
        let lowestCheckedStmtIdx = ref(None)
        let i = ref(st.stmts->Array.length-1)
        while (i.contents >= 0 && lowestCheckedStmtIdx.contents->Belt_Option.isNone) {
            let stmt = st.stmts->Array.getUnsafe(i.contents)
            if (checkedStmtIds->Belt_HashSetString.has(stmt.id)) {
                lowestCheckedStmtIdx := Some(i.contents)
            }
            i := i.contents - 1
        }
        switch lowestCheckedStmtIdx.contents {
            | None => st.stmts
            | Some(idx) => st.stmts->Array.slice(~start=0, ~end=idx+1)
        }
    }
}

let createNewLabel = (st:editorState, ~prefix:option<string>=?, ~forHyp:bool=false):string => {
    let reservedLabels = Belt_HashSetString.fromArray(st.stmts->Array.map(stmt=>stmt.label))
    switch textToVarDefs(st.varsText) {
        | Error(_) => ()
        | Ok(varDefs) => {
            varDefs->Array.forEach(varDef => {
                reservedLabels->Belt_HashSetString.add(varDef->Array.getUnsafe(0))
            })
        }
    }

    let preCtx = st.preCtxData.ctxV.val.min
    let labelIsReserved = (label:string):bool => {
        reservedLabels->Belt_HashSetString.has(label) 
            || preCtx->isHyp(label) 
            || forHyp && preCtx->getTokenType(label)->Belt.Option.isSome
    }

    let prefixToUse = switch prefix {
        | Some(prefix) => prefix
        | None => {
            if (forHyp) {
                switch st.stmts->Array.find(stmt => stmt.isGoal) {
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
    if (st.checkedStmtIds->Array.length == 0) {
        None
    } else {
        st.stmts->Array.find(stmt => isStmtChecked(st,stmt.id))
    }
}

let getLowestCheckedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Array.length == 0) {
        None
    } else {
        st.stmts->Array.toReversed->Array.find(stmt => isStmtChecked(st,stmt.id))
    }
}

let addNewStmt = (st:editorState, ~isHyp:bool=false, ~isBkm:option<bool>=?):(editorState,stmtId) => {
    let newId = st.nextStmtId->Belt_Int.toString
    let pCnt = st.stmts->Array.reduce(
        0,
        (cnt,stmt) => if (stmt.typ == P) {cnt + 1} else {cnt}
    )
    let defaultStmtLabel = st.preCtxData.settingsV.val.defaultStmtLabel->String.trim
    let newLabel = 
        if (pCnt == 0 && defaultStmtLabel->String.length > 0) {
            if (st.stmts->Array.some(stmt => stmt.label == defaultStmtLabel)) {
                createNewLabel(st, ~prefix=defaultStmtLabel, ~forHyp=isHyp)
            } else {
                defaultStmtLabel
            }
        } else {
            createNewLabel(st, ~prefix="", ~forHyp=isHyp)
        }
    let isGoal = pCnt == 0 && st.preCtxData.settingsV.val.initStmtIsGoal
    let idToAddBefore = getTopmostCheckedStmt(st)->Belt_Option.map(stmt => stmt.id)
    (
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                switch idToAddBefore {
                    | Some(idToAddBefore) => {
                        st.stmts->Array.map(stmt => {
                            if (stmt.id == idToAddBefore) {
                                [
                                    {
                                        ...createEmptyUserStmt(newId,P,newLabel,isGoal), 
                                        isBkm:isBkm->Belt_Option.getWithDefault(stmt.isBkm)
                                    }, 
                                    stmt
                                ]
                            } else {
                                [stmt]
                            }
                        })->Belt_Array.concatMany
                    }
                    | None => {
                        st.stmts->Array.concat([
                            {
                                ...createEmptyUserStmt(newId, P, newLabel, isGoal), 
                                isBkm:isBkm->Belt_Option.getWithDefault(false)
                            }
                        ])
                    }
                }
        },
        newId
    )
}

let addNewStmtAtIdx = (st:editorState, ~idx:int, ~isHyp:bool=false):(editorState,stmtId) => {
    let savedCheckedStmtIds = st.checkedStmtIds
    let st = st->uncheckAllStmts
    let st = if (0 <= idx && idx < st.stmts->Array.length) {
        st->toggleStmtChecked((st.stmts->Array.getUnsafe(idx)).id)
    } else {
        st
    }
    let (st,stmtId) = st->addNewStmt(~isHyp)
    let st = {...st, checkedStmtIds:savedCheckedStmtIds}
    (st,stmtId)
}

let isSingleStmtChecked = st => st.checkedStmtIds->Array.length == 1

let duplicateCheckedStmt = (st:editorState, top:bool) => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let (idToAddAfter,_) = st.checkedStmtIds->Array.getUnsafe(0)
        let st = {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                st.stmts->Array.map(stmt => {
                    if (stmt.id == idToAddAfter) {
                        [
                            stmt, 
                            {
                                ...stmt, 
                                id:newId, 
                                label:createNewLabel(st, ~forHyp = stmt.typ == E),
                                isGoal:false, 
                                jstfText:"",
                                isDuplicated:true,
                            }
                        ]
                    } else {
                        [stmt]
                    }
                })->Belt_Array.concatMany,
            checkedStmtIds: [(newId,Date.make())],
        }
        if (top) {
            st->moveCheckedStmts(true)
        } else {
            st
        }
    }
}

let bookmarkCheckedStmts = (st:editorState, isBkm:bool):editorState => {
    let checkedStmtIds = st.checkedStmtIds->Array.map(((stmtId,_)) => stmtId)
    {
        ...st,
        stmts: st.stmts->Array.map(stmt => {
            if (checkedStmtIds->Array.includes(stmt.id)) {
                {...stmt, isBkm}
            } else {
                stmt
            }
        })
    }
}

let canGoEditModeForStmt = (st:editorState,stmtId) => {
    !(st.stmts->Array.some(stmt => 
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
        if (newLabel->String.trim != "") {
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
        if (newContText->String.trim == "") {
            stmt
        } else {
            {
                ...stmt,
                cont:strToCont(newContText, ~preCtxColors=st.preCtxData.symColors, ~wrkCtxColors=st.wrkCtxColors),
                contEditMode: false,
                isDuplicated: false,
            }
        }
    })
}

let setStmtCont = (st, stmtId, stmtCont):editorState => {
    let newContStr = stmtCont->contToStr
    let isDuplicated = st.preCtxData.settingsV.val.autoMergeStmts 
        && st.stmts->Array.some(stmt => stmt.cont->contToStr == newContStr)
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

let setNextAction = (st:editorState, action:option<editorStateAction>):editorState => {
    {
        ...st,
        nextAction: action
    }
}

let extractVarColorsFromVarsText = (varsText:string, typeColors:Belt_HashMapString.t<string>):Belt_HashMapString.t<string> => {
    let res = Belt_HashMapString.make(~hintSize=16)
    switch textToVarDefs(varsText) {
        | Error(_) => ()
        | Ok(varDefs) => {
            varDefs->Array.forEach(varDef => {
                let varName = varDef->Array.getUnsafe(2)
                let typ = varDef->Array.getUnsafe(1)
                switch typeColors->Belt_HashMapString.get(typ) {
                    | None => ()
                    | Some(color) => res->Belt_HashMapString.set(varName, color)
                }
            })
        }
    }
    res
}

let recalcWrkCtxColors = (st:editorState):editorState => {
    {
        ...st,
        wrkCtxColors: extractVarColorsFromVarsText(st.varsText, st.preCtxData.typeColors),
    }
}

let updateColorsInAllStmts = st => {
    {
        ...st,
        stmts: st.stmts->Array.map(stmt => {
            ...stmt,
            cont: stmt.cont->contToStr->strToCont(~preCtxColors=st.preCtxData.symColors, ~wrkCtxColors=st.wrkCtxColors)
        })
    }
}

let recalcWrkColors = (st:editorState):editorState => {
    let st = recalcWrkCtxColors(st)
    let st = updateColorsInAllStmts(st)
    st
}

let setPreCtxData = (st:editorState, preCtxData:preCtxData):editorState => {
    let st = { ...st, preCtxData:preCtxData, }
    let st = recalcWrkColors(st)
    st
}

let completeDescrEditMode = (st, newDescr) => {
    {
        ...st,
        descr:newDescr,
        descrEditMode: false
    }
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
    let stmtsLen = st.stmts->Array.length
    if (stmtsLen < 2) {
        st
    } else {
        let newStmts = st.stmts->Array.copy
        let changed = ref(true)
        let e = ref(stmtsLen - 2)
        while (e.contents >= 0 && changed.contents) {
            changed.contents = false
            for i in 0 to e.contents {
                if (comp(newStmts->Array.getUnsafe(i), newStmts->Array.getUnsafe(i+1)) > 0) {
                    let tmp = newStmts->Array.getUnsafe(i)
                    newStmts[i] = newStmts->Array.getUnsafe(i+1)
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
    let stmtToInt = (stmt:userStmt) => {
        switch stmt.typ {
            | E => 1
            | P => {
                if (st.preCtxData.settingsV.val.stickGoalToBottom) {
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
        stmts: st.stmts->Array.map(stmt => {
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
        st.stmts->Array.some(stmt => 
            stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode 
        )
}

let userStmtHasCriticalErrors = stmt => {
    stmt.stmtErr->Belt_Option.isSome
}

let userStmtHasAnyErrors = stmt => {
    stmt.stmtErr->Belt_Option.isSome 
        || stmt.syntaxErr->Belt_Option.isSome 
        || stmt.unifErr->Belt_Option.isSome
}

let editorStateHasCriticalErrors = st => {
    st.varsErr->Belt_Option.isSome 
        || st.disjErr->Belt_Option.isSome 
        || st.stmts->Array.some(userStmtHasCriticalErrors)
}

let editorStateGetTextDescriptionOfAllCriticalErrors = (st:editorState):string => {
    let res = []
    st.varsErr->Option.forEach(msg => res->Array.push(`Variable definition error: ${msg}`))
    st.disjErr->Option.forEach(msg => res->Array.push(`Disjoints error: ${msg}`))
    if (st.stmts->Array.some(userStmtHasCriticalErrors)) {
        res->Array.push(`Errors for steps:`)
        st.stmts->Array.forEach(stmt => {
            stmt.stmtErr->Option.forEach(err => {
                res->Array.push(`${stmt.cont->contToStr}:`)
                res->Array.push(err.msg)
            })
        })
    }
    res->Array.join(" \n")
}

let editorStateHasAnyErrors = st => {
    st->editorStateHasCriticalErrors || st.stmts->Array.some(userStmtHasAnyErrors)
}

let editorStateHasDuplicatedStmts = (st:editorState):bool => {
    st.stmts->Array.some(stmt => {
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
        ~preCtx=st.preCtxData.ctxV.val.min,
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
    let jstfText = jstfText->String.trim
    if (jstfText->String.length == 0) {
        Ok(None)
    } else {
        let argsAndAsrt = jstfText->String.split(":")
        if (argsAndAsrt->Array.length != 2) {
            Error(`Cannot parse justification: '${jstfText}'. A justification must contain exactly one colon symbol.`)
        } else if (argsAndAsrt->Array.getUnsafe(1)->String.trim == "") {
            Error(`Cannot parse justification: '${jstfText}'. Reference must not be empty.`)
        } else {
            Ok(Some({
                args: argsAndAsrt->Array.getUnsafe(0)->getSpaceSeparatedValuesAsArray,
                label: argsAndAsrt->Array.getUnsafe(1)->String.trim
            }))
        }
    }
}

let setStmtExpr = (stmt:userStmt,wrkCtx:mmContext):userStmt => {
    if (userStmtHasCriticalErrors(stmt)) {
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
    if (userStmtHasCriticalErrors(stmt) || stmt.typ == E) {
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
    if (userStmtHasCriticalErrors(stmt)) {
        stmt
    } else {
        switch stmt.jstf {
            | None => stmt
            | Some({args,label}) => {
                switch args->Array.find(ref => !isLabelDefined(ref,wrkCtx,definedUserLabels) ) {
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
                                    let providedNumberOfArgs = args->Array.length
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
    if (userStmtHasCriticalErrors(stmt)) {
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
    if (userStmtHasCriticalErrors(stmt)) {
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
                                if (!stmt.contEditMode && (expr->Array.length == 0 || expr->Array.getUnsafe(0) >= 0)) {
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
    if (userStmtHasCriticalErrors(stmt)) {
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
            let stmtsLen = st.stmts->Array.length
            let definedUserLabels = Belt_HashSetString.make(~hintSize=stmtsLen)
            let definedUserExprs = Belt_HashMap.make(~hintSize=stmtsLen, ~id=module(ExprHash))
            let goalLabel = ref(None)
            let actions = [
                validateStmtLabel(_, wrkCtx, definedUserLabels),
                setStmtExpr(_, wrkCtx),
                validateStmtIsGoal(_, goalLabel),
                setStmtJstf,
                validateStmtJstf(_, wrkCtx, definedUserLabels, st.preCtxData.frms),
                validateStmtExpr(_, wrkCtx, definedUserExprs),
            ]
            st.stmts->Array.reduce(
                st,
                (st,stmt) => {
                    if (editorStateHasCriticalErrors(st)) {
                        st
                    } else {
                        let stmt = actions->Array.reduce(
                            stmt,
                            (stmt,action) => {
                                if (userStmtHasCriticalErrors(stmt)) {
                                    stmt
                                } else {
                                    action(stmt)
                                }
                            }
                        )

                        definedUserLabels->Belt_HashSetString.add(stmt.label)
                        if (!userStmtHasCriticalErrors(stmt)) {
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
                }
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
    ]->Array.reduce(
        st,
        (st,act) => {
            if (editorStateHasCriticalErrors(st)) {
                st
            } else {
                act(st)
            }
        }
    )
}

let getTheOnlyCheckedStmt = (st):option<userStmt> => {
    if (st.checkedStmtIds->Array.length != 1) {
        None
    } else {
        getTopmostCheckedStmt(st)
    }
}

let createNewVars = (
    st:editorState, 
    ~varTypes:array<int>,
    ~varNames:option<array<string>>=?,
    ~dontAddVariablesToContext:bool=false
):(editorState,array<int>) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot create new variables without wrkCtx.`}))
        | Some(wrkCtx) => {
            let numOfVars = varTypes->Array.length
            if (numOfVars == 0) {
                (st,[])
            } else {
                let newVarNames =
                    switch varNames {
                        | Some(varNames) => {
                            if (varTypes->Array.length != varNames->Array.length) {
                                raise(MmException({msg:`varTypes->Array.length != varNames->Array.length`}))
                            }
                            varNames
                        }
                        | None => {
                            let typeToPrefix = Belt_MapString.fromArray(
                                st.preCtxData.settingsV.val.typeSettings->Array.map(ts => (ts.typ, ts.prefix))
                            )
                            generateNewVarNames(
                                ~ctx=wrkCtx,
                                ~types=varTypes, 
                                ~typeToPrefix
                            )
                        }
                    }
                let newHypLabels = generateNewLabels(
                    ~ctx=wrkCtx,
                    ~prefix="var", 
                    ~amount=numOfVars
                )
                let varTypeNames = wrkCtx->ctxIntsToSymsExn(varTypes)
                if (!dontAddVariablesToContext) {
                    wrkCtx->applySingleStmt(Var({symbols:newVarNames}))
                    newHypLabels->Array.forEachWithIndex((label,i) => {
                        wrkCtx->applySingleStmt(Floating({label, expr:[varTypeNames->Array.getUnsafe(i), newVarNames->Array.getUnsafe(i)]}))
                    })
                }
                let newVarInts = wrkCtx->ctxSymsToIntsExn(newVarNames)
                let newVarsText = newHypLabels->Array.mapWithIndex((label,i) => {
                    `${label} ${varTypeNames->Array.getUnsafe(i)} ${newVarNames->Array.getUnsafe(i)}`
                })->Array.joinUnsafe("\n")
                let st = {
                    ...st,
                    varsText: [st.varsText, newVarsText]->Array.joinUnsafe("\n")->String.trim
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
                wrkCtx->applySingleStmt(Disj({vars:varsStr}))
                newDisjTextLines->Array.push(varsStr->Array.joinUnsafe(" "))
            })
            if (newDisjTextLines->Array.length == 0) {
                st
            } else {
                let newDisjText = newDisjTextLines->Array.joinUnsafe("\n")
                {
                    ...st,
                    disjText: [st.disjText, newDisjText]->Array.joinUnsafe("\n")->String.trim
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
    st.stmts->Array.find(stmt => {
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
    ~isBkm:bool,
):(editorState,string) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot insertStmt without wrkCtx.`}))
        | Some(wrkCtx) => {
            switch wrkCtx->getHypByExpr(expr) {
                | Some(hyp) => (st,hyp.label)
                | None => {
                    let maxIdx = switch before {
                        | None => st.stmts->Array.length
                        | Some(stmtId) => {
                            switch st.stmts->Array.findIndex(stmt => stmt.id == stmtId) {
                                | -1 => st.stmts->Array.length
                                | idx => idx
                            }
                        }
                    }
                    let minIdx = switch jstf {
                        | None => 0
                        | Some({args}) => {
                            let remainingLabels = Belt_HashSetString.fromArray(args)
                            let minIdx = st.stmts->Array.reduceWithIndex(
                                0,
                                (minIdx,stmt,idx) => {
                                    if (remainingLabels->Belt_HashSetString.isEmpty) {
                                        minIdx
                                    } else {
                                        remainingLabels->Belt_HashSetString.remove(stmt.label)
                                        idx + 1
                                    }
                                }
                            )
                            if (maxIdx < minIdx) { maxIdx } else { minIdx }
                        }
                    }

                    let updateExistingStmt = (st,existingStmt:userStmt):(editorState,string) => {
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
                        let (st,newStmtId) = st->addNewStmtAtIdx(~idx=newIdx)
                        let st = st->updateStmt(newStmtId, stmt => {
                            {
                                ...stmt,
                                typ: P,
                                cont: strToCont( 
                                    wrkCtx->ctxIntsToStrExn(expr), 
                                    ~preCtxColors=st.preCtxData.symColors, ~wrkCtxColors=st.wrkCtxColors
                                ),
                                contEditMode: false,
                                isBkm,
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
        stmts: newStmts.stmts->Array.map(stmt => {
            {
                ...stmt,
                expr: stmt.expr->Array.map(i => newStmtsVarToCtxVar->Belt_MutableMapInt.getWithDefault(i,i))
            }
        })
    }
}

let addNewStatements = (st:editorState, newStmts:stmtsDto, ~isBkm:bool=false):editorState => {
    let (st, newCtxVarInts) = createNewVars(st,~varTypes=newStmts.newVarTypes)
    let newStmtsVarToCtxVar = Belt_MutableMapInt.make()
    newStmts.newVars->Array.forEachWithIndex((newStmtsVarInt,i) => {
        newStmtsVarToCtxVar->Belt_MutableMapInt.set(newStmtsVarInt, newCtxVarInts->Array.getUnsafe(i))
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

    let checkedStmt = st->getLowestCheckedStmt
    let newStmtsLabelToCtxLabel = Belt_MutableMapString.make()

    let replaceDtoLabelsWithCtxLabels = jstf => {
        {
            ...jstf,
            args: jstf.args->Array.map(arg => 
                newStmtsLabelToCtxLabel->Belt_MutableMapString.getWithDefault(arg,arg)
            )
        }
    }

    let mergeWillBeNeeded = newStmts.stmts->Array.some(stmtDto => {
        st.stmts->Array.some(userStmt => stmtsHaveSameExpr(userStmt, stmtDto))
    })
    let placeAtMaxIdxByDefault = checkedStmt->Belt.Option.isSome && !mergeWillBeNeeded

    let stMut = ref(st)
    newStmts.stmts->Array.forEach(stmtDto => {
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
                ~placeAtMaxIdxByDefault,
                ~isBkm,
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
            wrkCtx->getLocalHyps->Array.forEach(hyp => {
                if (hyp.typ == F && hyp.label->String.startsWith(".")) {
                    usedSymbols->Belt_HashSetString.add(wrkCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(1)))
                }
            })
            let unusedVars = wrkCtx->getLocalVars->Array.filter(var => !(usedSymbols->Belt_HashSetString.has(var)))
            let st = if (unusedVars->Array.length == 0) {
                st
            } else {
                let unusedVarInts = wrkCtx->ctxSymsToIntsExn(unusedVars)->Belt_HashSetInt.fromArray
                let usedLocalVarsStr = wrkCtx->getLocalHyps
                    ->Array.filter(hyp => hyp.typ == F && !(unusedVarInts->Belt_HashSetInt.has(hyp.expr->Array.getUnsafe(1))))
                    ->Array.map(hyp => 
                        `${hyp.label} ${wrkCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(0))} ${wrkCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(1))}`
                    )
                    ->Array.joinUnsafe("\n")
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
                ~varIntToVarName=ctxIntToSym(wrkCtx, _),
                ~varIntToVarType=getTypeOfVar(wrkCtx, _),
                ~typeOrder=st.preCtxData.typeOrderInDisj
            )
                ->Array.map(dgrp => wrkCtx->ctxIntsToSymsExn(dgrp)->Array.joinUnsafe(" "))
                ->Array.joinUnsafe("\n")
            let st = if (st.disjText != newDisjText) {
                completeDisjEditMode(st, newDisjText)
            } else {
                st
            }
            st
        }
    }
}

let srcToJstf = (
    wrkCtx:mmContext, 
    proofTree:proofTreeDto, 
    exprSrc:exprSrcDto, 
    exprToUserStmt:Belt_HashMap.t<expr,userStmt,ExprHash.identity>
):option<jstf> => {
    switch exprSrc {
        | Assertion({args, label}) => {
            switch wrkCtx->getFrame(label) {
                | None => raise(MmException({msg:`Cannot find an assertion '${label}' in srcToJstf.`}))
                | Some(frame) => {
                    let argLabels = []
                    let argLabelsValid = ref(true)
                    frame.hyps->Array.forEachWithIndex((hyp,i) => {
                        if (hyp.typ == E && argLabelsValid.contents) {
                            switch args->Belt_Array.get(i) {
                                | None => raise(MmException({msg:`Too few arguments for '${label}' in srcToJstf.`}))
                                | Some(nodeIdx) => {
                                    switch exprToUserStmt->Belt_HashMap.get((proofTree.nodes->Array.getUnsafe(nodeIdx)).expr) {
                                        | None => argLabelsValid.contents = false //todo: return a meaningful error from here
                                        | Some(userStmt) => argLabels->Array.push(userStmt.label)
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

let userStmtSetJstfTextAndProof = (
    stmt:userStmt, wrkCtx, proofTree:proofTreeDto, proofNode:proofNodeDto, exprToUserStmt
):userStmt => {
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
                    let partialAsrtLabels = proofNode.parents->Array.map(src => {
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
                    })->Array.filter(Belt_Option.isSome(_))->Array.map(Belt_Option.getExn(_))
                    let unifErr = if (partialAsrtLabels->Array.length > 0) {
                        Some(unifErrToStr(
                            TooManyCombinations({frmLabels:Some(partialAsrtLabels)}),
                            ~exprToStr = ctxIntsToStrExn(wrkCtx, _),
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
                    switch proofNode.parents->Array.find(srcEqJstf(_, jstf)) {
                        | Some(src) => {...stmt, proofStatus:Some(Waiting), src:Some(src)}
                        | None => {
                            let errors = proofNode.parents->Array.map(src => {
                                switch src {
                                    | VarType | Hypothesis(_) | Assertion(_) => None
                                    | AssertionWithErr({label, err}) => Some((label,err))
                                }
                            })->Array.filter(Belt_Option.isSome(_))->Array.map(Belt_Option.getExn(_))
                            let unifErr = if (errors->Array.length > 0) {
                                Some(
                                    errors->Array.map(((asrtLabel,err)) => {
                                        unifErrToStr(
                                            err,
                                            ~exprToStr = ctxIntsToStrExn(wrkCtx, _),
                                            ~frmExprToStr = 
                                                expr => wrkCtx->frmIntsToStrExn(wrkCtx->getFrameExn(asrtLabel),expr)
                                        )
                                    })->Array.joinUnsafe("\n\n")
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
    ~wrkCtxColors:option<Belt_HashMapString.t<string>>=?
):syntaxTreeNode => {
    {
        ...tree,
        children: tree.children->Array.map(child => {
            switch child {
                | Subtree(syntaxTreeNode) => {
                    Subtree(addColorsToSyntaxTree(~tree=syntaxTreeNode, ~preCtxColors?, ~wrkCtxColors?))
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
    while (i.contents < expr->Array.length && parenState.contents != Failed) {
        parenState := parenCnt->parenCntPut(expr->Array.getUnsafe(i.contents))
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
            let syntaxTree = switch syntaxNodes->Belt_HashMap.get(expr->Array.sliceToEnd(~start=1)) {
                | None => None
                | Some(nodeDto) => Some(buildSyntaxTreeFromProofTreeDto(~ctx=wrkCtx, ~proofTreeDto, ~typeStmt=nodeDto.expr))
            }
            switch syntaxTree {
                | None => {
                    if (st.preCtxData.settingsV.val.checkSyntax) {
                        {
                            ...stmt,
                            syntaxErr: Some(if (checkParensMatch(expr, st.preCtxData.parenCnt)) {""} else {"parentheses mismatch"}),
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
                            exprTyp: (syms->Array.getUnsafe(0)).sym,
                            root: addColorsToSyntaxTree( 
                                ~tree=syntaxTree, 
                                ~preCtxColors=st.preCtxData.symColors, 
                                ~wrkCtxColors=st.wrkCtxColors
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
                ->Array.map(node => (node.expr,node))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let exprToUserStmt = st.stmts
                ->Array.map(stmt => {
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
            st.stmts->Array.reduce(
                st,
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
                }
            )
        }
    }
}

let splitIntoChunks = (str, chunkMaxSize): array<string> => {
    let len = str->String.length
    if (len <= chunkMaxSize) {
        [str]
    } else {
        let res = []
        let numberOfChunks = Math.Int.ceil(len->Belt_Int.toFloat /. chunkMaxSize->Belt_Int.toFloat)
        for i in 1 to numberOfChunks {
            let begin = (i-1)*chunkMaxSize
            res->Array.push(str->Js_string2.substrAtMost(~from=begin, ~length=chunkMaxSize))
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
            let blk = splitIntoChunks(compressedProofBlock, 50)->Array.joinUnsafe(" ")
            let asrt = `${stmt.label} $p ${stmt.cont->contToStr} $= ( ${labels->Array.joinUnsafe(" ")} ) ${blk} $.`
            let descrIsEmpty = descr->String.trim->String.length == 0
            let blockIsRequired = newHyps->Array.length > 0 || !(newDisj->disjIsEmpty) || !descrIsEmpty
            let result = []
            if (blockIsRequired) {
                result->Array.push("${")
            }
            let varsArrStr = newHyps->Array.filter(hyp => hyp.typ == F)
                ->Array.map(hyp => wrkCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(1)))
            if (varsArrStr->Array.length > 0) {
                result->Array.push("$v " ++ varsArrStr->Array.joinUnsafe(" ") ++ " $.")
            }
            newHyps->Array.forEach(hyp => {
                if (hyp.typ == F) {
                    result->Array.push(hyp.label ++ " $f " ++ wrkCtx->ctxIntsToStrExn(hyp.expr) ++ " $.")
                }
            })
            newDisj->disjForEachArr(
                ~sortByTypeAndName=true,
                ~varIntToVarType=getTypeOfVar(wrkCtx, _),
                ~varIntToVarName=ctxIntToSym(wrkCtx, _),
                ~typeOrder=typeOrderInDisj,
                vars => {
                    result->Array.push("$d " ++ wrkCtx->ctxIntsToStrExn(vars) ++ " $.")
                }
            )
            newHyps->Array.forEach(hyp => {
                if (hyp.typ == E) {
                    result->Array.push(hyp.label ++ " $e " ++ wrkCtx->ctxIntsToStrExn(hyp.expr) ++ " $.")
                }
            })
            if (!descrIsEmpty) {
                result->Array.push("$( " ++ descr ++ " $)")
            }
            result->Array.push(asrt)
            if (blockIsRequired) {
                result->Array.push("$}")
            }
            result->Array.joinUnsafe("\r\n")
        }
        | _ => "Error: only compressed proofs are supported."
    }
}

let generateCompressedProof = (st, stmtId, ~useAllLocalEHyps:bool=false):option<(string,string,string)> => {
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
                                    let preCtx = st.preCtxData.ctxV.val.min
                                    let expr = userStmtToRootStmt(stmt).expr
                                    let proofTableWithTypes = createProofTable(~tree=proofTreeDto, ~root=proofNode)
                                    let proofTableWithoutTypes = createProofTable(
                                        ~tree=proofTreeDto, ~root=proofNode, ~essentialsOnly=true, ~ctx=wrkCtx
                                    )
                                    let exprsUsedInProof = proofTableWithTypes->Array.map(r => r.expr)
                                        ->Belt_HashSet.fromArray(~id=module(ExprHash))
                                    let rootStmts = st.stmts->Array.map(userStmtToRootStmt)
                                    if (useAllLocalEHyps) {
                                        rootStmts->Array.forEach(stmt => {
                                            if (stmt.isHyp) {
                                                exprsUsedInProof->Belt_HashSet.add(stmt.expr)
                                            }
                                        })
                                    }
                                    let proofCtx = createProofCtx(
                                        wrkCtx,
                                        rootStmts->Array.filter(stmt => {
                                            stmt.isHyp && exprsUsedInProof->Belt_HashSet.has(stmt.expr)
                                        })
                                    )

                                    let mandHyps = proofCtx->getMandHyps(expr)
                                    let proof = MM_proof_table.createProof(
                                        mandHyps, proofTableWithTypes, proofTableWithTypes->Array.length-1
                                    )

                                    let newHyps = []
                                    mandHyps->Array.forEach(hyp => {
                                        if (preCtx->getHypByExpr(hyp.expr)->Belt.Option.isNone) {
                                            newHyps->Array.push(hyp)
                                        }
                                    })
                                    let varsUsedInProof = Belt_HashSetInt.make(~hintSize=16)
                                    exprsUsedInProof->Belt_HashSet.forEach(expr => {
                                        expr->Array.forEach(s => {
                                            if (s >= 0) {
                                                varsUsedInProof->Belt_HashSetInt.add(s)
                                            }
                                        })
                                    })
                                    let mandVars = mandHyps->Array.map(hyp => hyp.expr->Array.getUnsafe(1))->Belt_HashSetInt.fromArray
                                    wrkCtx->getLocalHyps->Array.forEach(hyp => {
                                        if (hyp.typ == F) {
                                            let var = hyp.expr->Array.getUnsafe(1)
                                            if (varsUsedInProof->Belt_HashSetInt.has(var) 
                                                && !(mandVars->Belt_HashSetInt.has(var))) {
                                                newHyps->Array.push(hyp)
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
                                            ~wrkCtx=wrkCtx, ~typeOrderInDisj=st.preCtxData.typeOrderInDisj,
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
    st.stmts->Array.reduce(
        Ok(st),
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
                                if (jstf.args->Array.includes(replaceWhat)) {
                                    let newJstf = {
                                        ...jstf,
                                        args: jstf.args
                                            ->Array.map(ref => if (ref == replaceWhat) {replaceWith} else {ref})
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
        }
    )
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->String.replaceRegExp(symbolsNotAllowedInLabelRegex, "")

let renameStmt = (st:editorState, stmtId:stmtId, newLabel:string):result<editorState,string> => {
    let newLabel = newLabel->removeSymbolsNotAllowedInLabel
    if (newLabel == "") {
        Error(`label must not be empty.`)
    } else {
        switch st.stmts->Array.find(stmt => stmt.id != stmtId && stmt.label == newLabel) {
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
                st.stmts->Array.find(stmt => stmt.isGoal)->Belt.Option.isSome
                || st.preCtxData.ctxV.val.min->getTokenType(newStmt.label)->Belt.Option.isSome
            ) {
                createNewLabel(st, ~forHyp=true)
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

let completeTypEditMode = (st:editorState, stmtId:stmtId, newTyp:userStmtType, newIsGoal:bool) => {
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
        let jstfTrimUpperCase = newJstfInp->String.trim->String.toLocaleUpperCase
        let newTyp = if (jstfTrimUpperCase == defaultJstfForHyp) {E} else {P}
        let newJstf = if (jstfTrimUpperCase == defaultJstfForHyp) {""} else {newJstfInp->String.trim}

        let pCnt = st.stmts->Array.reduce(
            0,
            (cnt,stmt) => {
                if (stmt.id != stmtId && stmt.typ == P) {
                    cnt + 1
                } else {
                    cnt
                }
            }
        )
        let settings = st.preCtxData.settingsV.val
        let newIsGoal = if (newTyp == E) { false } else { stmt.isGoal || settings.initStmtIsGoal && pCnt == 0 }
        let newLabel = if (newIsGoal && !stmt.isGoal && settings.defaultStmtLabel->String.length > 0) {
            settings.defaultStmtLabel
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

let isHyp = (stmtTyp:option<userStmtTypeExtended>):bool => {
    switch stmtTyp {
        | None => false
        | Some(typ) => {
            switch typ {
                | H => true
                | P | G => false
            }
        }
    }
}

let addStepsWithoutVars = (
    st:editorState,
    ~atIdx:option<int>=?,
    ~steps:array<userStmtDtoOpt>
):result<(editorState,array<stmtId>),string> => {
    let updates:array<(editorState,stmtId,userStmtDtoOpt)=>result<editorState,string>> = [
        (st,stmtId,step) => {
            switch step.cont {
                | None => Error(`Steps must not have empty statements.`)
                | Some(cont) => {
                    if (cont->String.trim == "") {
                        Error(`Steps must not have empty statements.`)
                    } else {
                        Ok(st->completeContEditMode(stmtId, cont))
                    }
                }
            }
        },
        (st,stmtId,step) => {
            switch step.label {
                | None => Ok(st)
                | Some(label) => {
                    if (label->String.trim == "") {
                        Error(`Steps must not have empty labels.`)
                    } else {
                        Ok(st->completeLabelEditMode(stmtId, label))
                    }
                }
            }
        },
        (st,stmtId,step) => {
            switch step.typ {
                | None => Ok(st)
                | Some(typExt) => {
                    let (typ,isGoal) = typExt->userStmtTypeExtendedToUserStmtType
                    Ok(st->completeTypEditMode(stmtId, typ, isGoal))
                }
            }
        },
        (st,stmtId,step) => {
            switch step.jstf {
                | None => Ok(st)
                | Some(jstf) => Ok(st->completeJstfEditMode(stmtId, jstf))
            }
        },
        (st,stmtId,step) => {
            switch step.isBkm {
                | None => Ok(st)
                | Some(isBkm) => Ok(st->updateStmt(stmtId, stmt => {...stmt, isBkm}))
            }
        },
    ]
    let stmtIds = []
    let res = steps->Array.reduceWithIndex(
        Ok(st),
        (res, step, i) => {
            switch res {
                | Error(_) => res
                | Ok(st) => {
                    let (st,stmtId) = switch atIdx {
                        | None => st->addNewStmt(~isHyp=isHyp(step.typ))
                        | Some(atIdx) => st->addNewStmtAtIdx(~idx=atIdx+i, ~isHyp=isHyp(step.typ))
                    }
                    stmtIds->Array.push(stmtId)
                    updates->Array.reduce(Ok(st), (res,update) => res->Belt.Result.flatMap(update(_,stmtId,step)))
                }
            }
        }
    )
    switch res {
        | Error(msg) => Error(msg)
        | Ok(st) => Ok((st,stmtIds))
    }
}

let validateVarNames = (vars:array<(string,option<string>)>):bool => {
    vars->Array.every(((_,varNameOpt)) => varNameOpt->Belt_Option.isNone)
    || vars->Array.every(((_,varNameOpt)) => varNameOpt->Belt_Option.isSome)
}

let addSteps = (
    st:editorState,
    ~atIdx:option<int>=?,
    ~steps:array<userStmtDtoOpt>,
    ~vars:array<(string,option<string>)>=[],
    ~dontAddVariablesToContext:bool
):result<(editorState,array<stmtId>),string> => {
    if (vars->Array.length == 0) {
        st->addStepsWithoutVars( ~atIdx?, ~steps )
    } else {
        switch st.wrkCtx {
            | None => Error("Cannot add new variables because of errors in the editor.")
            | Some(wrkCtx) => {
                let varTypesStr = vars->Array.map(((typStr,_)) => typStr)
                let varTypesOpt = varTypesStr->Array.map(ctxSymToInt(wrkCtx, _))
                let unknownTypeIdx = varTypesOpt->Array.findIndex(Belt_Option.isNone(_))
                if (unknownTypeIdx >= 0) {
                    Error(`Unknown type - ${varTypesStr->Array.getUnsafe(unknownTypeIdx)}`)
                } else {
                    let varTypes = varTypesOpt->Array.map(Belt_Option.getExn(_))
                    if (!validateVarNames(vars)) {
                        Error("All variable names must be either defined or undefined.")
                    } else {
                        let st = if (vars->Array.some(((_,varName)) => varName->Belt_Option.isNone)) {
                            let (st, _) = createNewVars(st, ~varTypes, ~dontAddVariablesToContext)
                            st
                        } else {
                            let varNames = vars->Array.map(((_,varName)) => varName->Belt_Option.getExn)
                            let (st, _) = createNewVars(st, ~varTypes, ~varNames, ~dontAddVariablesToContext)
                            st
                        }
                        st->addStepsWithoutVars( ~atIdx?, ~steps )
                    }
                }
            }
        }
    }
}

let updateSteps = (
    st:editorState,
    steps:array<userStmtDtoOpt>,
):result<editorState,string> => {
    let updates:array<(userStmt,userStmtDtoOpt)=>result<userStmt,string>> = [
        (stmt,step) => {
            switch step.cont {
                | None => Ok(stmt)
                | Some(cont) => {
                    if (cont->String.trim == "") {
                        Error(`Steps must not have empty statements.`)
                    } else {
                        Ok({
                            ...stmt,
                            cont:strToCont(cont, ~preCtxColors=st.preCtxData.symColors, ~wrkCtxColors=st.wrkCtxColors),
                            contEditMode: false,
                            isDuplicated: false,
                        })
                    }
                }
            }
        },
        (stmt,step) => {
            switch step.typ {
                | None => Ok(stmt)
                | Some(typExt) => {
                    let (typ,isGoal) = typExt->userStmtTypeExtendedToUserStmtType
                    Ok({ ...stmt, typ:typ, isGoal:isGoal, typEditMode: false })
                }
            }
        },
        (stmt,step) => {
            switch step.jstf {
                | None => Ok(stmt)
                | Some(jstf) => Ok({ ...stmt, jstfText:jstf, jstfEditMode:false })
            }
        },
        (stmt,step) => {
            switch step.isBkm {
                | None => Ok(stmt)
                | Some(isBkm) => Ok({ ...stmt, isBkm })
            }
        },
    ]
    let stmtIdToStepDto = steps->Array.filter(step => step.id->Belt_Option.isSome)
        ->Array.map(step => (step.id->Belt_Option.getExn,step))
        ->Belt_HashMapString.fromArray
    let newStmtsRes = st.stmts->Array.reduce(
        Ok([]),
        (res, stmt) => {
            switch res {
                | Error(_) => res
                | Ok(newStmts) => {
                    let newStmtRes = switch stmtIdToStepDto->Belt_HashMapString.get(stmt.id) {
                        | None => Ok(stmt)
                        | Some(step) => {
                            updates->Array.reduce(
                                Ok(stmt),
                                (res,update) => res->Belt_Result.flatMap(update(_,step))
                            )
                        }
                    }
                    switch newStmtRes {
                        | Error(msg) => Error(msg)
                        | Ok(stmt) => {
                            newStmts->Array.push(stmt)
                            Ok(newStmts)
                        }
                    }
                }
            }
        }
    )
    switch newStmtsRes {
        | Error(msg) => Error(msg)
        | Ok(newStmts) => Ok({...st, stmts:newStmts})
    }
}

let findStmtsToMerge = (st:editorState):result<(userStmt,userStmt),string> => {
    let stmt1 = if (st.checkedStmtIds->Array.length == 0) {
        st.stmts->Array.find(stmt => {
            stmt.stmtErr->Belt_Option.map(err => err.code == duplicatedStmtErrCode)->Belt.Option.getWithDefault(false)
        })
    } else {
        let (checkedStmtId,_) = st.checkedStmtIds->Array.getUnsafe(0)
        st->editorGetStmtById(checkedStmtId)
    }
    switch stmt1 {
        | None => Error("[1] Cannot determine a duplicated step.")
        | Some(stmt1) => {
            let contStr = stmt1.cont->contToStr
            switch st.stmts->Array.find(stmt => stmt.id != stmt1.id && stmt.cont->contToStr == contStr) {
                | None => Error("[2] Cannot find another step to merge with.")
                | Some(stmt2) => {
                    let idx1 = st.stmts->Array.findIndex(stmt => stmt.id == stmt1.id)
                    let idx2 = st.stmts->Array.findIndex(stmt => stmt.id == stmt2.id)
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
    st.stmts->Array.find(stmt => 
        !stmt.isDuplicated
        && stmt.stmtErr->Belt_Option.map(err => err.code == duplicatedStmtErrCode)
            ->Belt_Option.getWithDefault(false)
    )
}

let findSecondDuplicatedStmt = (st:editorState, stmt1:userStmt):option<userStmt> => {
    let contStr = stmt1.cont->contToStr
    st.stmts->Array.find(stmt2 => {
        !stmt2.isDuplicated && stmt2.id != stmt1.id && stmt2.cont->contToStr == contStr
    })
}

let autoMergeDuplicatedStatements = (st:editorState, ~selectFirst:bool):(editorState,array<(string,string)>) => {
    let renames = []
    let resultState = ref(st)
    let continue = ref(true)
    while (continue.contents) {
        switch resultState.contents->findFirstDuplicatedStmt {
            | None => continue := false
            | Some(stmt1) => {
                switch resultState.contents->findSecondDuplicatedStmt(stmt1) {
                    | None => continue := false
                    | Some(stmt2) => {
                        let jstf1 = stmt1.jstfText->String.trim
                        let jstf2 = stmt2.jstfText->String.trim
                        if (selectFirst) {
                            if (jstf1 == "") {
                                continue := false
                            } else {
                                switch resultState.contents->mergeStmts(stmt2.id, stmt1.id) {
                                    | Error(_) => continue := false
                                    | Ok(stateAfterMerge) => {
                                        renames->Array.push((stmt1.label, stmt2.label))
                                        resultState := stateAfterMerge->prepareEditorForUnification
                                    }
                                }
                            }
                        } else {
                            if (jstf1 != "" && jstf2 == "") {
                                switch resultState.contents->mergeStmts(stmt1.id, stmt2.id) {
                                    | Error(_) => continue := false
                                    | Ok(stateAfterMerge) => {
                                        renames->Array.push((stmt2.label, stmt1.label))
                                        resultState := stateAfterMerge->prepareEditorForUnification
                                    }
                                }
                            } else if (jstf2 != "" && (jstf1 == "" || jstf1 == jstf2)) {
                                switch resultState.contents->mergeStmts(stmt2.id, stmt1.id) {
                                    | Error(_) => continue := false
                                    | Ok(stateAfterMerge) => {
                                        renames->Array.push((stmt1.label, stmt2.label))
                                        resultState := stateAfterMerge->prepareEditorForUnification
                                    }
                                }
                            } else {
                                continue := false
                            }
                        }
                    }
                }
            }
        }
    }
    (resultState.contents,renames)
}

let verifyEditorState = (st:editorState):editorState => {
    let st = prepareEditorForUnification(st)
    if (st.wrkCtx->Belt_Option.isSome) {
        let st = removeUnusedVars(st)
        let st = if (st.preCtxData.settingsV.val.autoMergeStmts) {
            let (st,_) = autoMergeDuplicatedStatements(st, ~selectFirst=false)
            st
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
                | Some(Symbol({id, symInt, sym, color, isVar})) => {
                    if (treeData.expLvl == 0) {
                        Some(Symbol({id, symInt, sym, color, isVar}))
                    } else {
                        let curParent = ref(treeData.root->syntaxTreeGetParent(id))
                        let curLvl = ref(treeData.expLvl)
                        while (curLvl.contents > 1 && curParent.contents->Belt_Option.isSome) {
                            curLvl := curLvl.contents - 1
                            curParent := treeData.root->syntaxTreeGetParent((curParent.contents->Belt_Option.getExn).id)
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
        expLvl: Math.Int.min(treeData.expLvl + 1, treeData.root.height)
    }
}

let decExpLvl = (treeData:stmtContTreeData):stmtContTreeData => {
    {
        ...treeData, 
        expLvl: Math.Int.max(treeData.expLvl - 1, 0)
    }
}

let getAllExprsToSyntaxCheck = (st:editorState, rootStmts:array<rootStmt>):array<expr> => {
    let res = []
    st.stmts->Array.forEachWithIndex((stmt,i) => {
        switch stmt.cont {
            | Tree(_) => ()
            | Text(_) => res->Array.push((rootStmts->Array.getUnsafe(i)).expr->Array.sliceToEnd(~start=1))
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
                    | Some(Symbol({id})) => {
                        switch treeData.root->syntaxTreeGetParent(id) {
                            | None => treeData // this should never happen because a symbol cannot be the root
                            | Some(parent) => {
                                if (syntaxTreeGetNumberOfSymbols(Subtree(parent)) == 1) {
                                    /* if size == 1 then the clicked symbol is a variable in the syntax definition */
                                    treeData
                                } else {
                                    treeData->updateExpLevel(true)
                                }
                            }
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
    if (state->editorStateHasCriticalErrors) {
        Error(
            `Cannot perform renumbering because there is an error in the editor content.`
                ++ ` Please resolve the error before renumbering.`
        )
    } else {
        let tmpPrefix = "###tmp###"
        let idsToRenumberArr = state.stmts
            ->Array.filter(isStmtToRenumber)
            ->Array.map(stmt => stmt.id)
        let idsToRenumberSet = idsToRenumberArr->Belt_HashSetString.fromArray

        //step 1: assign temporary labels to all the renumberable statements in order to make all numeric labels not used
        let res = state.stmts->Array.reduce(
            Ok(state),
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
            }
        )

        //step 2: assign final labels to each renumberable statement
        let res = idsToRenumberArr->Array.reduce(
            res,
            (res,stmtId) => {
                switch res {
                    | Ok(st) => st->renameStmt(stmtId, st->createNewLabel(~prefix, ~forHyp))
                    | err => err
                }
            }
        )

        switch res {
            | Error(_) => res
            | Ok(state) => {
                let state = state->prepareEditorForUnification
                if (state->editorStateHasCriticalErrors) {
                    Error( "Cannot renumber steps due to an internal error." 
                        ++ " An error was detected in the editor content after a renumbering attempt." 
                        ++ " All errors detected are: " ++ editorStateGetTextDescriptionOfAllCriticalErrors(state) )
                } else {
                    Ok(state)
                }
            }
        }
    }
}

let renumberProvableSteps = (state:editorState):result<editorState, string> => {
    state->renumberSteps(
        ~isStmtToRenumber = stmt => stmt.typ == P && !stmt.isGoal && stmt.label->containsOnlyDigits,
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
    if (syntaxTypes->Array.length == 0) {
        Error(`Could not determine syntax types.`)
    } else {
        let findUndefinedSym = (syms:array<string>):option<string> => 
            syms->Array.find(sym => wrkCtx->ctxSymToInt(sym)->Belt_Option.isNone)
        switch Belt_Array.concatMany(syms)->findUndefinedSym {
            | Some(unrecognizedSymbol) => Error(`Unrecognized symbol: '${unrecognizedSymbol}'`)
            | None => {
                let lastSyntaxTypeInt = lastSyntaxType->Belt.Option.flatMap(ctxSymToInt(wrkCtx, _))->Belt.Option.getWithDefault(0)
                let syntaxTypes = syntaxTypes->Array.copy->Expln_utils_common.sortInPlaceWith((a,b) => {
                    if (a == lastSyntaxTypeInt) {
                        -1.0
                    } else if (b == lastSyntaxTypeInt) {
                        1.0
                    } else {
                        Belt_Float.fromInt(a - b)
                    }
                })
                let exprs = syms->Array.map(ctxSymsToIntsExn(wrkCtx, _))
                let proofTree = MM_provers.proveSyntaxTypes(
                    ~wrkCtx=wrkCtx, ~frms, ~parenCnt, ~exprs, ~syntaxTypes, ~frameRestrict
                )
                let typeStmts = exprs->Array.map(expr => {
                    switch proofTree->ptGetSyntaxProof(expr) {
                        | None => None
                        | Some(node) => Some(node->pnGetExpr)
                    }
                })
                let proofTreeDto = proofTree->MM_proof_tree_dto.proofTreeToDto(
                    typeStmts->Array.filter(Belt_Option.isSome(_))->Array.map(Belt_Option.getExn(_))
                )
                switch typeStmts->Array.find(Belt_Option.isSome(_)) {
                    | None => ()
                    | Some(None) => ()
                    | Some(Some(typeStmt)) => {
                        switch lastSyntaxType {
                            | None => wrkCtx->ctxIntToSym(typeStmt->Array.getUnsafe(0))->Belt_Option.forEach(onLastSyntaxTypeChange)
                            | Some(lastSyntaxType) => {
                                wrkCtx->ctxIntToSym(typeStmt->Array.getUnsafe(0))->Belt_Option.forEach(provedSyntaxType => {
                                    if (lastSyntaxType != provedSyntaxType) {
                                        onLastSyntaxTypeChange(provedSyntaxType)
                                    }
                                })
                            }
                        }
                    }
                }
                Ok(
                    typeStmts->Array.map(typeStmt => {
                        switch typeStmt {
                            | None => {
                                Error(
                                    `Could not prove this statement is of any of the types: ` 
                                        ++ `${wrkCtx->ctxIntsToSymsExn(syntaxTypes)->Array.joinUnsafe(", ")}`
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
                proofTables->Array.map(proofTable => {
                    switch proofTable {
                        | Error(msg) => Error(msg)
                        | Ok(proofTable) => buildSyntaxTree(wrkCtx, proofTable, proofTable->Array.length-1)
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

let deleteUnrelatedSteps = (
    state:editorState, 
    ~stepIdsToKeep:array<stmtId>, 
    ~deleteHyps:bool
):result<editorState, string> => {
    let state = state->prepareEditorForUnification
    if (state->editorStateHasCriticalErrors) {
        Error(
            `Cannot perform deletion because there is an error in the editor content.`
                ++ ` Please resolve the error before deleting steps.`
        )
    } else {
        let unprocessedIds = Belt.MutableQueue.fromArray(stepIdsToKeep)
        state.stmts->Array.forEach(stmt => {
            if ((!deleteHyps && stmt.typ == E) || stmt.isGoal || stmt.isBkm) {
                unprocessedIds->Belt_MutableQueue.add(stmt.id)
            }
        })
        let idsToKeep = Belt_HashSetString.make(~hintSize = stepIdsToKeep->Array.length*5)
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
                                args->Array.forEach(label => {
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
            ->Array.map(stmt => stmt.id)
            ->Array.filter(id => !(idsToKeep->Belt_HashSetString.has(id)))
        Ok(state->deleteStmts(idsToRemove))
    }
}