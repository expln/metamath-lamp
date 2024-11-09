open MM_context
open MM_proof_tree
open MM_proof_tree_dto
open MM_syntax_tree
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_statements_dto
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

let defaultJstfForHyp:string

let contIsEmpty: stmtCont => bool
let contToArrStr: stmtCont => array<string>
let contToStr: stmtCont => string
let strToCont: (
    string,
    ~preCtxColors: Belt_HashMapString.t<string>=?,
    ~wrkCtxColors: Belt_HashMapString.t<string>=?
) => stmtCont

type userStmtType = E | P
type userStmtTypeExtended = H | P | G
let userStmtTypeFromStr: string => userStmtType
let userStmtTypeToStr: userStmtType => string
let userStmtTypeExtendedFromStrExn: string => userStmtTypeExtended

let readInstrToStr: readInstr => string
let readInstrFromStr: string => readInstr
let mmFileSourceTypeToStr: mmFileSourceType => string
let mmFileSourceTypeFromStr: string => mmFileSourceType
let mmFileSourceTypeFromStrOpt: string => option<mmFileSourceType>

type stmtId = string

type proofStatus = Ready | Waiting | NoJstf | JstfIsIncorrect

type stmtGenericError = {
    code:int,
    msg:string,
}
let someStmtErrCode:int
let duplicatedStmtErrCode:int

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

let createEmptyUserStmt: (string, userStmtType, string, bool) => userStmt
let userStmtToRootStmt: userStmt => rootStmt

let updateStmt: (editorState, stmtId, userStmt => userStmt) => editorState
let isStmtChecked: (editorState, stmtId) => bool
let toggleStmtChecked: (editorState, stmtId) => editorState
let toggleStmtCheckedWithShift: (editorState, stmtId, ~showBkmOnly:bool) => editorState
let checkAllStmts: editorState => editorState
let uncheckAllStmts: editorState => editorState
let deleteCheckedStmts: editorState => editorState
let deleteStmt: (editorState, stmtId) => editorState
let deleteStmts: (editorState, array<stmtId>) => editorState
let canMoveCheckedStmts: (editorState, bool) => bool
let moveCheckedStmts: (editorState, bool) => editorState
let moveCheckedStmtsToTop: editorState => editorState
let moveCheckedStmtsToBottom: editorState => editorState
let getRootStmtsForUnification: editorState => array<userStmt>
let addNewStmt: (editorState, ~isHyp:bool=?, ~isBkm:bool=?) => (editorState,stmtId)
let addNewStatements: (editorState, stmtsDto, ~isBkm:bool=?) => editorState
let isSingleStmtChecked: editorState => bool
let duplicateCheckedStmt: (editorState,bool) => editorState
let bookmarkCheckedStmts: (editorState, bool) => editorState
let canGoEditModeForStmt: (editorState, string) => bool
let setDescrEditMode: editorState => editorState
let completeDescrEditMode: (editorState, string) => editorState
let setVarsEditMode: editorState => editorState
let completeVarsEditMode: (editorState, string) => editorState
let setDisjEditMode: editorState => editorState
let completeDisjEditMode: (editorState, string) => editorState
let setLabelEditMode: (editorState, string) => editorState
let completeLabelEditMode: (editorState, string, string) => editorState
let setContEditMode: (editorState, string) => editorState
let completeContEditMode: (editorState, stmtId, string) => editorState
let setStmtCont: (editorState, stmtId, stmtCont) => editorState
let setTypEditMode: (editorState, stmtId) => editorState
let completeTypEditMode: (editorState, stmtId, userStmtType, bool) => editorState
let setJstfEditMode: (editorState, string) => editorState
let completeJstfEditMode: (editorState, stmtId, string) => editorState
let setNextAction: (editorState, option<editorStateAction>) => editorState
let setPreCtxData: (editorState, preCtxData) => editorState
let editorStateHasCriticalErrors: editorState => bool
let editorStateHasAnyErrors: editorState => bool
let editorStateHasDuplicatedStmts: editorState => bool
let createNewDisj: (editorState, disjMutable) => editorState
let removeUnusedVars: editorState => editorState
let applyUnifyAllResults: (editorState,proofTreeDto) => editorState
let verifyEditorState: editorState => editorState
let generateCompressedProof: (editorState, string, ~useAllLocalEHyps:bool=?) => option<(string,string,string)>
let updateColorsInAllStmts: editorState => editorState
let getTheOnlyCheckedStmt: editorState => option<userStmt>
let editorGetStmtById: (editorState, stmtId) => option<userStmt>
let editorGetStmtByIdExn: (editorState, stmtId) => userStmt
let editorGetStmtByLabel: (editorState, string) => option<userStmt>
let getAllExprsToSyntaxCheck: (editorState, array<rootStmt>) => array<expr>
let mergeStmts: (editorState, stmtId, stmtId) => result<editorState, string>
let renameStmt: (editorState, stmtId, string) => result<editorState, string>
let findStmtsToMerge: editorState => result<(userStmt,userStmt), string>
let parseJstf: string => result<option<jstf>,string>
let removeAllTempData: editorState => editorState
let isEditMode: editorState => bool
let resetEditorContent: editorState => editorState
let deleteUnrelatedSteps: (editorState, ~stepIdsToKeep:array<stmtId>, ~deleteHyps:bool) => result<editorState, string>
let autoMergeDuplicatedStatements: (editorState, ~selectFirst:bool) => (editorState,array<(string,string)>)
let userStmtHasCriticalErrors: userStmt => bool
let userStmtHasAnyErrors: userStmt => bool

let createNewVars: (
    editorState, 
    ~varTypes:array<int>, 
    ~varNames:array<string>=?, 
    ~dontAddVariablesToContext:bool=?
) => (editorState,array<int>)
let addSteps: (
    editorState, 
    ~atIdx:int=?, 
    ~steps:array<userStmtDtoOpt>, 
    ~vars:array<(string,option<string>)>=?,
    ~dontAddVariablesToContext:bool
) => result<(editorState,array<stmtId>),string>
let updateSteps: ( editorState, array<userStmtDtoOpt>) => result<editorState,string>

let getSelectedSubtree: stmtContTreeData => option<childNode>
let getSelectedSubtreeFromStmtCont: stmtCont => option<childNode>
let getIdsOfSelectedNodes: stmtCont => (int,Belt_SetInt.t)
let hasSelectedText: stmtCont => bool
let getSelectedText: stmtCont => option<string>
let getNodeIdBySymIdx: ( ~symIdx:int, ~tree:syntaxTreeNode) => option<int>
let incExpLvl: stmtContTreeData => stmtContTreeData
let decExpLvl: stmtContTreeData => stmtContTreeData
let incExpLvlIfConstClicked: stmtContTreeData => stmtContTreeData
let getNumberOfSelectedSymbols: stmtContTreeData => int
let updateExpLevel: (stmtContTreeData, bool) => stmtContTreeData

let addColorsToSyntaxTree: (
    ~tree:syntaxTreeNode,
    ~preCtxColors:Belt_HashMapString.t<string>=?,
    ~wrkCtxColors:Belt_HashMapString.t<string>=?
) => syntaxTreeNode

let getColorForSymbol: (
    ~sym:string,
    ~preCtxColors:option<Belt_HashMapString.t<string>>,
    ~wrkCtxColors:option<Belt_HashMapString.t<string>>,
) => option<string>

let createSymbolColors: (~ctx:mmContext, ~typeColors: Belt_HashMapString.t<string>) => Belt_HashMapString.t<string>

let renumberProvableSteps: editorState => result<editorState, string>
let renumberHypothesisSteps: (editorState, ~goalLabel:string) => result<editorState, string>

let textToSyntaxProofTable: (
    ~wrkCtx:mmContext,
    ~syms:array<array<string>>,
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
) => result<array<result<MM_proof_table.proofTable,string>>,string>

let textToSyntaxTree: (
    ~wrkCtx:mmContext,
    ~syms:array<array<string>>,
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
) => result<array<result<syntaxTreeNode,string>>,string>

let recalcWrkColors: editorState => editorState