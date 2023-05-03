open MM_context
open MM_proof_tree
open MM_proof_tree_dto
open MM_syntax_tree
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_statements_dto

type mmFileSourceType = Local | Web

type readInstr = ReadAll | StopBefore | StopAfter

type webSource = {
    alias:string,
    url:string,
}

type mmFileSource =
    | Local({fileName:string})
    | Web(webSource)

type mmCtxSrcDto = {
    typ: string,
    fileName: string,
    url: string,
    readInstr: string,
    label: string,
}

type stmtSym = {
    sym: string,
    color: option<string>,
}

type stmtCont =
    | Text(array<stmtSym>)
    | Tree({exprTyp:string, root:syntaxTreeNode, selectedNodeId:option<string>})

let contIsEmpty: stmtCont => bool
let contToArrStr: stmtCont => array<string>
let contToStr: stmtCont => string
let strToCont: (
    string,
    ~preCtxColors: Belt_HashMapString.t<string>=?,
    ~wrkCtxColors: Belt_HashMapString.t<string>=?,
    ()
) => stmtCont

type userStmtType = E | P
let userStmtTypeFromStr: string => userStmtType
let userStmtTypeToStr: userStmtType => string

let readInstrToStr: readInstr => string
let readInstrFromStr: string => readInstr
let mmFileSourceTypeToStr: mmFileSourceType => string
let mmFileSourceTypeFromStr: string => mmFileSourceType
let mmFileSourceTypeFromStrOpt: string => option<mmFileSourceType>

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
    proofTreeDto: option<proofTreeDto>,
    src: option<exprSrcDto>,
    proof: option<proofNodeDto>,
    proofStatus: option<proofStatus>,
    unifErr: option<string>,
}

type editorState = {
    settingsV:int,
    settings:settings,
    typeColors: Belt_HashMapString.t<string>,

    srcs: array<mmCtxSrcDto>,
    preCtxV: int,
    preCtx: mmContext,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    preCtxColors: Belt_HashMapString.t<string>,
    syntaxTypes: array<int>,

    descr: string,
    descrEditMode: bool,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,
    wrkCtxColors: Belt_HashMapString.t<string>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkCtx: option<mmContext>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<stmtId>,

    unifyAllIsRequiredCnt: int,
}

type wrkSubsErr = 
    | CommonVar({var1:int, var2:int, commonVar:int})
    | TypeMismatch({var:int, subsExpr:expr, typeExpr:expr})

type wrkSubs = {
    newDisj: disjMutable,
    subs: Belt_MapInt.t<expr>,
    mutable err: option<wrkSubsErr>,
}

let createEmptyUserStmt: (string, userStmtType, string) => userStmt
let userStmtToRootStmt: userStmt => rootStmt

let updateStmt: (editorState, string, userStmt => userStmt) => editorState
let isStmtChecked: (editorState, string) => bool
let toggleStmtChecked: (editorState, string) => editorState
let checkAllStmts: editorState => editorState
let uncheckAllStmts: editorState => editorState
let deleteCheckedStmts: editorState => editorState
let deleteStmt: (editorState, string) => editorState
let canMoveCheckedStmts: (editorState, bool) => bool
let moveCheckedStmts: (editorState, bool) => editorState
let getRootStmtsForUnification: editorState => array<userStmt>
let addNewStmt: editorState => (editorState,string)
let isSingleStmtChecked: editorState => bool
let duplicateCheckedStmt: editorState => editorState
let canGoEditModeForStmt: (editorState, string) => bool
let setDescrEditMode: editorState => editorState
let completeDescrEditMode: (editorState, string) => editorState
let setVarsEditMode: editorState => editorState
let completeVarsEditMode: (editorState, string) => editorState
let setDisjEditMode: editorState => editorState
let completeDisjEditMode: (editorState, string) => editorState
let setLabelEditMode: (editorState, string) => editorState
let completeLabelEditMode: (editorState, string, Js_string2.t) => editorState
let setContEditMode: (editorState, string) => editorState
let completeContEditMode: (editorState, string, string) => editorState
let setStmtCont: (editorState, stmtId, stmtCont) => editorState
let setTypEditMode: (editorState, string) => editorState
let completeTypEditMode: (editorState, string, userStmtType) => editorState
let setJstfEditMode: (editorState, string) => editorState
let completeJstfEditMode: (editorState, string, string) => editorState
let incUnifyAllIsRequiredCnt: editorState => editorState
let setSettings: (editorState, int, settings) => editorState
let setPreCtx: (editorState, array<mmCtxSrcDto>, int, mmContext) => editorState
let editorStateHasErrors: editorState => bool
let prepareEditorForUnification: editorState => editorState
let addNewStatements: (editorState, stmtsDto) => editorState
let findPossibleSubs: (editorState, expr, expr) => array<wrkSubs>
let applySubstitutionForEditor: (editorState, wrkSubs) => editorState
let removeUnusedVars: editorState => editorState
let applyUnifyAllResults: (editorState,proofTreeDto) => editorState
let updateEditorStateWithPostupdateActions: (editorState, editorState => editorState) => editorState
let generateCompressedProof: (editorState, string) => option<(string,string,string)>
let updateColorsInAllStmts: editorState => editorState
let getTheOnlySelectedStmt: editorState => option<userStmt>
let editorGetStmtById: (editorState, string) => option<userStmt>
let mergeStmts: (editorState, string, string) => result<editorState, string>
let renameStmt: (editorState, string, string) => result<editorState, string>
let findStmtsToMerge: editorState => result<(userStmt,userStmt), string>
let parseJstf: string => result<option<jstf>,string>