open MM_context
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_asrt_apply

type stmtCont =
    | Text(array<string>)
    | Tree(syntaxTreeNode)

let contIsEmpty: stmtCont => bool
let contToArrStr: stmtCont => array<string>
let contToStr: stmtCont => string
let strToCont: string => stmtCont

type userStmtType = [#e | #p]
let userStmtTypeFromStr: string => userStmtType

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
    proof: option<proofTreeNode>,
    proofStatus: option<proofStatus>,
}

let createEmptyUserStmt: (string, userStmtType, string) => userStmt

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

let updateStmt: (editorState, string, userStmt => userStmt) => editorState
let isStmtChecked: (editorState, string) => bool
let toggleStmtChecked: (editorState, string) => editorState
let checkAllStmts: editorState => editorState
let uncheckAllStmts: editorState => editorState
let deleteCheckedStmts: editorState => editorState
let canMoveCheckedStmts: (editorState, bool) => bool
let moveCheckedStmts: (editorState, bool) => editorState
let getAllStmtsUpToChecked: editorState => array<userStmt>
let addNewStmt: editorState => (editorState,string)
let isSingleStmtChecked: editorState => bool
let duplicateCheckedStmt: editorState => editorState
let canGoEditModeForStmt: (editorState, string) => bool
let setVarsEditMode: editorState => editorState
let completeVarsEditMode: (editorState, string) => editorState
let setDisjEditMode: editorState => editorState
let completeDisjEditMode: (editorState, string) => editorState
let setLabelEditMode: (editorState, string) => editorState
let completeLabelEditMode: (editorState, string, Js_string2.t) => editorState
let setContEditMode: (editorState, string) => editorState
let completeContEditMode: (editorState, string, stmtCont) => editorState
let setTypEditMode: (editorState, string) => editorState
let completeTypEditMode: (editorState, string, userStmtType) => editorState
let setJstfEditMode: (editorState, string) => editorState
let completeJstfEditMode: (editorState, string, string) => editorState
let setSettings: (editorState, int, settings) => editorState
let setPreCtx: (editorState, int, mmContext) => editorState
let editorStateHasErrors: editorState => bool
let refreshWrkCtx: editorState => editorState
let prepareProvablesForUnification: editorState => editorState
let prepareEditorForUnification: editorState => editorState
let addAsrtSearchResult: (editorState, applyAssertionResult) => editorState
let findPossibleSubs: (editorState, expr, expr) => array<Belt_MapInt.t<expr>>
let applySubstitutionForEditor: (editorState, wrkSubs) => editorState
let removeUnusedVars: editorState => editorState
let applyUnifyAllResults: (editorState,proofTreeDto) => editorState