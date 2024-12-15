open MM_wrk_editor
open MM_wrk_pre_ctx_data

type userStmtLocStor = {
    label: string, 
    typ: string, 
    isGoal: bool,
    isBkm:bool,
    cont: string,
    jstfText: string,
}

type editorStateLocStor = {
    tabTitle: string,
    srcs: array<mmCtxSrcDto>,
    descr: string,
    varsText: string, 
    disjText: string, 
    stmts: array<userStmtLocStor>
}

let userStmtLocStorToUserStmt: userStmtLocStor => userStmt

let createInitialEditorState: (
    ~preCtxData:preCtxData,
    ~stateLocStor:option<editorStateLocStor>
) => editorState

let editorStateToEditorStateLocStor: editorState => editorStateLocStor

let readEditorStateFromJsonStr: string => result<editorStateLocStor,string>

let makeEmptyEditorStateLocStor: (~tabTitle:string=?) => editorStateLocStor