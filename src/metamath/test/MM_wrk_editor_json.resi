open MM_wrk_editor
open MM_wrk_settings
open MM_context
open MM_wrk_pre_ctx_data

type userStmtLocStor = {
    label: string, 
    typ: string, 
    isGoal: bool,
    cont: string, 
    jstfText: string
}

type editorStateLocStor = {
    srcs: array<mmCtxSrcDto>,
    descr: string,
    varsText: string, 
    disjText: string, 
    stmts: array<userStmtLocStor>
}

let userStmtLocStorToUserStmt: userStmtLocStor => userStmt

let createInitialEditorState: (
    ~settingsV:int, 
    ~settings:settings, 
    ~srcs:array<mmCtxSrcDto>,
    ~preCtxV:int, 
    ~preCtx:mmContext, 
    ~stateLocStor:option<editorStateLocStor>
) => editorState

let editorStateToEditorStateLocStor: editorState => editorStateLocStor

let readEditorStateFromJsonStr: string => result<editorStateLocStor,string>
