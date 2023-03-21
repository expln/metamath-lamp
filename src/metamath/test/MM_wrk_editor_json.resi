open MM_wrk_editor
open MM_wrk_settings
open MM_context

type userStmtLocStor = {
    label: string, 
    typ: string, 
    cont: string, 
    jstfText: string
}

type editorStateLocStor = {
    varsText: string, 
    disjText: string, 
    stmts: array<userStmtLocStor>
}

let userStmtLocStorToUserStmt: userStmtLocStor => userStmt

let createInitialEditorState: (
    ~settingsV:int, 
    ~settings:settings, 
    ~preCtxV:int, 
    ~preCtx:mmContext, 
    ~stateLocStor:option<editorStateLocStor>
) => editorState

let editorStateToEditorStateLocStor: editorState => editorStateLocStor

let readEditorStateFromJsonStr: string => option<editorStateLocStor>
