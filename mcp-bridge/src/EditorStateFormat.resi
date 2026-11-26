// Converts the editor state returned by metamath-lamp's api.editor().getState()
// to compact plain text for an AI agent.

// The parts of the result of api.editor().getState() which are used here
// (see getEditorStateDict() in src/metamath/api/MM_api_editor.res).
type stmtErr = {
    msg: Nullable.t<string>,
}

type step = {
    label: string,
    stmt: string,
    isHyp: bool,
    isGoal: bool,
    isBkm: bool,
    // "v", "~", "?", "x" or null.
    status: Nullable.t<string>,
    jstfText: Nullable.t<string>,
    stmtErr: Nullable.t<stmtErr>,
    syntaxErr: Nullable.t<string>,
    unifErr: Nullable.t<string>,
}

type editorState = {
    tabId: string,
    scope: string,
    scopeErr: Nullable.t<string>,
    // Present only if requested with includeDescription.
    descr: Nullable.t<string>,
    varsText: Nullable.t<string>,
    varsErr: Nullable.t<string>,
    disjText: Nullable.t<string>,
    disjErr: Nullable.t<string>,
    selectedSteps: array<string>,
    steps: array<step>,
}

type editorStateToTextParams = {
    // The result of api.editor().getState()
    state: editorState,
    // If true, proved non-bookmarked steps without errors are omitted.
    onlyRelevantSteps?: bool,
    // Additional lines to show at the top.
    notes?: array<string>,
}

let editorStateToText: editorStateToTextParams => string
