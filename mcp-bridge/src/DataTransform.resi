// Pure transformations of data returned by metamath-lamp's API:
// mostly conversions to compact plain text for an AI agent.

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

// A found assertion, as returned by api.explorer.findAssertions().
type foundAssertion = {
    label: string,
    isAxiom: bool,
    isDisc: bool,
    isDepr: bool,
    disj: array<array<string>>,
    hyps: array<string>,
    asrt: string,
}

/*
* Converts one found assertion to the text format of the find_assertions tool.
* */
let assertionToText: foundAssertion => string

// The result of api.context.getLoadedContextInfo()
// (see getLoadedContextInfo() in src/metamath/api/MM_api_context.res).
type ctxFileInfo = {
    fileName: string,
    numOfAsrts: int,
    numOfAxioms: int,
}

type ctxSrcInfo = {
    // "web" or "local".
    @as("type") type_: string,
    // Empty for local sources.
    url: string,
    // Empty for web sources.
    fileName: string,
    alias: Nullable.t<string>,
    // "readAll", "stopBefore" or "stopAfter".
    readInstr: string,
    // null for readAll.
    label: Nullable.t<string>,
    numOfAsrts: int,
    numOfAxioms: int,
    // Empty if the source doesn't include other files.
    files: array<ctxFileInfo>,
}

type loadedContextInfo = {
    numOfAsrts: int,
    numOfAxioms: int,
    srcs: array<ctxSrcInfo>,
}

type loadedContextInfoToTextParams = {
    info: loadedContextInfo,
    // Returns the local path of a file served by this MCP server, or None for other URLs.
    urlToLocalPath: string => option<string>,
}

/*
* Converts the information about the loaded context to the text format of the get_loaded_context_info tool.
* */
let loadedContextInfoToText: loadedContextInfoToTextParams => string

type getInsertIdxParams = {
    // Steps of the editor, as in the result of api.editor().getState().
    steps: array<step>,
    beforeLabel?: string,
    afterLabel?: string,
}

// Either idx or err is defined.
type insertIdxResult = {
    idx?: int,
    err?: string,
}

/*
* Returns the index in the editor where new steps should be inserted, or an error message.
* beforeLabel takes precedence over afterLabel.
* If neither is given, new steps go before the goal step, or to the end if there is no goal step.
* */
let getInsertIdx: getInsertIdxParams => insertIdxResult
