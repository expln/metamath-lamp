open MM_wrk_editor
open MM_syntax_tree
open MM_apply_asrt_matcher

type apiResp = {
    "isOk": bool,
    "res": option<JSON.t>,
    "err": option<string>,
}

type api = JSON.t => promise<apiResp>

let api: {
    "setLogApiCallsToConsole": api,
    "showInfoMsg": api,
    "showErrMsg": api,
    "editor" : {
        "getState": api,
        "proveBottomUp": api,
        "unifyAll": api,
        "addSteps": api,
        "updateSteps": api,
        "deleteSteps": api,
        "getTokenType": api,
        "substitute": api,
        "mergeDuplicatedSteps": api,
        "setContentIsHidden": api,
        "buildSyntaxTrees": api,
    }
}

type proverParams = {
    delayBeforeStartMs:int,
    stmtId: stmtId,
    debugLevel:int,
    bottomUpProverParams: MM_provers.bottomUpProverParams,
    selectFirstFoundProof:bool,
}

let updateEditorApi: (
    ~state: editorState,
    ~showInfoMsg:string=>unit,
    ~showErrMsg:string=>unit,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
) => unit

let stmtToJson: (userStmt, option<int=>string>) => JSON.t

type apiApplyAsrtResultHypMatcher = {
    label: option<string>,
    idx: option<int>,
    pat: string,
}

type apiApplyAsrtResultMatcher = {
    res: option<string>,
    hyps: array<apiApplyAsrtResultHypMatcher>,
}

let optArrayToMatchers: (
    ~state:editorState,
    ~matches:option<array<apiApplyAsrtResultMatcher>>,
) => result<option<array<applyAsrtResultMatcher>>,string>