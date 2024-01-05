open MM_wrk_editor
open MM_syntax_tree

type apiResp = {
    "isOk": bool,
    "res": option<Js_json.t>,
    "err": option<string>,
}

type api = Js.Json.t => promise<apiResp>

let api: {
    "setLogApiCallsToConsole": api,
    "editor" : {
        "getState": api,
        "proveBottomUp": api,
        "unifyAll": api,
        "addSteps": api,
        "updateSteps": api,
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
    bottomUpProverParams: array<MM_provers.bottomUpProverParams>,
    selectFirstFoundProof:bool,
}

let updateEditorApi: (
    ~state: editorState,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
) => unit
