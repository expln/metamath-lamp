open MM_wrk_editor

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
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
) => unit
