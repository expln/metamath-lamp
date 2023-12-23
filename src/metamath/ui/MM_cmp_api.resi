open MM_wrk_editor

type api = Js.Json.t => promise<Js_json.t>

let api: {
    "editor" : {
        "getState": api,
        "proveBottomUp": api,
        "unifyAll": api,
        "addSteps": api,
        "updateSteps": api,
        "getTokenType": api,
        "substitute": api,
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
    ~showError: string => unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
) => unit
