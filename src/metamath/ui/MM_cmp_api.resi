type api = (string, Js.Json.t) => promise<Js_json.t>

let apiEntry: api

let fun: {
    "editor" : {
        "getState": string,
        "proveBottomUp": string,
        "unifyAll": string,
    }
}

type proverParams = {
    delayBeforeStartMs:int,
    stmtId: MM_wrk_editor.stmtId,
    debugLevel:int,
    bottomUpProverParams: MM_provers.bottomUpProverParams,
    selectFirstFoundProof:bool,
}

let updateEditorApi: (
    ~state: MM_wrk_editor.editorState,
    ~showError: string => unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
) => unit
