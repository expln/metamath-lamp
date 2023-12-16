open MM_wrk_editor

type api = (string,Js.Json.t) => Js_json.t

let apiRef:ref<option<api>> = ref(None)
let api = ():option<api> => apiRef.contents

let getAllLabelsStr = "getAllLabels"
let proveBottomUpStr = "proveBottomUp"
let fun = {
    "getAllLabels": getAllLabelsStr,
    "proveBottomUp": proveBottomUpStr,
}

let getAllLabels = (~state:editorState):Js_json.t => {
    state.stmts->Js.Array2.map(stmt => stmt.label->Js.Json.string)->Js.Json.array
}

let proveBottomUp = (
    ~paramsJson:Js_json.t,
    ~showError:string=>unit,
):Js_json.t => {
    open Expln_utils_jsonParse
    let parseResult = fromJson(paramsJson, asObj(_,d=>{
        {
            "asrtsToUse": d->strOpt("asrtsToUse", ())
        }
    }, ()), ())
    switch parseResult {
        | Error(msg) => showError(msg)
        | Ok(parsedParams) => Js.Console.log2(`parsedParams`, parsedParams)
    }
    Js.Json.null
}

let makeEditorApi = (
    ~state:editorState,
    // ~setState:(editorState=>editorState)=>unit,
    ~showError:string=>unit,
):api => (funcName,paramsJson) => {
    if (funcName == getAllLabelsStr) {
        getAllLabels(~state)
    } else if (funcName == proveBottomUpStr) {
        proveBottomUp(~paramsJson, ~showError)
    } else {
        showError(`Unknown api function ${funcName}`)
        Js.Json.null
    }
}

let updateEditorApi = (
    ~state:editorState,
    // ~setState:(editorState=>editorState)=>unit,
    ~showError:string=>unit,
):unit => {
    apiRef := Some(
        makeEditorApi(
            ~state,
            // ~setState,
            ~showError,
        )
    )
}