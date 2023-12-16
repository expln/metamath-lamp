type api = (string, Js.Json.t) => Js_json.t

let api: unit => option<api>

let fun: {
    "getAllLabels": string, 
    "proveBottomUp": string
}

type proverParams = {
    delayBeforeStartMs:int,
    stmtId: MM_wrk_editor.stmtId,
    bottomUpProverParams: MM_provers.bottomUpProverParams,
}

let updateEditorApi: (
  ~state: MM_wrk_editor.editorState,
  ~showError: string => unit,
  ~canStartProvingBottomUp:bool,
  ~startProvingBottomUp:proverParams=>unit,
) => unit
