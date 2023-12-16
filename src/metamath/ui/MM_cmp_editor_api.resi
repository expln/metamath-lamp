type api = (string, Js.Json.t) => Js_json.t

let api: unit => option<api>

let fun: {
    "getAllLabels": string, 
    "proveBottomUp": string
}

let updateEditorApi: (
  ~state: MM_wrk_editor.editorState,
  ~showError: string => unit,
  ~canStartProvingBottomUp:bool,
  ~startProvingBottomUp:(MM_wrk_editor.stmtId, MM_provers.bottomUpProverParams)=>unit,
) => unit
