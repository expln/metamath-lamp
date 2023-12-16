type api = (string, Js.Json.t) => Js_json.t

let api: unit => option<api>

let fun: {
    "getAllLabels": string, 
    "proveBottomUp": string
}

let updateEditorApi: (
  ~state: MM_wrk_editor.editorState,
//   ~setState: (MM_wrk_editor.editorState => MM_wrk_editor.editorState) => unit,
  ~showError: string => unit,
) => unit
