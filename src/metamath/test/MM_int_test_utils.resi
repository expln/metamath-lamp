open MM_wrk_editor

let createEditorState: (
  ~mmFilePath: string,
  ~stopBefore: string=?,
  ~stopAfter: string=?,
  unit,
) => editorState

let addStmt: (
  editorState,
  ~typ: userStmtType=?,
  ~label: string=?,
  ~stmt: string,
  unit,
) => (editorState, string)

let addStmtsBySearch: (
  editorState,
  ~addBefore: string=?,
  ~filterLabel: string=?,
  ~filterTyp: string=?,
  ~filterPattern: string=?,
  ~chooseLabel: string,
  unit,
) => editorState

let applySubstitution: (
  editorState,
  ~replaceWhat: string,
  ~replaceWith: string,
) => editorState

let unifyAll: editorState => editorState

let setTestDataDir: string => unit

let assertEditorState: (editorState, string) => unit
