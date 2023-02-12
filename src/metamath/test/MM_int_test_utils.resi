open MM_wrk_editor
open MM_provers
open MM_proof_tree_dto
open MM_statements_dto

let setMmPath: string
let failOnMismatch: bool

let createEditorState: ( ~mmFilePath: string, ~stopBefore: string=?, ~stopAfter: string=?, unit, ) => editorState

let addStmt: ( editorState, ~typ: userStmtType=?, ~label: string=?, ~stmt: string, unit, ) => (editorState, string)

let duplicateStmt: ( editorState, string, ) => (editorState, string)

let updateStmt: (
  editorState,
  string,
  ~label: string=?,
  ~typ: MM_wrk_editor.userStmtType=?,
  ~content: string=?,
  ~contReplaceWhat: string=?,
  ~contReplaceWith: string=?,
  unit,
) => editorState

let addStmtsBySearch: (
  editorState,
  ~addBefore: string=?,
  ~filterLabel: string=?,
  ~filterTyp: string=?,
  ~filterPattern: string=?,
  ~chooseLabel: string,
  unit,
) => editorState

let applySubstitution: ( editorState, ~replaceWhat: string, ~replaceWith: string, ) => editorState

let unifyAll: editorState => editorState
let unifyBottomUp: (
    editorState,
    string,
    ~asrtLabel:string=?,
    ~maxSearchDepth:int=?,
    ~lengthRestriction:lengthRestrict=?,
    ~allowNewVars:bool=?,
    ~useRootStmtsAsArgs:bool=?,
    ~chooseLabel:string,
    ()
) => (editorState, array<newStmtsDto>)
let arrNewStmtsDtoToStr: array<MM_statements_dto.newStmtsDto> => string

let setTestDataDir: string => unit
let assertEditorState: (editorState, string) => unit
let assertProof: (editorState, string, string) => unit
let assertTextsEq: (string, string, string, string) => unit
let assertTextEqFile: (string, string) => unit
let getStmtId: (editorState, ~contains: string) => string