open MM_wrk_editor
open MM_statements_dto
open MM_progress_tracker
open MM_editor_history

let setMmPath: string
let asrtsToSkipFilePath: string
let failOnMismatch: bool
let debug: bool

let newStmtsDtoToStr: stmtsDto => string

let setTestDataDir: string => unit
let getTestDataDir: () => string
let assertNoErrors: editorState => unit
let assertStrEqFile: (string, string) => unit
let assertEditorState: (editorState, string) => unit
let assertStmtsDto: (stmtsDto, string) => unit
let assertEditorHistory: (editorHistory, string) => unit
let assertProof: (editorState, string, string) => unit
let assertTextsEq: (string, string, string, string) => unit
let assertTextEqFile: (string, string) => unit
let assertFileContentsEq: (string, string) => unit
let readTestFileToString: string => string

let generateReducedMmFile: (
    ~pathToFullMmFile:string,
    ~pathToSaveTo:string,
    ~skipComments:bool=?,
    ~skipProofs:bool=?,
    ()
) => unit

let countFrames: (
  MM_parser.mmAstNode,
  ~stopBefore: string=?,
  ~stopAfter: string=?,
  unit,
) => int

let testProgressTrackerMake: (
  ~step: float,
  ~maxCnt: int,
) => progressStateInt

let testProgressTrackerIncCnt: progressStateInt => unit