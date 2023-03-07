open MM_wrk_editor
open MM_statements_dto

let setMmPath: string
let failOnMismatch: bool
let debug: bool

let newStmtsDtoToStr: stmtsDto => string

let setTestDataDir: string => unit
let assertEditorState: (editorState, string) => unit
let assertStmtsDto: (stmtsDto, string) => unit
let assertProof: (editorState, string, string) => unit
let assertTextsEq: (string, string, string, string) => unit
let assertTextEqFile: (string, string) => unit
let readEditorStateToString: string => string

let generateReducedMmFile: (
    ~pathToFullMmFile:string,
    ~pathToSaveTo:string,
    ~skipComments:bool=?,
    ~skipProofs:bool=?,
    ()
) => unit