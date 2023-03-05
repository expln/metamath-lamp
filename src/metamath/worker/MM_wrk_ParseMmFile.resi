let procName: string

type parseResult = result<(MM_parser.mmAstNode, array<string>), string>

type request
type response

let beginParsingMmFile: ( ~mmFileText: string, ~onProgress: float => unit, ~onDone: parseResult => unit, ) => unit
let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let reqToStr: request => string
let respToStr: response => string
