open MM_wrk_pre_ctx_data

let procName: string

type mmScope = {
  ast: MM_parser.mmAstNode,
  expectedNumOfAssertions: int,
  stopBefore: option<string>,
  stopAfter: option<string>,
  resetNestingLevel:bool,
}

type request
type response

let beginLoadingMmContext: ( 
    ~scopes: array<mmScope>, 
    ~onProgress: float => unit, 
    ~onDone: result<MM_context.mmContext, string> => unit,
    ()
) => unit

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

let reqToStr: request => string
let respToStr: response => string

let createMmScopesForFrame: ( ~srcs:array<mmCtxSrcDto>, ~label:string, ) => array<mmScope>