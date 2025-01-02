let procName: string

type request
type response

let reqToStr: request => string
let respToStr: response => string

let buildSyntaxTreesForAllAssertions: (
  ~settingsV: Common.version<MM_wrk_settings.settings>,
  ~preCtxVer: int,
  ~preCtx: MM_context.mmContext,
  ~onProgress: float => unit,
) => promise<array<(string, MM_syntax_tree.syntaxTreeNode)>>

let doBuildSyntaxTreesForAllAssertions: (
  ~ctx: MM_context.mmContext,
  ~frms: MM_substitution.frms,
  ~parenCnt: MM_parenCounter.parenCnt,
  ~allowedFrmsInSyntax:MM_wrk_settings.frameRestrict,
  ~syntaxTypes: array<int>,
  ~onProgress: float => unit=?,
) => array<(string, MM_syntax_tree.syntaxTreeNode)>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
