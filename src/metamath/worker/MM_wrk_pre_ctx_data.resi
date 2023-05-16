open MM_context
open MM_wrk_ctx_data
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open Common
open MM_parser

type mmCtxSrcDto = {
    typ: string, 
    fileName: string, 
    url: string, 
    readInstr: string, 
    label: string,
    ast: option<mmAstNode>,
    allLabels: array<string>,
}

type preCtxData = {
    settingsV: version<settings>,
    srcs: array<mmCtxSrcDto>,
    ctxV: version<mmContext>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes: array<int>,
}

let preCtxDataMake: (~settings: settings) => preCtxData

let preCtxDataUpdate: (
  preCtxData,
  ~settings: settings=?,
  ~ctx: (array<mmCtxSrcDto>, mmContext)=?,
  unit,
) => preCtxData

let findSyntaxTypes: (mmContext, Belt_MapString.t<frmSubsData>) => array<int>