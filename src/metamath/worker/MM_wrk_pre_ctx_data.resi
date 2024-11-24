open MM_context
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
    resetNestingLevel:bool,
    ast: option<mmAstNode>,
    allLabels: array<string>,
}

type preCtxData = {
    settingsV: version<settings>,
    srcs: array<mmCtxSrcDto>,
    ctxV: version<mmContext>,
    ctxMinV: version<mmContext>,
    frms: frms,
    parenCnt: parenCnt,
    allTypes:array<int>,
    syntaxTypes:array<int>,
    parensMap: Belt_HashMapString.t<string>,
    typeOrderInDisj:Belt_HashMapInt.t<int>,
    typeColors: Belt_HashMapString.t<string>,
    symColors: Belt_HashMapString.t<string>,
}

let preCtxDataMake: (~settings: settings) => preCtxData

let preCtxDataUpdate: (
  preCtxData,
  ~settings: settings=?,
  ~ctx: (array<mmCtxSrcDto>, mmContext)=?,
) => preCtxData

let findTypes: mmContext => (array<int>,array<int>)

let createSymbolColors: (~ctx:mmContext, ~typeColors: Belt_HashMapString.t<string>) => Belt_HashMapString.t<string>