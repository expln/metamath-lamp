open Expln_React_Modal
open MM_wrk_pre_ctx_data
open MM_wrk_editor_json
open MM_context
open MM_proof_tree_dto
open MM_statements_dto

type vDataRec = {
    hyps: array<array<string>>,
    eHyps: array<int>,
    asrt: array<string>,
    hypLabels: array<int>,
    subs: Belt_HashMapString.t<array<string>>,
    frmColors: Belt_HashMapString.t<string>,
}
type state = {
    settings: MM_wrk_settings.settings,
    frmMmScopes: array<MM_wrk_LoadCtx.mmScope>,
    frmCtx: MM_context.mmContext,
    frms: MM_substitution.frms,
    parenCnt: MM_parenCounter.parenCnt,
    syntaxTypes: array<int>,
    typeOrderInDisj: Belt_HashMapInt.t<int>,
    frame: MM_context.frame,
    disjStr: option<array<array<(string, option<string>)>>>,
    dummyVarDisj: option<MM_context.disjMutable>,
    dummyVarDisjStr: option<array<array<(string, option<string>)>>>,
    hyps: array<MM_context.hypothesis>,
    asrt: MM_context.expr,
    proofTable: option<MM_proof_table.proofTable>,
    symColors: Belt_HashMapString.t<string>,
    vData: array<option<vDataRec>>,
    showTypes: bool,
    essIdxs: Belt_HashSetInt.t,
    stepRenum: Belt_HashMapInt.t<int>,
    expandedIdxs: array<int>,
    syntaxProofTableWasRequested: bool,
    syntaxProofTableError: option<string>,
    proofRecordsPerPage: int,
    pageIdx: int,
}
type frameProofData = state

let makeFrameProofData: (
  ~preCtxData: MM_wrk_pre_ctx_data.preCtxData,
  ~label: string,
  ~onProgress: float => unit,
) => promise<result<frameProofData, string>>

let frameProofDataToEditorStateLocStor: (
    ~preCtxData:preCtxData,
    ~frameProofData:frameProofData, 
    ~adjustContext:bool, 
    ~loadSteps:bool
) => editorStateLocStor

let frameProofDataToStmtsDto: (
    ~preCtxData:preCtxData,
    ~wrkCtx:mmContext,
    ~proofTreeDto:proofTreeDto,
    ~args:array<int>, 
    ~frameProofData:frameProofData,
) => result<stmtsDto,string>

type props = {
    top:int,
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
    openFrameExplorer:string=>unit,
    openExplorer:(~initPatternFilterStr:string=?)=>unit,
    openEditor: editorStateLocStor => unit,
    toggleCtxSelector:React.ref<Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
}

let make: React.component<props>