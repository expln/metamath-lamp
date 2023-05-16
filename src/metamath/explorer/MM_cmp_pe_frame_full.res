open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_parser

type state = {
    isLoaded:bool,
    ctx:mmContext,
}

let makeInitialState = () => {
    {
        isLoaded:false,
        ctx:createContext(()),
    }
}

let loadFrameContext = (
    ~srcs:array<mmCtxSrcDto>,
    ~label:string,
    ~onProgress:float=>unit,
    ~onDone: result<mmContext,string>=>unit,
):unit => {
    beginLoadingMmContext(
        ~scopes = createMmScopesForFrame( ~srcs, ~label, ),
        ~onProgress,
        ~onDone,
        ~dontChangeNestingLevelForLastElem=true,
        ()
    )
}

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    label,
}:props) => {
    let (state, setState) = React.useState(() => makeInitialState())

    if (!state.isLoaded) {
        "Loading..."->React.string
    } else {
        "Loaded"->React.string
    }

}, propsAreSame)