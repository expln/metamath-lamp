open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_pre_ctx_data

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
}

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