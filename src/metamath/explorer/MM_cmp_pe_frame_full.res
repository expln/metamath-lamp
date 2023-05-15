open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_pre_ctx_data
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

let getAllLabelsAfterReading = (src:mmCtxSrcDto) => {
    switch src.readInstr->readInstrFromStr {
        | ReadAll => src.allLabels
        | StopBefore => {
            switch src.allLabels->Js_array2.findIndex(label => label == src.label) {
                | -1 => src.allLabels
                | idx => src.allLabels->Js_array2.slice(~start=0, ~end_=idx)
            }
        }
        | StopAfter => {
            switch src.allLabels->Js_array2.findIndex(label => label == src.label) {
                | -1 => src.allLabels
                | idx => src.allLabels->Js_array2.slice(~start=0, ~end_=idx+1)
            }
        }
    }
}

let convertSrcDtoAndAddToRes = (~src:mmCtxSrcDto, ~label:string, ~res:array<mmScope>):bool => {
    let allLabels = getAllLabelsAfterReading(src)
    let (expectedNumOfAssertions, stopBefore) =
        if (allLabels->Js_array2.includes(label)) {
            (allLabels->Js_array2.indexOf(label), Some(label))
        } else {
            (allLabels->Js_array2.length, None)
        }
    let ast = switch src.ast {
        | Some(ast) => ast
        | _ => raise(MmException({msg:`Cannot create MM context for a frame without ast.`}))
    }
    let mmScope = {
        ast,
        expectedNumOfAssertions,
        stopBefore,
        stopAfter: None,
    }
    res->Js_array2.push(mmScope)->ignore
    allLabels->Js_array2.length != expectedNumOfAssertions
}

let createMmScopesToLoad = ( ~srcs:array<mmCtxSrcDto>, ~label:string, ):array<mmScope> => {
    let res = []
    srcs->Js_array2.reduce(
        (found,src) => {
            if (found) {
                found
            } else {
                convertSrcDtoAndAddToRes(~src, ~label, ~res)
            }
        },
        false
    )->ignore
    res
}

let loadFrameContext = (
    ~srcs:array<mmCtxSrcDto>,
    ~label:string,
    ~onProgress:float=>unit,
    ~onDone: result<mmContext,string>=>unit,
):unit => {
    MM_wrk_LoadCtx.beginLoadingMmContext(
        ~scopes = createMmScopesToLoad( ~srcs, ~label, ),
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