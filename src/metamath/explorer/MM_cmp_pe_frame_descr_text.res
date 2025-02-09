open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open Expln_React_Modal
open Common
open MM_cmp_pe_frame_summary_state
open MM_wrk_settings

type props = {
    preCtx:mmContext,
    openFrameExplorer:option<string=>unit>,
    text:string,
}

let propsAreSame = (a:props,b:props):bool => {
    a.preCtx === b.preCtx
}

let make = React.memoCustomCompareProps( ({
    preCtx,
    openFrameExplorer,
    text,
}:props) =>  {

    React.null

}, propsAreSame)