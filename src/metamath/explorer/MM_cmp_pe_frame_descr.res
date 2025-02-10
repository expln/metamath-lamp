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
open MM_comment_parser

type props = {
    settings:settings,
    preCtx:mmContext,
    openFrameExplorer:option<string=>unit>,
    text:string,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settings === b.settings
        && a.preCtx === b.preCtx
}

let make = React.memoCustomCompareProps( ({
    settings,
    preCtx,
    openFrameExplorer,
    text,
}:props) =>  {

    let textParts = splitCommentIntoParts(text)

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch preCtx->getFrame(label) {
            | None => None
            | Some(frame) => MM_react_common.getFrmLabelBkgColor(frame, settings)
        }
    }

    let rndLabelOrUrl = (~key:string, ~label:string) => {
        /* Metamath.pdf, page 142:
            When generating html, the tokens after the tilde must be a URL (either
            http: or https:) or a valid label.
        */
        switch preCtx->getTokenType(label) {
            | Some(A) | Some(P) => {
                <span
                    key
                    style=ReactDOM.Style.make(
                        ~cursor="pointer", 
                        ~backgroundColor=?getFrmLabelBkgColor(label),
                        ~borderRadius="3px",
                        ~color="blue",
                        ~textDecoration="underline",
                        ()
                    )
                    onClick=clickHnd(
                        ~act=()=>openFrameExplorer->Option.forEach(openFrameExplorer => openFrameExplorer(label))
                    )
                >
                    {label->React.string}
                </span>
            }
            | Some(C) | Some(V) | Some(F) | Some(E) | None => {
                if (label->String.startsWith("http:") || label->String.startsWith("https:")) {
                    <a 
                        key
                        href=label 
                        target="_blank"
                        style=ReactDOM.Style.make( ~cursor="pointer", ~color="blue", ~textDecoration="underline", () )
                    >
                        {label->React.string}
                    </a>
                } else {
                    label->React.string
                }
            }
        }
    }

    textParts->Array.mapWithIndex((part,idx) => {
        switch part {
            | Text(str) => React.string(str)
            | MathMode(str) => React.string(str)
            | LabelMode(str) => rndLabelOrUrl(~key=idx->Int.toString, ~label=str)
        }
    })->React.array


}, propsAreSame)