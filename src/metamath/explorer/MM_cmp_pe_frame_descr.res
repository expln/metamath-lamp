open MM_react_common
open MM_context
open Common
open MM_wrk_settings
open MM_comment_parser

type props = {
    settings:settings,
    ctx:mmContext,
    symColors: Belt_HashMapString.t<string>,
    openFrameExplorer:string=>unit,
    text:string,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settings === b.settings
        && a.ctx === b.ctx
        && a.symColors === b.symColors
        && a.text == b.text
}

let make = React.memoCustomCompareProps( ({
    settings,
    ctx,
    symColors,
    openFrameExplorer,
    text,
}:props) =>  {

    let textParts = splitCommentIntoParts(text)

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch ctx->getFrame(label) {
            | None => None
            | Some(frame) => MM_react_common.getFrmLabelBkgColor(frame, settings)
        }
    }

    let rndLabelOrUrl = (~key:string, ~label:string) => {
        /* Metamath.pdf, page 142:
            When generating html, the tokens after the tilde must be a URL (either
            http: or https:) or a valid label.
        */
        switch ctx->getTokenType(label) {
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
                    onClick=clickHnd( ~act=()=>openFrameExplorer(label) )
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

    let rndExpr = (~key:string, ~expr:string) => {
        <span key style=ReactDOM.Style.make( ~fontFamily="monospace", () ) >
            {
                getSpaceSeparatedValuesAsArray(expr)->Array.mapWithIndex((sym,idx) => {
                    let (color,fontWeight) = switch symColors->Belt_HashMapString.get(sym) {
                        | None => ("black","normal")
                        | Some(color) => (color,"bold")
                    }
                    <span 
                        key={sym ++ "$" ++ idx->Int.toString} 
                        style=ReactDOM.Style.make( ~color, ~fontWeight, ~fontSize="1.3em", () )
                    >
                        {React.string((idx>0?" ":"")++sym)}
                    </span>
                })->React.array
            }
        </span>
    }

    textParts->Array.mapWithIndex((part,idx) => {
        switch part {
            | Text(str) => React.string(str)
            | MathMode(str) => rndExpr(~key=idx->Int.toString, ~expr=str)
            | LabelMode(str) => rndLabelOrUrl(~key=idx->Int.toString, ~label=str)
        }
    })->React.array


}, propsAreSame)