open Expln_2d
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open MM_provers
open Local_storage_utils

type svgComp = vector => (reElem,boundaries)

let ey = ey->vecRev
let deg = fl => deg(-. fl)
let rad = fl => rad(-. fl)

let scale = 1.

let fontSize = scale *. 15.
let charLength = fontSize *. 0.6
let charHeight = charLength *. 0.85
let lineWidth = scale *. 1.
let lineWidthStr = lineWidth->Belt_Float.toString ++ "px"

let subsAvailableColors = ["green", "orange", "#03a9f4", "pink", "brown", "lawngreen", "olive", "blue", "red", "magenta"]

let viewBox = (b:boundaries):string => 
    `${b->bndMinX->Belt.Float.toString}` 
        ++ ` ${b->bndMinY->Belt.Float.toString}` 
        ++ ` ${b->bndWidth->Belt.Float.toString}`
        ++ ` ${b->bndHeight->Belt.Float.toString}`

let vecLine = (v:vector, ~color:string):(reElem,boundaries) => {
    let b = v->vecBegin
    let e = v->vecEnd
    (
        <line
            x1={b->pntX->Belt.Float.toString}
            y1={b->pntY->Belt.Float.toString}
            x2={e->pntX->Belt.Float.toString}
            y2={e->pntY->Belt.Float.toString}
            stroke=color
            strokeWidth=lineWidthStr
        />,
        bndFromPoints([v->vecBegin, v->vecEnd])
    )
}

let polyline = (~ps:array<point>, ~color:string):(reElem,boundaries) => {
    (
        <polyline 
            points={
                ps->Js_array2.map(p => `${p->pntX->Belt.Float.toString},${p->pntY->Belt.Float.toString}`)
                    ->Js.Array2.joinWith(" ")
            } 
            style=ReactDOM.Style.make(~fill="none", ~stroke=color, ~strokeWidth=lineWidthStr, ())
        />,
        bndFromPoints(ps)
    )
}

let rect = (~at:point, ~width:float, ~height:float, ~color:string):(reElem,boundaries) => {
    let p1 = at
    let p2 = p1->pntTrDir(ey, height)
    let p3 = p2->pntTrDir(ex, width)
    let p4 = p3->pntTrDir(ey, -. height)
    polyline(~ps=[p1,p2,p3,p4,p1], ~color)
}

let text = (
    ~text:string,
    ~key:option<string>=?,
    ~color:string="black",
    ~fontWeight:string="none",
    ()
):svgComp => {
    ex => {
        let at = ex->vecBegin
        (
            <text
                ?key
                x={at->pntX->Belt_Float.toString}
                y={at->pntY->Belt_Float.toString}
                fill=color
                fontSize={fontSize->Belt_Float.toString ++ "px"}
                fontFamily="courier"
                fontWeight
            >
                {text->React.string}
            </text>,
            bndFromVectors([
                ex->vecMult(charLength *. text->Js_string2.length->Belt_Int.toFloat),
                ey->vecMult(charHeight),
            ])

        )
    }
}

// let rndColoredText = (

// ):svgElem => {

// }

module SubModule = {
    @react.component
    let make = (
        ~ctx:mmContext,
        ~args:array<expr>,
        ~label:string,
        ~asrt:expr,
        ~symColors1:option<Belt_HashMapString.t<string>>=?,
        ~symColors2:option<Belt_HashMapString.t<string>>=?,
        ~essOnly:bool=true,
    ) => {
        let mainSvgComp:svgComp = switch ctx->getFrame(label) {
            | _ => {
                ex => {
                    let (textElem, textBnd) = text(~text="Test text", ())(ex)
                    let (rectElem, rectBnd) = rect(~at=ex->vecBegin, ~width=textBnd->bndWidth, ~height=textBnd->bndHeight, ~color="red")
                    (
                        <> rectElem textElem </>,
                        bndMergeAll([textBnd, rectBnd])
                    )
                }
            }
        }

        // let dir = ex->vecRot(15.->deg)->vecMult(0.3)
        // let (l1,b1) = ex->vecTr(dir)->vecLine(~color="red", ())
        // let (l2,b2) = ey->vecTr(dir)->vecLine(~color="green", ())
        // let (l3,b3) = ex->vecRot(45.->deg)->vecTr(dir)->vecLine(~color="blue", ())
        // let (l4,b4) = dir->vecLine(~color="black", ())

        let (mainElem,mainBoundaries) = mainSvgComp(ex)
        <svg
            // viewBox=viewBox(bndMergeAll([b1,b2,b3]))
            viewBox=viewBox(mainBoundaries)
            width={300.->Belt.Float.toString}
            height={300.->Belt.Float.toString}
        >
            // l1 l2 l3
            mainElem
        </svg>
    }
}

let make = React.memoCustomCompareProps( SubModule.make, (_,_) => true )