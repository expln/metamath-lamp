open Expln_2D.Svg2D
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

let scale = 1.

let lineWidth:float = scale *. 0.1
let lineWidthStr = lineWidth->Belt_Float.toString ++ "px"

let subsAvailableColors = ["green", "orange", "#03a9f4", "pink", "brown", "lawngreen", "olive", "blue", "red", "magenta"]

let viewBox = (b:boundaries):string => {
    `${b->bndMinX->Belt.Float.toString}` 
        ++ ` ${b->bndMinY->Belt.Float.toString}` 
        ++ ` ${b->bndWidth->Belt.Float.toString}`
        ++ ` ${b->bndHeight->Belt.Float.toString}`
}

let rndSvg = (~boundaries:boundaries, ~width:option<int>=?, ~height:option<int>=?, ~content:reElem, ()):reElem => {
    let bndWidth = boundaries->bndWidth
    let bndHeight = boundaries->bndHeight
    let (width,height):(int,int) =
        switch width {
            | Some(width) => {
                switch height {
                    | Some(height) => (width,height)
                    | None => (width, width * (bndHeight /. bndWidth)->Belt_Float.toInt)
                }
            }
            | None => {
                switch height {
                    | Some(height) => (height * (bndWidth /. bndHeight)->Belt_Float.toInt, height)
                    | None => Js.Exn.raiseError("At least one of (~width, ~heing) must be specified.")
                }
            }
        }
    <svg
        viewBox=viewBox(boundaries)
        width={width->Belt.Int.toString}
        height={height->Belt.Int.toString}
    >
        content
    </svg>
}

let vecToLine = (v:vector, ~color:string="black", ~key:option<string>=?, ()):(reElem,boundaries) => {
    let b = v->vecBegin
    let e = v->vecEnd
    (
        <line
            ?key
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

let polyline = (~ps:array<point>, ~color:string, ~strokeWidth:float=lineWidth, ~key:option<string>=?, ()):(reElem,boundaries) => {
    (
        <polyline 
            ?key
            points={
                ps->Js_array2.map(p => `${p->pntX->Belt.Float.toString},${p->pntY->Belt.Float.toString}`)
                    ->Js.Array2.joinWith(" ")
            } 
            style=ReactDOM.Style.make(~fill="none", ~stroke=color, ~strokeWidth={strokeWidth->Belt_Float.toString}, ())
        />,
        bndFromPoints(ps)
    )
}

let rect = (~bnd:boundaries, ~color:string, ~strokeWidth:float=lineWidth, ~key:option<string>=?, ()):(reElem,boundaries) => {
    let p1 = bnd->bndLeftBottom
    let p2 = bnd->bndLeftTop
    let p3 = bnd->bndRightTop
    let p4 = bnd->bndRightBottom
    polyline( ~ps=[ p1,p2,p3,p4,p1 ], ~color, ~strokeWidth, ~key=?key, ())
}

let text = (
    ~text:string,
    ~key:option<string>=?,
    ~color:string="black",
    ~bold:bool=false,
    ()
):svgComp => {
    let fontSize:float = scale *. 20.
    let charLength:float = fontSize *. 0.6
    let charHeight:float = charLength *. 1.
    ex => {
        let ey = ex->vecRot(90.->deg)
        let at = ex->vecBegin
        (
            <text
                ?key
                x={at->pntX->Belt_Float.toString}
                y={at->pntY->Belt_Float.toString}
                fill=color
                fontSize={fontSize->Belt_Float.toString ++ "px"}
                fontFamily="courier"
                fontWeight={if (bold) {"bold"} else {"normal"}}
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

let testTextRendering = ():reElem => {
    let (textElem1, textBnd1) = text(~text="Test gy ..WW..", ~bold=true, ())(ex)
    let (rectElem11, rectBnd11) = rect(~bnd=textBnd1, ~color="yellow", ~strokeWidth=lineWidth, ())
    let (rectElem12, rectBnd12) = rect(~bnd=textBnd1, ~color="green", ~strokeWidth=lineWidth *. 10., ())

    let (textElem2, textBnd2) = text(~text="Test gy ..WW..", ~bold=false, ())(ex->vecTr(ey->vecRev->vecMult(textBnd1->bndHeight *. 1.5)))
    let (rectElem21, rectBnd21) = rect(~bnd=textBnd2, ~color="yellow", ~strokeWidth=lineWidth, ())
    let (rectElem22, rectBnd22) = rect(~bnd=textBnd2, ~color="blue", ~strokeWidth=lineWidth *. 10., ())
    rndSvg(
        ~boundaries=
            bndMergeAll([textBnd1, rectBnd11, rectBnd12, textBnd2, rectBnd21, rectBnd22])
            ->bndAddMarginPct(~all=0.01, ()), 
        ~height=700, 
        ~content = <> rectElem12 rectElem11 textElem1 rectElem22 rectElem21 textElem2 </>, 
        ()
    )

}

type props = {
    ctx:mmContext,
    args:array<expr>,
    label:string,
    asrt:expr,
    symColors1:option<Belt_HashMapString.t<string>>,
    symColors2:option<Belt_HashMapString.t<string>>,
    essOnly:bool,
}
let make = React.memoCustomCompareProps( @react.component (props:props) => {
    let rndContent = () => {
        testTextRendering()
    }

    <table style=ReactDOM.Style.make(~tableLayout="fixed", ~width="100%", ())>
        <tbody>
            <tr>
                <td>
                    <div style=ReactDOM.Style.make(~width="100%", ~overflow="auto", ())>
                        {rndContent()}
                    </div>
                </td>
            </tr>
        </tbody>
    </table>
}, (_,_) => true )

// let make = React.memoCustomCompareProps( SubModule.make, (_,_) => true )