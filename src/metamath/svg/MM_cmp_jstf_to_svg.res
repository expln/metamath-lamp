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

let vecToLine = (v:vector, ~color:string="black", ~key:option<string>=?, ~lineWidth:float, ()):(reElem,boundaries) => {
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
            strokeWidth={lineWidth->Belt.Float.toString}
        />,
        bndFromPoints([v->vecBegin, v->vecEnd])
    )
}

let polyline = (~ps:array<point>, ~color:string, ~lineWidth:float, ~key:option<string>=?, ()):(reElem,boundaries) => {
    (
        <polyline 
            ?key
            points={
                ps->Js_array2.map(p => `${p->pntX->Belt.Float.toString},${p->pntY->Belt.Float.toString}`)
                    ->Js.Array2.joinWith(" ")
            } 
            style=ReactDOM.Style.make(~fill="none", ~stroke=color, ~strokeWidth={lineWidth->Belt_Float.toString}, ())
        />,
        bndFromPoints(ps)
    )
}

let rect = (~bnd:boundaries, ~color:string, ~lineWidth:float, ~key:option<string>=?, ()):(reElem,boundaries) => {
    let p1 = bnd->bndLeftBottom
    let p2 = bnd->bndLeftTop
    let p3 = bnd->bndRightTop
    let p4 = bnd->bndRightBottom
    polyline( ~ps=[ p1,p2,p3,p4,p1 ], ~color, ~lineWidth, ~key=?key, ())
}

let text = (
    ~text:string,
    ~key:option<string>=?,
    ~color:string="black",
    ~bold:bool=false,
    ()
):svgComp => {
    ex => {
        let ey = ex->vecRot(90.->deg)

        let fontSize:float = 20. *. ex->vecLen

        let yShift = fontSize *. 0.21
        let at = ex->vecBegin->pntTr(ey->vecMul(yShift))
        let charHeight:float = fontSize *. 0.84
        let charWidth:float = fontSize *. 0.6
        (
            <text
                ?key
                x={at->pntX->Belt_Float.toString}
                y={at->pntY->Belt_Float.toString}
                fill=color
                fontSize={fontSize->Belt_Float.toString}
                fontFamily="courier"
                fontWeight={if (bold) {"bold"} else {"normal"}}
                style=ReactDOM.Style.make(~whiteSpace="pre", ())
            >
                {text->React.string}
            </text>,
            bndFromVectors([
                ex->vecMul(charWidth *. text->Js_string2.length->Belt_Int.toFloat),
                ey->vecMul(charHeight),
            ])
        )
    }
}

let testTextRendering = ():reElem => {
    let testText = "|Test gy ..WW.."
    let (textElem1, textBnd1) = text(~text=testText, ~bold=true, ())(ex)
    let textHeight = textBnd1->bndHeight
    let lineWidth = textHeight *. 0.01
    let (rectElem11, rectBnd11) = rect(~bnd=textBnd1, ~color="yellow", ~lineWidth, ())
    let (rectElem12, rectBnd12) = rect(~bnd=textBnd1, ~color="green", ~lineWidth=lineWidth *. 10., ())

    let (textElem2, textBnd2) = text(~text=testText, ~bold=false, ())(ex->vecTr(ey->vecRev->vecMul(textHeight *. 1.1)))
    let (rectElem21, rectBnd21) = rect(~bnd=textBnd2, ~color="yellow", ~lineWidth, ())
    let (rectElem22, rectBnd22) = rect(~bnd=textBnd2, ~color="blue", ~lineWidth=lineWidth *. 10., ())

    let testText2 = "|- AbCdEf       WWW eee ... AbCdEf  WWW eee ... AbCdEf  WWW eee ... AbCdEf  WWW eee ... |||"
    let (textElem3, textBnd3) = text(~text=testText2, ~bold=false, ())(ex->vecTr(ey->vecRev->vecMul(2. *. textHeight *. 1.1)))
    let charWidth = textBnd3->bndWidth /. (testText2->Js.String2.length->Belt.Int.toFloat)
    let textBnd4Arr = []
    let textElem4Arr = []
    let ex4 = ref(ex->vecTr(ey->vecRev->vecMul(3. *. textHeight *. 1.1)))
    let dx = ex->vecMul(charWidth)
    for i in 0 to testText2->Js.String2.length-1 {
        let (textElem4, textBnd4) = text(~text=testText2->Js_string2.charAt(i), ~bold=false, ~key=i->Belt_Int.toString, ())(ex4.contents)
        textElem4Arr->Js.Array2.push(textElem4)->ignore
        textBnd4Arr->Js.Array2.push(textBnd4)->ignore
        ex4 := ex4.contents->vecTr(dx)
    }

    rndSvg(
        ~boundaries=
            bndMergeAll([textBnd1, rectBnd11, rectBnd12, textBnd2, rectBnd21, rectBnd22, textBnd3]->Js_array2.concat(textBnd4Arr))
            ->bndAddMarginPct(~all=0.01, ()), 
        ~height=700,
        ~content = <> rectElem12 rectElem11 textElem1 rectElem22 rectElem21 textElem2 textElem3 {textElem4Arr->React.array}</>, 
        ()
    )

}

@react.component 
let make = (
    ~hyps:array<array<string>>,
    ~asrt:array<string>,
    ~symColors1:option<Belt_HashMapString.t<string>>,
    ~symColors2:option<Belt_HashMapString.t<string>>,
    ~subs:Belt_HashMapString.t<array<string>>,
) => {
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
}