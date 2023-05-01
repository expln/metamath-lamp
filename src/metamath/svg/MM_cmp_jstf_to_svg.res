open Expln_2D.Svg2D
open Expln_React_common

type svgComp = vector => (reElem,boundaries)

let pxSize = 1.3

let subsAvailableColors = ["green", "orange", "#03a9f4", "pink", "brown", "lawngreen", "olive", "blue", "red", "magenta"]

let viewBox = (b:boundaries):string => {
    `${b->bndMinX->Belt.Float.toString}` 
        ++ ` ${b->bndMinY->Belt.Float.toString}` 
        ++ ` ${b->bndWidth->Belt.Float.toString}`
        ++ ` ${b->bndHeight->Belt.Float.toString}`
}

let rndSvg = (~boundaries:boundaries, ~content:reElem, ()):reElem => {
    let bndWidth = boundaries->bndWidth
    let bndHeight = boundaries->bndHeight
    <svg
        viewBox=viewBox(boundaries)
        width={(bndWidth /. pxSize)->Belt.Float.toString}
        height={(bndHeight /. pxSize)->Belt.Float.toString}
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
    ~ex:vector,
    ~text:string,
    ~key:option<string>=?,
    ~color:string="black",
    ~bold:bool=false,
    ()
):(reElem,boundaries) => {
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

let rndStmt = (
    ~ex:vector,
    ~stmt:array<string>,
    ~symToColor:string=>option<string>,
    ~charWidth:float,
    ~key:string,
):(array<reElem>,boundaries,boundaries) => {
    let elems = []
    let bnds = []
    let curEx = ref(ex)

    let printSymbol = (~sym:string, ~key:string):unit => {
        let color = symToColor(sym)
        let (elem, bnd) = text(
            ~ex=curEx.contents,
            ~text=sym, 
            ~bold=color->Belt_Option.isSome, 
            ~color=color->Belt_Option.getWithDefault("black"),
            ~key, 
            ()
        )
        elems->Js.Array2.push(elem)->ignore
        bnds->Js.Array2.push(bnd)->ignore
        curEx := curEx.contents->vecTr(ex->vecMul(bnd->bndWidth))
    }

    let contentOnlyBnd = []
    for i in 0 to stmt->Js_array2.length-1 {
        let subKey = `${key}-${i->Belt_Int.toString}`
        printSymbol(~sym=stmt[i], ~key=`${subKey}-S`)
        if (i == stmt->Js_array2.length-1) {
            contentOnlyBnd->Js.Array2.push(bndMergeAll(bnds))->ignore
        }
        printSymbol(~sym=" ", ~key=`${subKey}-s`)
    }
    (elems, bndMergeAll(bnds), contentOnlyBnd[0])
}

let testTextRendering = ():reElem => {
    let testText = "|Test gy ..WW.."
    let (textElem1, textBnd1) = text(~ex=ex, ~text=testText, ~bold=true, ())
    let textHeight = textBnd1->bndHeight
    let lineWidth = textHeight *. 0.01
    let (rectElem11, rectBnd11) = rect(~bnd=textBnd1, ~color="yellow", ~lineWidth, ())
    let (rectElem12, rectBnd12) = rect(~bnd=textBnd1, ~color="green", ~lineWidth=lineWidth *. 10., ())

    let (textElem2, textBnd2) = text(~ex=ex->vecTr(ey->vecRev->vecMul(textHeight *. 1.1)), ~text=testText, ~bold=false, ())
    let (rectElem21, rectBnd21) = rect(~bnd=textBnd2, ~color="yellow", ~lineWidth, ())
    let (rectElem22, rectBnd22) = rect(~bnd=textBnd2, ~color="blue", ~lineWidth=lineWidth *. 10., ())

    let testText2 = "|- AbCdEf       WWW eee ... AbCdEf  WWW eee ... AbCdEf  WWW eee ... AbCdEf  WWW eee ... |||"
    let (textElem3, textBnd3) = text(~ex=ex->vecTr(ey->vecRev->vecMul(2. *. textHeight *. 1.1)), ~text=testText2, ~bold=false, ())
    let charWidth = textBnd3->bndWidth /. (testText2->Js.String2.length->Belt.Int.toFloat)
    let textBnd4Arr = []
    let textElem4Arr = []
    let ex4 = ref(ex->vecTr(ey->vecRev->vecMul(3. *. textHeight *. 1.1)))
    let dx = ex->vecMul(charWidth)
    for i in 0 to testText2->Js.String2.length-1 {
        let (textElem4, textBnd4) = text(~ex=ex4.contents, ~text=testText2->Js_string2.charAt(i), ~bold=false, ~key=i->Belt_Int.toString, ())
        textElem4Arr->Js.Array2.push(textElem4)->ignore
        textBnd4Arr->Js.Array2.push(textBnd4)->ignore
        ex4 := ex4.contents->vecTr(dx)
    }

    rndSvg(
        ~boundaries=
            bndMergeAll([textBnd1, rectBnd11, rectBnd12, textBnd2, rectBnd21, rectBnd22, textBnd3]->Js_array2.concat(textBnd4Arr))
            ->bndAddMarginPct(~all=0.01, ()), 
        ~content = <> rectElem12 rectElem11 textElem1 rectElem22 rectElem21 textElem2 textElem3 {textElem4Arr->React.array}</>, 
        ()
    )

}

let rndConnection = (~bnd1:boundaries, ~bnd2:boundaries, ~color:string, ~key:string):(reElem,boundaries) => {
    let bndHeight = bnd1->bndHeight
    let lineWidth = bndHeight *. 0.05
    let (topBnd,bottomBnd) = if (bnd1->bndMinY <= bnd2->bndMinY) {
        (bnd1, bnd2)
    } else {
        (bnd2, bnd1)
    }
    let margin = bndHeight *. 0.4
    let topBnd = topBnd->bndAddMargin(~top=margin, ~bottom=margin, ())
    let bottomBnd = bottomBnd->bndAddMargin(~top=margin, ~bottom=margin, ())
    let (rElem1,rBnd1) = rect(~bnd=topBnd, ~color, ~lineWidth, ~key=`bnd-top-${key}`, ())
    let (rElem2,rBnd2) = rect(~bnd=bottomBnd, ~color, ~lineWidth, ~key=`bnd-bottom-${key}`, ())
    let lineVec = pntVec(
        pntVec(topBnd->bndLeftBottom, topBnd->bndRightBottom)->vecMul(0.5)->vecEnd,
        pntVec(bottomBnd->bndLeftTop, bottomBnd->bndRightTop)->vecMul(0.5)->vecEnd,
    )
    let (lElem,lBnd) = lineVec->vecToLine(~color, ~key=`line-${key}`, ~lineWidth, ())
    (
        [rElem1,rElem2,lElem]->React.array,
        bndMergeAll([rBnd1,rBnd2,lBnd])
    )
}

let rndStmtAndHyp = (
    ~ctxFirst:bool,
    ~frmStmt:array<string>,
    ~hypLabel:option<string>,
    ~subs:Belt_HashMapString.t<array<string>>,
    ~subsColors:Belt_HashMapString.t<string>,
    ~frmColors:option<Belt_HashMapString.t<string>>,
    ~ctxColors1:option<Belt_HashMapString.t<string>>,
    ~ctxColors2:option<Belt_HashMapString.t<string>>,
):svgComp => {
    ex => {
        let frmSymToColor = sym => frmColors->Belt_Option.flatMap(frmColors =>  frmColors->Belt_HashMapString.get(sym))
        let ctxSymToColor = sym => {
            switch ctxColors1->Belt_Option.flatMap(ctxColors1 => ctxColors1->Belt_HashMapString.get(sym)) {
                | Some(color) => Some(color)
                | None => ctxColors2->Belt_Option.flatMap(ctxColors2 => ctxColors2->Belt_HashMapString.get(sym))
            }
        }
        let getCtxSubStmt = (frmSym:string):array<string> => {
            switch subs->Belt_HashMapString.get(frmSym) {
                | Some(ctxSubStmt) => ctxSubStmt
                | None => [frmSym]
            }
        }

        let (_, bndSample) = text(~ex, ~text=".", ())
        let charHeight = bndSample->bndHeight
        let charWidth = bndSample->bndWidth
        let ctxStmtStr = frmStmt->Expln_utils_common.arrFlatMap(getCtxSubStmt)->Js.Array2.joinWith(" ")
        let ctxStmtLen = ctxStmtStr->Js.String2.length->Belt_Int.toFloat *. charWidth
        let frmStmtLen = frmStmt->Js.Array2.joinWith(" ")->Js.String2.length->Belt_Int.toFloat *. charWidth
        let dx = (ctxStmtLen -. frmStmtLen) /. 2.
        let exL = ex
        let exS = ex->vecTr(ex->vecMul(dx))
        let (ctxEx,frmEx) = if (ctxFirst) {
            (
                ref(exL->vecTr(ey->vecMul(charHeight *. 3.3))),
                ref(exS)
            )
        } else {
            (
                ref(exL),
                ref(exS->vecTr(ey->vecMul(charHeight *. 3.3)))
            )
        }
        let frmElems = []
        let ctxElems = []
        let conElems = []
        let bnds = []
        frmStmt->Js_array2.forEachi((frmSym,i) => {
            let (fElems,frmBnd,frmContentOnlyBnd) = rndStmt(
                ~ex=frmEx.contents,
                ~stmt=[frmSym],
                ~symToColor=frmSymToColor,
                ~charWidth,
                ~key="frm-" ++ i->Belt_Int.toString,
            )
            frmElems->Js_array2.pushMany(fElems)->ignore
            bnds->Js_array2.push(frmBnd)->ignore
            frmEx := frmEx.contents->vecTr(ex->vecMul(frmBnd->bndWidth))

            let (cElems,ctxBnd,ctxContentOnlyBnd) = rndStmt(
                ~ex=ctxEx.contents,
                ~stmt=getCtxSubStmt(frmSym),
                ~symToColor=ctxSymToColor,
                ~charWidth,
                ~key="ctx-" ++ i->Belt_Int.toString,
            )
            ctxElems->Js_array2.pushMany(cElems)->ignore
            bnds->Js_array2.push(ctxBnd)->ignore
            ctxEx := ctxEx.contents->vecTr(ex->vecMul(ctxBnd->bndWidth))

            switch subsColors->Belt_HashMapString.get(frmSym) {
                | None => ()
                | Some(color) => {
                    let (conElem,conBnd) = rndConnection(
                        ~bnd1=frmContentOnlyBnd, 
                        ~bnd2=ctxContentOnlyBnd, 
                        ~color, 
                        ~key=i->Belt_Int.toString
                    )
                    conElems->Js_array2.push(conElem)->ignore
                    bnds->Js_array2.push(conBnd)->ignore
                }
            }
        })
        switch hypLabel {
            | None => ()
            | Some(hypLabel) => {
                let stmtsBnd = bndMergeAll(bnds)
                let (labelElem, labelBnd) = text(
                    ~ex = pntVec(stmtsBnd->bndLeftTop, stmtsBnd->bndRightTop)
                        ->vecNorm->vecTr(ey->vecMul(charHeight)),
                    ~color="darkgrey",
                    ~bold=false,
                    ~key="label",
                    ~text=hypLabel,
                    ()
                )
                bnds->Js_array2.push(labelBnd)->ignore
                ctxElems->Js_array2.push(labelElem)->ignore
            }
        }
        (conElems->Js.Array2.concat(frmElems)->Js.Array2.concat(ctxElems)->React.array, bndMergeAll(bnds))
    }
}

@react.component 
let make = (
    ~hyps:array<array<string>>,
    ~hypLabels:array<string>,
    ~asrt:array<string>,
    ~subs:Belt_HashMapString.t<array<string>>,
    ~frmColors:option<Belt_HashMapString.t<string>>,
    ~ctxColors1:option<Belt_HashMapString.t<string>>,
    ~ctxColors2:option<Belt_HashMapString.t<string>>,
) => {
    let (_, bndSample) = text(~ex, ~text=".", ())
    let charHeight = bndSample->bndHeight
    let hypMargin = charHeight *. 3.
    let delimLineWidth = charHeight *. 0.05
    let delimLineMargin = charHeight *. 0.5

    let numOfColors = subsAvailableColors->Js.Array2.length
    let subsColors = subs->Belt_HashMapString.toArray->Js.Array2.mapi(((frmSym,_),i) => {
        (frmSym, subsAvailableColors[mod(i, numOfColors)])
    })->Belt_HashMapString.fromArray

    let rndContent = () => {
        let curEx = ref(ex)
        let hypElems = []
        let hypBnds = []
        hyps->Js.Array2.forEachi((hyp,i) => {
            let (elem, bnd) = rndStmtAndHyp( 
                ~ctxFirst=true, ~frmStmt=hyp, ~subs, ~subsColors, ~frmColors, ~ctxColors1, ~ctxColors2, 
                ~hypLabel=Some(hypLabels[i])
            )(curEx.contents)
            hypElems->Js.Array2.push(elem)->ignore
            hypBnds->Js.Array2.push(bnd)->ignore
            curEx := curEx.contents->vecTr(ex->vecMul(bnd->bndWidth +. hypMargin))
        })
        let asrtComp = rndStmtAndHyp( 
            ~ctxFirst=false, ~frmStmt=asrt, ~subs, ~subsColors, ~frmColors, ~ctxColors1, ~ctxColors2, 
            ~hypLabel=None
        )
        let (_, asrtSampleBnd) = asrtComp(ex)
        let (hypsElem, hypsBnd) = if (hyps->Js.Array2.length == 0) {
            let (sepElem, sepBnd) = pntVec(asrtSampleBnd->bndLeftTop, asrtSampleBnd->bndRightTop)->vecToLine(
                ~color="black", ~lineWidth=delimLineWidth, ~key="delim-line", ()
            )
            (sepElem, sepBnd)
        } else {
            let hypsBnd = bndMergeAll(hypBnds)
            let (sepElem, sepBnd) = pntVec(hypsBnd->bndLeftBottom, hypsBnd->bndRightBottom)
                ->vecNorm
                ->vecMul(Js_math.max_float(hypsBnd->bndWidth, asrtSampleBnd->bndWidth))
                ->vecTr(ey->vecMul(-. delimLineMargin))->vecToLine(
                    ~color="black", ~lineWidth=delimLineWidth, ~key="delim-line", ()
                )
            (
                hypElems->Js.Array2.concat([sepElem])->React.array,
                bndMergeAll([hypsBnd, sepBnd])
            )
        }
        let (asrtElem, asrtBnd) = asrtComp(
            pntVec(hypsBnd->bndLeftBottom, hypsBnd->bndRightBottom)
                ->vecNorm
                ->vecTr(ey->vecMul(-. (delimLineMargin +. asrtSampleBnd->bndHeight)))
        )

        rndSvg(
            ~boundaries=bndMergeAll([hypsBnd, asrtBnd])->bndAddMargin(~all=charHeight *. 0.3, ()), 
            ~content = <> hypsElem asrtElem </>, 
            ()
        )
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