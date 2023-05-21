open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open Expln_React_common
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_wrk_ctx_data
open MM_parser
open MM_proof_table
open MM_substitution
open MM_parenCounter
open Common
open ColumnWidth

type vDataRec = {
    hyps:array<array<string>>,
    eHyps:array<int>,
    asrt:array<string>,
    hypLabels:array<int>,
    subs:Belt_HashMapString.t<array<string>>,
    frmColors:Belt_HashMapString.t<string>,
}

type state = {
    frmCtx:mmContext,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes: array<int>,
    frame:frame,
    hyps:array<expr>,
    asrt:expr,
    proofTable:option<proofTable>,
    symColors:Belt_HashMapString.t<string>,
    vData:array<option<vDataRec>>,
    showTypes:bool,
    essIdxs: Belt_HashSetInt.t,
    stepRenum: Belt_HashMapInt.t<int>,
    expandedIdxs: array<int>,
}

let createVDataRec = (
    ~ctx:mmContext,
    ~pRec:proofRecord,
    ~typeColors:Belt_HashMapString.t<string>,
    ~exprsStr:array<array<string>>,
):option<vDataRec> => {
    switch pRec.proof {
        | Hypothesis(_) => None
        | Assertion({args, label}) => {
            let frame = ctx->getFrameExn(label)
            let hypsStr = []
            let eHyps = []
            frame.hyps->Js_array2.forEachi((hyp,i) => {
                hypsStr->Js.Array2.push(ctx->frmIntsToSymsExn(frame, hyp.expr))->ignore
                if (hyp.typ == E) {
                    eHyps->Js_array2.push(i)->ignore
                }
            })
            let subs = Belt_HashMapString.make(~hintSize=frame.numOfVars)
            frame.hyps->Js.Array2.forEachi((hyp,i) => {
                if (hyp.typ == F) {
                    subs->Belt_HashMapString.set(
                        frame.frameVarToSymb[hyp.expr[1]],
                        exprsStr[args[i]]->Js_array2.sliceFrom(1)
                    )
                }
            })
            let frmColors = Belt_HashMapString.make(~hintSize=frame.numOfVars)
            for i in 0 to frame.numOfVars-1 {
                switch typeColors->Belt_HashMapString.get(ctx->ctxIntToSymExn(frame.varTypes[i])) {
                    | None => ()
                    | Some(color) => frmColors->Belt_HashMapString.set( frame.frameVarToSymb[i], color )
                }
            }
            Some({
                hyps:hypsStr,
                eHyps,
                asrt:ctx->frmIntsToSymsExn(frame, frame.asrt),
                hypLabels:args,
                subs,
                frmColors,
            })
        }
    }
}

let createInitialState = (~settings:settings, ~preCtx:mmContext, ~frmCtx:mmContext, ~frame:frame):state => {
    frmCtx->moveConstsToBegin(settings.parens)
    let frms = prepareFrmSubsData( ~ctx=frmCtx, () )
    let parenCnt = parenCntMake(prepareParenInts(frmCtx, settings.parens), ~checkParensOptimized=true, ())
    let syntaxTypes = findSyntaxTypes(frmCtx, frms)

    let frmIntToCtxInt = (i:int):int => {
        switch frmCtx->ctxSymToInt(
            if (i < 0) { preCtx->ctxIntToSymExn(i) } else { frame.frameVarToSymb[i] }
        ) {
            | None => raise(MmException({msg:`ctx->ctxSymToInt == None in frmIntToCtxInt`}))
            | Some(n) => n
        }
    }

    let frmExprToCtxExpr = (expr:expr):expr => {
        expr->Js_array2.map(frmIntToCtxInt)
    }

    let typeColors = settings->settingsGetTypeColors
    let asrt = frame.asrt->frmExprToCtxExpr
    let essIdxs = Belt_HashSetInt.make(~hintSize=1000)
    let stepRenum = Belt_HashMapInt.make(~hintSize=1000)
    let (proofTable,vData) = switch frame.proof {
        | None => (None,[])
        | Some(proof) => {
            switch proof {
                | Uncompressed({labels:["?"]}) => (None,[])
                | _ => {
                    let proofRoot = MM_proof_verifier.verifyProof(
                        ~ctx=frmCtx,
                        ~expr=asrt,
                        ~proof,
                        ~isDisjInCtx = (_,_) => true,
                    )
                    let proofTable = createProofTableFromProof(proofRoot)
                    let exprsStr = proofTable->Js_array2.map(r => frmCtx->ctxIntsToSymsExn(r.expr))
                    let vData = proofTable->Js.Array2.map(pRec => {
                        createVDataRec( ~ctx=frmCtx, ~pRec, ~typeColors, ~exprsStr)
                    })
                    proofTable->Js.Array2.forEach(pRec => {
                        switch pRec.proof {
                            | Hypothesis(_) => ()
                            | Assertion({args, label}) => {
                                let frame = frmCtx->getFrameExn(label)
                                frame.hyps->Js.Array2.forEachi((hyp,i) => {
                                    if (hyp.typ == E) {
                                        essIdxs->Belt_HashSetInt.add(args[i])
                                    }
                                })
                            }
                        }
                    })
                    essIdxs->Belt_HashSetInt.add(proofTable->Js.Array2.length-1)
                    proofTable->Js.Array2.forEachi((_,i) => {
                        if (essIdxs->Belt_HashSetInt.has(i)) {
                            stepRenum->Belt_HashMapInt.set(i,stepRenum->Belt_HashMapInt.size)
                        }
                    })
                    (Some(proofTable), vData)
                }
            }
        }
    }
    {
        frmCtx,
        frms,
        parenCnt,
        syntaxTypes,
        frame,
        hyps:frame.hyps->Js.Array2.map(hyp => frmExprToCtxExpr(hyp.expr)),
        asrt,
        proofTable,
        symColors: createSymbolColors(~ctx=frmCtx, ~typeColors),
        vData,
        showTypes:false,
        essIdxs,
        stepRenum,
        expandedIdxs: [],
    }
}

let toggleShowTypes = (st:state):state => {
    {...st, showTypes:!st.showTypes}
}

let toggleIdxExpanded = (st:state, idx:int):state => {
    if (st.expandedIdxs->Js.Array2.includes(idx)) {
        {
            ...st, 
            expandedIdxs: st.expandedIdxs->Js.Array2.filter(i => i != idx)
        }
    } else {
        {
            ...st, 
            expandedIdxs: st.expandedIdxs->Js.Array2.concat([idx])
        }
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
    true
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    label,
}:props) => {
    let (loadPct, setLoadPct) = React.useState(() => 0.)
    let (loadErr, setLoadErr) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (stepWidth, setStepWidth) = React.useState(() => "100px")
    let (hypWidth, setHypWidth) = React.useState(() => "100px")
    let (refWidth, setRefWidth) = React.useState(() => "100px")

    React.useEffect0(() => {
        loadFrameContext(
            ~srcs=preCtxData.srcs,
            ~label,
            ~onProgress = pct => setLoadPct(_ => pct),
            ~onDone = res => {
                switch res {
                    | Error(msg) => setLoadErr(_ => Some(msg))
                    | Ok(frmCtx) => {
                        setState(_ => {
                            Some(createInitialState(
                                ~settings=preCtxData.settingsV.val, 
                                ~preCtx=preCtxData.ctxV.val,
                                ~frmCtx, 
                                ~frame=preCtxData.ctxV.val->getFrameExn(label)
                            ))
                        })
                    }
                }
            },
        )
        None
    })

    let modifyState = (update:state=>state):unit => {
        setState(st => {
            switch st {
                | None => None
                | Some(st) => Some(update(st))
            }
        })
    }

    let actToggleShowTypes = () => modifyState(toggleShowTypes)

    let actToggleIdxExpanded = (idx:int) => modifyState(toggleIdxExpanded(_, idx))

    let getStepNum = (state,pRecIdx:int):int => {
        if (state.showTypes) {
            pRecIdx + 1
        } else {
            state.stepRenum->Belt_HashMapInt.get(pRecIdx)->Belt.Option.map(n => n + 1)->Belt.Option.getWithDefault(0)
        }
    }

    let rndLabel = state => {
        let asrtType = if (state.frame.isAxiom) {
            <span style=ReactDOM.Style.make(~color="red", ())>
                {"Axiom"->React.string}
            </span>
        } else {
            <span style=ReactDOM.Style.make(~color="green", ())>
                {"Theorem"->React.string}
            </span>
        }
        <span>
            asrtType
            <span 
                style=ReactDOM.Style.make(~fontWeight="bold", ~cursor="pointer", ())
            >
                { (" " ++ state.frame.label)->React.string }
            </span>
        </span>
    }

    let rndDescr = state => {
        <span>
            {
                state.frame.descr->Belt.Option.getWithDefault("This assertion doesn't have any description.")->React.string
            }
        </span>
    }

    let rndHyp = (state,pRec:proofRecord):reElem => {
        switch pRec.proof {
            | Hypothesis(_) => React.null
            | Assertion({args}) => {
                let elems = []
                for i in 0 to args->Js.Array2.length-1 {
                    let argIdx = args[i]
                    if (state.showTypes || state.essIdxs->Belt_HashSetInt.has(argIdx)) {
                        let iStr = i->Belt_Int.toString
                        if (elems->Js.Array2.length > 0) {
                            elems->Js.Array2.push(
                                <span key={"delim-" ++ iStr}>
                                    {React.string(", ")}
                                </span>
                            )->ignore
                        }
                        elems->Js.Array2.push(
                            <a href={"#" ++ argIdx->Belt_Int.toString} key={"hyp-" ++ iStr}>
                                {React.string(getStepNum(state,argIdx)->Belt_Int.toString)}
                            </a>
                        )->ignore
                    }
                }
                elems->React.array
            }
        }
    }

    let linkStyle = ReactDOM.Style.make(
        ~cursor="pointer", 
        ~textDecoration="underline",
        ~color="rgb(0,0,238)",
        ~textDecorationColor="rgb(0,0,238)",
        ()
    )
    let rndRef = (pRec:proofRecord):reElem => {
        switch pRec.proof {
            | Hypothesis({label}) => label->React.string
            | Assertion({label}) => {
                <span style=linkStyle>
                    {label->React.string}
                </span>
            }
        }
    }

    let getNumberOfRowsInProofTable = (state:option<state>):int => {
        switch state {
            | None => 0
            | Some(state) => {
                switch state.proofTable {
                    | None => 0
                    | Some(proofTable) => {
                        if (state.showTypes) {
                            proofTable->Js_array2.length
                        } else {
                            proofTable->Js_array2.reducei(
                                (cnt,_,idx) => {
                                    cnt + if (state.essIdxs->Belt_HashSetInt.has(idx)) {1} else {0}
                                },
                                0
                            )
                        }
                    }
                }
            }
        }
    }

    let numberOfRowsInProofTable = getNumberOfRowsInProofTable(state)
    let proofTableId = `tbl-${label}`
    let classColStep = "step"
    let classColHyp = "hyp"
    let classColRef = "ref"

    React.useEffect1(() => {
        setStepWidth(_ => calcColumnWidth(`#${proofTableId} .${classColStep}`, 30, 1000)->Belt.Int.toString ++ "px")
        setHypWidth(_ => (calcColumnWidth(`#${proofTableId} .${classColHyp}`, 30, 100)+5)->Belt.Int.toString ++ "px")
        setRefWidth(_ => calcColumnWidth(`#${proofTableId} .${classColRef}`, 30, 1000)->Belt.Int.toString ++ "px")
        None
    }, [numberOfRowsInProofTable])

    let expBtnBaseStyle = ReactDOM.Style.make(
        ~color="lightgrey", 
        ~border="1px solid",
        ~marginRight="3px",
        ~padding="1px 3px 0px 2px",
        ~fontFamily="courier",
        ~fontSize="10px",
        ~cursor="pointer",
        ()
    )
    let rndExpBtn = (~state:state, ~pRecIdx:int):reElem => {
        let style = switch state.vData[pRecIdx] {
            | None => expBtnBaseStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~opacity="0", ()))
            | Some(_) => expBtnBaseStyle
        }
        <a style onClick={_=>actToggleIdxExpanded(pRecIdx)}>
            {React.string(if (state.expandedIdxs->Js_array2.includes(pRecIdx)) {"-"} else {"+"})}
        </a>
    }

    let rndExprText = (~state:state, ~pRec:proofRecord):reElem => {
        <MM_cmp_pe_stmt
            modalRef
            ctx=state.frmCtx
            syntaxTypes=state.syntaxTypes
            frms=state.frms
            parenCnt=state.parenCnt
            stmt=pRec.expr
            symColors=state.symColors
            symRename=None
            editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
        />
    }

    let rndExprSvg = (~state:state, ~vDataRec:vDataRec):reElem => {
        <MM_cmp_jstf_to_svg
            hyps={if (state.showTypes) {vDataRec.hyps} else {vDataRec.eHyps->Js_array2.map(i => vDataRec.hyps[i])}}
            hypLabels={
                let labelIdxs = if (state.showTypes) {
                    vDataRec.hypLabels
                } else {
                    vDataRec.eHyps->Js_array2.map(i => vDataRec.hypLabels[i])
                }
                labelIdxs->Js_array2.map(i => getStepNum(state,i)->Belt_Int.toString)
            }
            asrt=vDataRec.asrt
            frmColors=Some(vDataRec.frmColors)
            ctxColors1=Some(state.symColors)
            ctxColors2=None
            subs=vDataRec.subs
        />
    }

    let rndExpr = (~state:state, ~pRec:proofRecord, ~pRecIdx:int):reElem => {
        <table>
            <tbody>
                <tr>
                    <td style=ReactDOM.Style.make(~verticalAlign="top", ())>
                        {rndExpBtn(~state, ~pRecIdx)}
                    </td>
                    <td style=ReactDOM.Style.make(~verticalAlign="top", ())>
                        {
                            switch state.vData[pRecIdx] {
                                | None => rndExprText(~state, ~pRec)
                                | Some(vDataRec) => {
                                    if (state.expandedIdxs->Js_array2.includes(pRecIdx)) {
                                        rndExprSvg(~state, ~vDataRec)
                                    } else {
                                        rndExprText(~state, ~pRec)
                                    }
                                }
                            }
                        }
                    </td>
                </tr>
            </tbody>
        </table>
    }

    let rndProof = state => {
        let tdStyle=ReactDOM.Style.make(
            ~borderCollapse="collapse", 
            ~border="1px solid black", 
            ~padding="5px",
            ~verticalAlign="verticalAlign",
            ()
        )
        switch state.proofTable {
            | None => "This assertion doesn't have proof."->React.string
            | Some(proofTable) => {
                <Col spacing=0.>
                    <Button onClick={_=>actToggleShowTypes()}> {React.string(if(state.showTypes) {"Hide types"} else {"Show types"})} </Button>
                    <table
                        id=proofTableId
                        style=ReactDOM.Style.combine(tdStyle, ReactDOM.Style.make(
                            ~tableLayout="fixed",
                            ~width="100%",
                            ()
                        ))
                    >
                        <thead>
                            <tr>
                                <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=stepWidth, ()))} > {"Step"->React.string} </th>
                                <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=hypWidth, ()))} > {"Hyp"->React.string} </th>
                                <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=refWidth, ()))} > {"Ref"->React.string} </th>
                                <th style=tdStyle > {"Expression"->React.string} </th>
                            </tr>
                        </thead>
                        <tbody >
                            {
                                proofTable->Js_array2.mapi((pRec,idx) => (pRec,idx))->Js_array2.filter(((_,idx)) => {
                                    if (state.showTypes) {true} else {state.essIdxs->Belt_HashSetInt.has(idx)}
                                })
                                    ->Js_array2.map(((pRec,idx)) => {
                                    <tr 
                                        key={idx->Belt.Int.toString} 
                                        id={idx->Belt.Int.toString} 
                                    >
                                        <td style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=stepWidth, ()))} className=classColStep>
                                            {getStepNum(state,idx)->Belt_Int.toString->React.string}
                                        </td>
                                        <td style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=hypWidth, ()))} className=classColHyp >
                                            {rndHyp(state,pRec)}
                                        </td>
                                        <td style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=refWidth, ()))} className=classColRef >
                                            {rndRef(pRec)}
                                        </td>
                                        <td style=tdStyle >
                                            {rndExpr(~state, ~pRec, ~pRecIdx=idx)}
                                        </td>
                                    </tr>
                                })->React.array
                            }
                        </tbody>
                    </table>
                </Col>
            }
        }
    }

    switch state {
        | None => {
            switch loadErr {
                | Some(msg) => `Error: ${msg}`->React.string
                | None => `Loading ${floatToPctStr(loadPct)}`->React.string
            }
        }
        | Some(state) => {
            <Col style=ReactDOM.Style.make(~padding="0px 10px", ())>
                {rndLabel(state)}
                {rndDescr(state)}
                {rndProof(state)}
            </Col>
        }
    }

}, propsAreSame)