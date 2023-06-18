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
open MM_react_common

@val external window: {..} = "window"
let location = window["location"]

type vDataRec = {
    hyps:array<array<string>>,
    eHyps:array<int>,
    asrt:array<string>,
    hypLabels:array<int>,
    subs:Belt_HashMapString.t<array<string>>,
    frmColors:Belt_HashMapString.t<string>,
}

type state = {
    settings:settings,
    frmCtx:mmContext,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes: array<int>,
    frame:frame,
    disj: option<array<array<(string,option<string>)>>>,
    hyps:array<hypothesis>,
    asrt:expr,
    proofTable:option<proofTable>,
    symColors:Belt_HashMapString.t<string>,
    vData:array<option<vDataRec>>,
    showTypes:bool,
    essIdxs: Belt_HashSetInt.t,
    stepRenum: Belt_HashMapInt.t<int>,
    expandedIdxs: array<int>,
    syntaxProofTableWasRequested: bool,
    syntaxProofTableError: option<string>,
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

let setProofTable = (st:state, proofTable:proofTable):state => {
    let frmCtx = st.frmCtx
    let settings = st.settings
    let typeColors = settings->settingsGetTypeColors
    let exprsStr = proofTable->Js_array2.map(r => frmCtx->ctxIntsToSymsExn(r.expr))
    let vData = proofTable->Js.Array2.map(pRec => {
        createVDataRec( ~ctx=frmCtx, ~pRec, ~typeColors, ~exprsStr)
    })
    let essIdxs = Belt_HashSetInt.make(~hintSize=1000)
    let stepRenum = Belt_HashMapInt.make(~hintSize=1000)
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
    {
        ...st,
        proofTable: Some(proofTable),
        vData,
        showTypes:false,
        essIdxs,
        stepRenum,
        expandedIdxs: [],
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
    let symColors = createSymbolColors(~ctx=frmCtx, ~typeColors)
    let asrt = frame.asrt->frmExprToCtxExpr

    let disj = if (frame.disj->Belt_MapInt.size > 0) {
        Some(
            MM_cmp_pe_frame_summary_state.createDisjGroups(
                ~disj = frame.disj,
                ~intToSym = i => {
                    let sym = frmCtx->frmIntToSymExn(frame, i)
                    (
                        sym,
                        symColors->Belt_HashMapString.get(sym)
                    )
                },
            )
        )
    } else {
        None
    }

    let st = {
        settings,
        frmCtx,
        frms,
        parenCnt,
        syntaxTypes,
        frame,
        disj,
        hyps:frame.hyps->Js.Array2.map(hyp => {...hyp, expr:frmExprToCtxExpr(hyp.expr)}),
        asrt,
        proofTable: None,
        symColors,
        vData: [],
        showTypes:false,
        essIdxs: Belt_HashSetInt.make(~hintSize=0),
        stepRenum: Belt_HashMapInt.make(~hintSize=0),
        expandedIdxs: [],
        syntaxProofTableWasRequested: false,
        syntaxProofTableError: None,
    }
    switch frame.proof {
        | None => st
        | Some(proof) => {
            switch proof {
                | Uncompressed({labels:["?"]}) => st
                | _ => {
                    let proofRoot = MM_proof_verifier.verifyProof(
                        ~ctx=frmCtx,
                        ~expr=asrt,
                        ~proof,
                        ~isDisjInCtx = (_,_) => true,
                    )
                    st->setProofTable(createProofTableFromProof(proofRoot))
                }
            }
        }
    }
}

let setShowTypes = (st:state,showTypes:bool):state => {
    {...st, showTypes}
}

let toggleShowTypes = (st:state):state => {
    st->setShowTypes(!st.showTypes)
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

let setSyntaxProofTableWasRequested = (st:state,syntaxProofTableWasRequested:bool):state => {
    {...st, syntaxProofTableWasRequested}
}

let setSyntaxProofTableError = (st:state,syntaxProofTableError:option<string>):state => {
    {...st, syntaxProofTableError}
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
        ()
    )
}

let dotPattern = %re("/\./g")

type props = {
    top:int,
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
    openFrameExplorer:string=>unit,
}

let propsAreSame = (a:props, b:props):bool => {
    a.top === b.top
}

let make = React.memoCustomCompareProps(({
    top,
    modalRef,
    preCtxData,
    label,
    openFrameExplorer,
}:props) => {
    let (loadPct, setLoadPct) = React.useState(() => 0.)
    let (loadErr, setLoadErr) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (stepWidth, setStepWidth) = React.useState(() => "100px")
    let (hypWidth, setHypWidth) = React.useState(() => "100px")
    let (refWidth, setRefWidth) = React.useState(() => "100px")

    React.useEffect0(() => {
        setTimeout(
            () => {
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
            },
            10
        )->ignore
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

    let actBuildSyntaxProofTable = ():unit => {
        modifyState(st => {
            let ctx = st.frmCtx
            switch MM_cmp_user_stmt.textToSyntaxProofTable( 
                ~wrkCtx=ctx, 
                ~syms = st.asrt->Js_array2.map(i => {sym:ctx->ctxIntToSymExn(i), color:None}),
                ~syntaxTypes = st.syntaxTypes, 
                ~frms = st.frms, 
                ~parenCnt = st.parenCnt, 
                ~lastSyntaxType=MM_cmp_user_stmt.getLastSyntaxType(),
                ~onLastSyntaxTypeChange=MM_cmp_user_stmt.setLastSyntaxType,
            ) {
                | Error(msg) => st->setSyntaxProofTableError(Some(msg))
                | Ok(proofTable) => {
                    let st = st->setProofTable(proofTable)
                    let st = st->setShowTypes(true)
                    st
                }
            }
        })
    }

    let actSyntaxProofTableWasRequested = () => {
        modifyState(setSyntaxProofTableWasRequested(_, true))
    }

    let syntaxProofTableWasRequested = 
        state->Belt_Option.map(st => st.syntaxProofTableWasRequested)->Belt.Option.getWithDefault(false)
    React.useEffect1(() => {
        if (syntaxProofTableWasRequested) {
            setTimeout(
                () => {
                    modifyState(setSyntaxProofTableWasRequested(_,false))
                    actBuildSyntaxProofTable()
                },
                10
            )->ignore
        }
        None
    }, [syntaxProofTableWasRequested])

    let actToggleShowTypes = () => modifyState(toggleShowTypes)

    let actToggleIdxExpanded = (idx:int) => modifyState(toggleIdxExpanded(_, idx))

    let getStepNum = (state,pRecIdx:int):int => {
        if (state.showTypes) {
            pRecIdx + 1
        } else {
            state.stepRenum->Belt_HashMapInt.get(pRecIdx)->Belt.Option.map(n => n + 1)->Belt.Option.getWithDefault(0)
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
        let proofTableIdCssSelector = proofTableId->Js_string2.replaceByRe(dotPattern, "\\.")
        setStepWidth(_ => calcColumnWidth(`#${proofTableIdCssSelector} .${classColStep}`, 30, 1000)->Belt.Int.toString ++ "px")
        setHypWidth(_ => (calcColumnWidth(`#${proofTableIdCssSelector} .${classColHyp}`, 30, 100)+5)->Belt.Int.toString ++ "px")
        setRefWidth(_ => calcColumnWidth(`#${proofTableIdCssSelector} .${classColRef}`, 30, 1000)->Belt.Int.toString ++ "px")
        None
    }, [numberOfRowsInProofTable])

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
            <span 
                style=ReactDOM.Style.make(~fontFamily="Arial Narrow", ~fontSize="x-small", ~color="grey", ~marginLeft="5px", ())
            >
                { (" " ++ (state.frms->Belt_MapString.size+1)->Belt_Int.toString)->React.string }
            </span>
        </span>
    }

    let rndSummary = state => {
        let style=ReactDOM.Style.make( ~borderCollapse="collapse", () )
        <table style>
            <tbody>
                {
                    state.hyps
                        ->Js_array2.filter(hyp => hyp.typ == E)
                        ->Js_array2.mapi((hyp,i) => {
                            <tr key={i->Belt_Int.toString}>
                                <td style={style->ReactDOMStyle.combine(ReactDOM.Style.make(~paddingLeft="15px", ()))}> 
                                    {(circleChar ++ nbsp ++ hyp.label)->React.string} 
                                </td>
                                <td style={style->ReactDOMStyle.combine(ReactDOM.Style.make(~paddingLeft="10px", ()))}> 
                                    <MM_cmp_pe_stmt
                                        modalRef
                                        ctx=state.frmCtx
                                        syntaxTypes=state.syntaxTypes
                                        frms=state.frms
                                        parenCnt=state.parenCnt
                                        stmt=hyp.expr
                                        symColors=state.symColors
                                        symRename=None
                                        editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                                    /> 
                                </td>
                            </tr>
                        })->React.array
                }
                <tr>
                    <td style=ReactDOM.Style.make(~verticalAlign="top", ~paddingTop="10px", ())> {state.frame.label->React.string} </td>
                    <td style={style->ReactDOMStyle.combine(ReactDOM.Style.make(~paddingLeft="10px", ~verticalAlign="top", ~paddingTop="10px", ()))}> 
                        <MM_cmp_pe_stmt
                            modalRef
                            ctx=state.frmCtx
                            syntaxTypes=state.syntaxTypes
                            frms=state.frms
                            parenCnt=state.parenCnt
                            stmt=state.asrt
                            symColors=state.symColors
                            symRename=None
                            editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                        /> 
                    </td>
                </tr>
            </tbody>
        </table>
    }

    let rndDescr = state => {
        <span>
            {
                state.frame.descr->Belt.Option.getWithDefault(
                    `This ${if (state.frame.isAxiom) {"axiom"} else {"theorem"}} doesn't have any description.`
                )->React.string
            }
        </span>
    }

    let rndDisj = state => {
        switch state.disj {
            | None => <div style=ReactDOM.Style.make(~display="none", ()) />
            | Some(disj) => {
                <div>
                    { (`Distinct variable groups:` ++ MM_cmp_pe_frame_summary_state.disjGrpDelim)->React.string }
                    {MM_cmp_pe_frame_summary_state.rndDisj(disj)}
                </div>
            }
        }
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
                            <a href={"#" ++ proofTableId ++ "-" ++ argIdx->Belt_Int.toString} key={"hyp-" ++ iStr}>
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
                <span style=linkStyle onClick={clickHnd(~act=()=>openFrameExplorer(label),())}>
                    {label->React.string}
                </span>
            }
        }
    }

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
        let labelIdxs = if (state.showTypes) {
            vDataRec.hypLabels
        } else {
            vDataRec.eHyps->Js_array2.map(i => vDataRec.hypLabels[i])
        }
        <MM_cmp_jstf_to_svg
            hyps={if (state.showTypes) {vDataRec.hyps} else {vDataRec.eHyps->Js_array2.map(i => vDataRec.hyps[i])}}
            hypLabels={labelIdxs->Js_array2.map(i => getStepNum(state,i)->Belt_Int.toString)}
            asrt=vDataRec.asrt
            frmColors=Some(vDataRec.frmColors)
            ctxColors1=Some(state.symColors)
            ctxColors2=None
            subs=vDataRec.subs
            onLabelClick = {(i,_) => {
                location["hash"] = "#" ++ proofTableId ++ "-" ++ labelIdxs[i]->Belt_Int.toString
            }}
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
        let rowStyle=ReactDOM.Style.make(())
            ->ReactDOM.Style.unsafeAddProp("scrollMarginTop", top->Belt_Int.toString ++ "px")
        
        switch state.proofTable {
            | None => {
                if (state.frame.isAxiom) {
                    if (state.syntaxProofTableWasRequested) {
                        "Building a syntax breakdown..."->React.string
                    } else {
                        switch state.syntaxProofTableError {
                            | Some(msg) => {
                                <pre style=ReactDOM.Style.make(~color="red", ())>
                                    {React.string(`Could not build a syntax breakdown: ${msg}`)}
                                </pre>
                            }
                            | None => {
                                <Button 
                                    onClick={_=>actSyntaxProofTableWasRequested()}
                                > 
                                    {React.string("Show syntax breakdown")} </Button>
                            }
                        }
                    }
                } else {
                    "This assertion doesn't have a proof."->React.string
                }
            }
            | Some(proofTable) => {
                <Col spacing=0.>
                    {
                        if (state.frame.isAxiom) {
                            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                                {"Syntax breakdown"->React.string}
                            </span>
                        } else {
                            <Button onClick={_=>actToggleShowTypes()}> 
                                {React.string(if(state.showTypes) {"Hide types"} else {"Show types"})} 
                            </Button>
                        }
                    }
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
                                        id={proofTableId ++ "-" ++ idx->Belt.Int.toString} 
                                        style=rowStyle
                                    >
                                        <td 
                                            style={tdStyle->ReactDOM.Style.combine( ReactDOM.Style.make(
                                                ~width=stepWidth, ~verticalAlign="top", 
                                                ()) )
                                            } 
                                            className=classColStep
                                        >
                                            {getStepNum(state,idx)->Belt_Int.toString->React.string}
                                        </td>
                                        <td 
                                            style={tdStyle->ReactDOM.Style.combine( ReactDOM.Style.make(
                                                ~width=hypWidth, ~verticalAlign="top", 
                                                ()) )
                                            } 
                                            className=classColHyp 
                                        >
                                            {rndHyp(state,pRec)}
                                        </td>
                                        <td 
                                            style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                                                ~width=refWidth, ~verticalAlign="top", 
                                                ()))
                                            } 
                                            className=classColRef 
                                        >
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

    let rndFooter = () => {
        Belt_Array.range(1,12)->Js.Array2.map(i => {
            <span key={i->Belt_Int.toString} style=ReactDOM.Style.make(~fontSize="20px", ())>{nbsp->React.string}</span>
        })->React.array
    }

    switch state {
        | None => {
            switch loadErr {
                | Some(msg) => `Error: ${msg}`->React.string
                | None => `Loading ${floatToPctStr(loadPct)}`->React.string
            }
        }
        | Some(state) => {
            <Col spacing=3. style=ReactDOM.Style.make(~padding="5px 10px", ())>
                {rndLabel(state)}
                {rndDescr(state)}
                {rndDisj(state)}
                {rndSummary(state)}
                {rndProof(state)}
                {rndFooter()}
            </Col>
        }
    }

}, propsAreSame)