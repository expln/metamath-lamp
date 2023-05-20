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
            let hypsStr = frame.hyps->Js_array2.map(hyp => ctx->frmIntsToSymsExn(frame, hyp.expr))
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

    let rndHyp = (pRec:proofRecord):reElem => {
        switch pRec.proof {
            | Hypothesis(_) => React.null
            | Assertion({args}) => 
                args->Js.Array2.map(i => (i + 1)->Belt_Int.toString)->Js.Array2.joinWith(", ")->React.string
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
                    | Some(proofTable) => proofTable->Js_array2.length
                }
            }
        }
    }

    let numberOfRowsInProofTable = getNumberOfRowsInProofTable(state)
    let classColStep = "step"
    let classColHyp = "hyp"
    let classColRef = "ref"

    React.useEffect1(() => {
        setStepWidth(_ => calcColumnWidth(classColStep, 30, 1000)->Belt.Int.toString ++ "px")
        setHypWidth(_ => calcColumnWidth(classColHyp, 30, 100)->Belt.Int.toString ++ "px")
        setRefWidth(_ => calcColumnWidth(classColRef, 30, 1000)->Belt.Int.toString ++ "px")
        None
    }, [numberOfRowsInProofTable])

    let rndExpr = (~state:state, ~pRec:proofRecord, ~pRecIdx:int):reElem => {
        let elems = [
            <MM_cmp_pe_stmt
                key="MM_cmp_pe_stmt"
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
        ]
        switch state.vData[pRecIdx] {
            | None => ()
            | Some(vDataRec) => {
                elems->Js_array2.push(
                    <MM_cmp_jstf_to_svg
                        key="MM_cmp_jstf_to_svg"
                        hyps=vDataRec.hyps
                        hypLabels={vDataRec.hypLabels->Js_array2.map(i => (i + 1)->Belt_Int.toString)}
                        asrt=vDataRec.asrt
                        frmColors=Some(vDataRec.frmColors)
                        ctxColors1=Some(state.symColors)
                        ctxColors2=None
                        subs=vDataRec.subs
                    />
                )->ignore
            }
        }

        <Col> { elems->React.array } </Col>
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
                <table
                    style=ReactDOM.Style.combine(tdStyle, ReactDOM.Style.make(
                        ~tableLayout="fixed",
                        ~width="100%",
                        ()
                    ))
                >
                    <tbody >
                        <tr>
                            <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=stepWidth, ()))} > {"Step"->React.string} </th>
                            <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=hypWidth, ()))} > {"Hyp"->React.string} </th>
                            <th style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=refWidth, ()))} > {"Ref"->React.string} </th>
                            <th style=tdStyle > {"Expression"->React.string} </th>
                        </tr>
                        {
                            proofTable->Js_array2.mapi((pRec,idx) => {
                                <tr 
                                    key={idx->Belt.Int.toString} 
                                >
                                    <td style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=stepWidth, ()))} className=classColStep>
                                        {(idx+1)->Belt_Int.toString->React.string}
                                    </td>
                                    <td style={tdStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~width=hypWidth, ()))} className=classColHyp >
                                        {rndHyp(pRec)}
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