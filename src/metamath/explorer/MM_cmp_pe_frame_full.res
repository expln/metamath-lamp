open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_wrk_ctx_data
open MM_parser
open MM_proof_table
open MM_substitution
open MM_parenCounter
open Common

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

    let asrt = frame.asrt->frmExprToCtxExpr
    let proofTable = switch frame.proof {
        | None => None
        | Some(proof) => {
            switch proof {
                | Uncompressed({labels:["?"]}) => None
                | _ => {
                    let proofRoot = MM_proof_verifier.verifyProof(
                        ~ctx=frmCtx,
                        ~expr=asrt,
                        ~proof,
                        ~isDisjInCtx = (_,_) => true,
                    )
                    Some(createProofTableFromProof(proofRoot))
                }
            }
        }
    }
    let typeColors = settings->settingsGetTypeColors
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

    let rndProof = state => {
        switch state.proofTable {
            | None => "This assertion doesn't have proof."->React.string
            | Some(proofTable) => {
                <table>
                    <tbody>
                        {
                            proofTable->Js_array2.mapi((row,idx) => {
                                <tr key={idx->Belt.Int.toString}>
                                    <td>
                                        {(idx+1)->Belt_Int.toString->React.string}
                                    </td>
                                    <td>
                                        <MM_cmp_pe_stmt
                                            modalRef
                                            ctx=state.frmCtx
                                            syntaxTypes=state.syntaxTypes
                                            frms=state.frms
                                            parenCnt=state.parenCnt
                                            stmt=row.expr
                                            symColors=state.symColors
                                            symRename=None
                                            editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                                        />
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
            <Col>
                {rndLabel(state)}
                {rndDescr(state)}
                {rndProof(state)}
            </Col>
        }
    }

}, propsAreSame)