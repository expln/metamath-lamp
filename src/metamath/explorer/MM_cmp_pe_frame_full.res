open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_parser
open MM_proof_table
open Common

type state = {
    ctx:mmContext,
    frame:frame,
    hyps:array<expr>,
    asrt:expr,
    proofTable:option<proofTable>,
    symColors:Belt_HashMapString.t<string>,
}

let createInitialState = (~preCtxData:preCtxData, ~ctx:mmContext, ~frame:frame):state => {
    let frmIntToCtxInt = (i:int):int => {
        if (i < 0) {
            i
        } else {
            switch ctx->ctxSymToInt(frame.frameVarToSymb[i]) {
                | None => raise(MmException({msg:`ctx->ctxSymToInt == None in frmIntToCtxInt`}))
                | Some(n) => n
            }
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
                        ~ctx,
                        ~expr=asrt,
                        ~proof,
                        ~isDisjInCtx = (_,_) => true,
                    )
                    Some(createProofTableFromProof(proofRoot))
                }
            }
        }
    }
    let typeColors = preCtxData.settingsV.val->settingsGetTypeColors
    {
        ctx,
        frame,
        hyps:frame.hyps->Js.Array2.map(hyp => frmExprToCtxExpr(hyp.expr)),
        asrt,
        proofTable,
        symColors: createSymbolColors(~ctx, ~typeColors),
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
                    | Ok(ctx) => {
                        setState(_ => {
                            Some(createInitialState(~preCtxData, ~ctx, ~frame=preCtxData.ctxV.val->getFrameExn(label)))
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
                                            ctx=state.ctx
                                            syntaxTypes=preCtxData.syntaxTypes
                                            frms=preCtxData.frms
                                            parenCnt=preCtxData.parenCnt
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