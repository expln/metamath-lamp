open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open Expln_React_common
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_parser
open MM_proof_table
open MM_substitution
open MM_parenCounter
open Common
open ColumnWidth
open MM_react_common
open MM_wrk_editor_json
open MM_statements_dto
open Expln_utils_promise

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
    frmMmScopes:array<mmScope>,
    frmCtx:mmContext,
    frms: frms,
    parenCnt: parenCnt,
    syntaxTypes: array<int>,
    typeOrderInDisj:Belt_HashMapInt.t<int>,
    frame:frame,
    disjStr: option<array<array<(string,option<string>)>>>,
    dummyVarDisj: option<disjMutable>,
    dummyVarDisjStr: option<array<array<(string,option<string>)>>>,
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

type frameProofData = state

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

let setProofTable = (st:state, ~proofTable:proofTable, ~dummyVarDisj:disjMutable):state => {
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
    let dummyVarDisjStr = if (!(dummyVarDisj->disjIsEmpty)) {
        Some(
            MM_cmp_pe_frame_summary_state.createDisjGroups(
                ~disj = dummyVarDisj->disjMutToDisjImm,
                ~intToSym = i => {
                    let sym = frmCtx->ctxIntToSymExn(i)
                    (
                        sym,
                        st.symColors->Belt_HashMapString.get(sym)
                    )
                },
                ~intToTyp=frmCtx->getTypeOfVar,
                ~typeOrder=st.typeOrderInDisj,
            )
        )
    } else {
        None
    }
    {
        ...st,
        dummyVarDisj: if (dummyVarDisj->disjIsEmpty) {None} else {Some(dummyVarDisj)},
        dummyVarDisjStr,
        proofTable: Some(proofTable),
        vData,
        showTypes:false,
        essIdxs,
        stepRenum,
        expandedIdxs: [],
    }
}

let createInitialState = (
    ~settings:settings, 
    ~frmMmScopes:array<mmScope>,
    ~preCtx:mmContext, 
    ~frmCtx:mmContext, 
    ~frame:frame
):state => {
    let frmCtx = frmCtx->ctxOptimizeForProver(~parens=settings.parens, ~removeAsrtDescr=false, ~removeProofs=false, ())
    let frms = prepareFrmSubsData( ~ctx=frmCtx, () )
    let parenCnt = MM_provers.makeParenCnt(~ctx=frmCtx, ~parens=settings.parens)
    let (_, syntaxTypes) = findTypes(frmCtx)

    let frmIntToCtxInt = (i:int):int => {
        switch frmCtx->ctxSymToInt(
            if (i < 0) { preCtx->ctxIntToSymExn(i) } else { frame.frameVarToSymb[i] }
        ) {
            | None => raise(MmException({msg:`preCtx->ctxSymToInt == None in frmIntToCtxInt`}))
            | Some(n) => n
        }
    }

    let ctxVarToFrmVar = (i:int):option<int> => {
        switch frame.frameVarToSymb->Js.Array2.indexOf(frmCtx->ctxIntToSymExn(i)) {
            | -1 => None
            | n => Some(n)
        }
    }

    let frmExprToCtxExpr = (expr:expr):expr => {
        expr->Js_array2.map(frmIntToCtxInt)
    }

    let typeColors = settings->settingsGetTypeColors
    let symColors = createSymbolColors(~ctx=frmCtx, ~typeColors)
    let asrt = frame.asrt->frmExprToCtxExpr

    let typeOrderInDisj = createTypeOrderFromStr(
        ~sortDisjByType=settings.sortDisjByType, 
        ~typeNameToInt=frmCtx->ctxSymToInt
    )

    let disjStr = if (frame.disj->Belt_MapInt.size > 0) {
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
                ~intToTyp = i => frmCtx->getTypeOfVar(frmIntToCtxInt(i)),
                ~typeOrder = typeOrderInDisj,
            )
        )
    } else {
        None
    }

    let st = {
        settings,
        frmMmScopes,
        frmCtx,
        frms,
        parenCnt,
        syntaxTypes,
        typeOrderInDisj,
        frame,
        disjStr,
        dummyVarDisj:None,
        dummyVarDisjStr:None,
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
                    let dummyVarDisj = disjMake()
                    let proofRoot = MM_proof_verifier.verifyProof(
                        ~ctx=frmCtx,
                        ~expr=asrt,
                        ~proof,
                        ~isDisjInCtx = (n,m) => {
                            let isFrmDisj = switch ctxVarToFrmVar(n) {
                                | None => false
                                | Some(n) => {
                                    switch ctxVarToFrmVar(m) {
                                        | None => false
                                        | Some(m) => frame.disj->disjImmContains(n,m)
                                    }
                                }
                            }
                            if (!isFrmDisj) {
                                dummyVarDisj->disjAddPair(n,m)
                            }
                            true
                        },
                    )
                    st->setProofTable(~proofTable=createProofTableFromProof(~proofNode=proofRoot, ()), ~dummyVarDisj)
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
    ~descrRegexToDisc:string,
    ~labelRegexToDisc:string,
    ~descrRegexToDepr:string,
    ~labelRegexToDepr:string,
    ~onProgress:float=>unit,
    ~onDone: result<(array<mmScope>, mmContext),string>=>unit,
):unit => {
    let scopes = createMmScopesForFrame( ~srcs, ~label, )
    beginLoadingMmContext(
        ~scopes,
        ~descrRegexToDisc,
        ~labelRegexToDisc,
        ~descrRegexToDepr,
        ~labelRegexToDepr,
        ~onProgress,
        ~onDone = res => {
            switch res {
                | Error(msg) => onDone(Error(msg))
                | Ok(ctx) => onDone(Ok((scopes,ctx)))
            }
        },
    )
}

let dotPattern = %re("/\./g")

type props = {
    top:int,
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
    openFrameExplorer:string=>unit,
    openExplorer:(~initPatternFilterStr:string)=>unit,
    loadEditorState: React.ref<Js.Nullable.t<editorStateLocStor => unit>>,
    focusEditorTab: unit=>unit,
    toggleCtxSelector:React.ref<Js.Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
}

let propsAreSame = (a:props, b:props):bool => {
    a.top === b.top 
    && a.ctxSelectorIsExpanded === b.ctxSelectorIsExpanded
}

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool, 
    ~notifyEditInTempMode:option<(unit=>'a)=>'a>=?,
    ~ref:option<ReactDOM.domRef>=?,
    ~title:option<string>=?, 
    ~smallBtns:bool=false,
    ()
) => {
    <span ?ref ?title>
        <IconButton 
            disabled={!active} 
            onClick={_ => {
                switch notifyEditInTempMode {
                    | None => onClick()
                    | Some(notifyEditInTempMode) => notifyEditInTempMode(() => onClick())
                }
            }} 
            color="primary"
            style=?{
                if (smallBtns) {Some(ReactDOM.Style.make(~padding="2px", ()))} else {None}
            }
        > 
            icon 
        </IconButton>
    </span>
}

let convertMmScopesToMmCtxSrcDtos = (
    ~origMmCtxSrcDtos:array<mmCtxSrcDto>,
    ~mmScopes:array<mmScope>,
):option<array<mmCtxSrcDto>> => {
    let canConvert = mmScopes->Js_array2.length <= origMmCtxSrcDtos->Js_array2.length 
                        && mmScopes->Js_array2.everyi((mmScope,i) => {
                            origMmCtxSrcDtos[i].ast
                                ->Belt_Option.map(origAst => origAst == mmScope.ast)
                                ->Belt.Option.getWithDefault(false)
                        })
    if (!canConvert) {
        None
    } else {
        Some(
            mmScopes->Js_array2.mapi((mmScope,i) => {
                let origMmCtxSrcDto = origMmCtxSrcDtos[i]
                let (readInstr,label) = switch mmScope.stopBefore {
                    | Some(label) => (readInstrToStr(StopBefore), label)
                    | None => {
                        switch mmScope.stopAfter {
                            | Some(label) => (readInstrToStr(StopAfter), label)
                            | None => (readInstrToStr(ReadAll), "")
                        }
                    }
                }
                {
                    typ: origMmCtxSrcDto.typ,
                    fileName: origMmCtxSrcDto.fileName,
                    url: origMmCtxSrcDto.url,
                    readInstr,
                    label,
                    resetNestingLevel:true,
                    ast: origMmCtxSrcDto.ast,
                    allLabels: origMmCtxSrcDto.allLabels,
                }
            })
        )
    }
}

let makeFrameProofData = (
    ~preCtxData:preCtxData,
    ~label:string,
    ~onProgress: float => unit,
):promise<result<frameProofData,string>> => {
    promise(resolve => {
        loadFrameContext(
            ~srcs=preCtxData.srcs,
            ~label,
            ~descrRegexToDisc=preCtxData.settingsV.val.descrRegexToDisc,
            ~labelRegexToDisc=preCtxData.settingsV.val.labelRegexToDisc,
            ~descrRegexToDepr=preCtxData.settingsV.val.descrRegexToDepr,
            ~labelRegexToDepr=preCtxData.settingsV.val.labelRegexToDepr,
            ~onProgress,
            ~onDone = res => {
                switch res {
                    | Error(msg) => resolve(Error(msg))
                    | Ok((frmMmScopes,frmCtx)) => {
                        resolve(
                            Ok(
                                createInitialState(
                                    ~settings=preCtxData.settingsV.val, 
                                    ~frmMmScopes,
                                    ~preCtx=preCtxData.ctxV.val,
                                    ~frmCtx,
                                    ~frame=preCtxData.ctxV.val->getFrameExn(label)
                                )
                            )
                        )
                    }
                }
            },
        )
    })
}

let make = React.memoCustomCompareProps(({
    top,
    modalRef,
    preCtxData,
    label,
    openFrameExplorer,
    openExplorer,
    loadEditorState,
    focusEditorTab,
    toggleCtxSelector,
    ctxSelectorIsExpanded,
}:props) => {
    let (loadPct, setLoadPct) = React.useState(() => 0.)
    let (loadErr, setLoadErr) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (stepWidth, setStepWidth) = React.useState(() => "100px")
    let (hypWidth, setHypWidth) = React.useState(() => "100px")
    let (refWidth, setRefWidth) = React.useState(() => "100px")

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Js.Nullable.null)

    React.useEffect0(() => {
        setTimeout(
            () => {
                loadFrameContext(
                    ~srcs=preCtxData.srcs,
                    ~label,
                    ~descrRegexToDisc=preCtxData.settingsV.val.descrRegexToDisc,
                    ~labelRegexToDisc=preCtxData.settingsV.val.labelRegexToDisc,
                    ~descrRegexToDepr=preCtxData.settingsV.val.descrRegexToDepr,
                    ~labelRegexToDepr=preCtxData.settingsV.val.labelRegexToDepr,
                    ~onProgress = pct => setLoadPct(_ => pct),
                    ~onDone = res => {
                        switch res {
                            | Error(msg) => setLoadErr(_ => Some(msg))
                            | Ok((frmMmScopes,frmCtx)) => {
                                setState(_ => {
                                    Some(createInitialState(
                                        ~settings=preCtxData.settingsV.val, 
                                        ~frmMmScopes,
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

    let actOpenMainMenu = () => {
        setMainMenuIsOpened(_ => true)
    }

    let actCloseMainMenu = () => {
        setMainMenuIsOpened(_ => false)
    }

    let actBuildSyntaxProofTable = ():unit => {
        modifyState(st => {
            let ctx = st.frmCtx
            switch textToSyntaxProofTable( 
                ~wrkCtx=ctx, 
                ~syms = [ctx->ctxIntsToSymsExn(st.asrt->Js_array2.sliceFrom(_, 1))],
                ~syntaxTypes = st.syntaxTypes, 
                ~frms = st.frms,
                ~frameRestrict=preCtxData.settingsV.val.allowedFrms.inSyntax, 
                ~parenCnt = st.parenCnt, 
                ~lastSyntaxType=MM_cmp_user_stmt.getLastSyntaxType(),
                ~onLastSyntaxTypeChange=MM_cmp_user_stmt.setLastSyntaxType,
            ) {
                | Error(msg) => st->setSyntaxProofTableError(Some(msg))
                | Ok(proofTables) => {
                    switch proofTables[0] {
                        | Error(msg) => st->setSyntaxProofTableError(Some(msg))
                        | Ok(proofTable) => {
                            let st = st->setProofTable(~proofTable, ~dummyVarDisj=disjMake())
                            let st = st->setShowTypes(true)
                            st
                        }
                    }
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

    let pRecIdxToLabel = (state, proofTable, pRecIdx:int, maxUsedStepNum:option<int>):string => {
        switch proofTable[pRecIdx].proof {
            | Hypothesis({label}) => label
            | Assertion(_) => {
                let stepNum = getStepNum(state,pRecIdx)
                if (
                    maxUsedStepNum->Belt_Option.map(maxUsedStepNum => stepNum <= maxUsedStepNum)
                        ->Belt_Option.getWithDefault(false)
                ) {
                    maxUsedStepNum->Belt_Option.getExn + 1
                } else {
                    stepNum
                }->Belt_Int.toString
            }
        }
    }

    let actLoadProofToEditor = (~state:state, ~adjustContext:bool, ~loadSteps:bool) => {
        loadEditorState.current->Js.Nullable.toOption->Belt.Option.forEach(loadEditorState => {
            let vars = []
            state.frmCtx->forEachHypothesisInDeclarationOrder(hyp => {
                if (hyp.typ == F) {
                    switch preCtxData.ctxV.val->getTokenType(state.frmCtx->ctxIntToSymExn(hyp.expr[1])) {
                        | Some(V) => ()
                        | None | Some(C) | Some(F) | Some(E) | Some(A) | Some(P) => {
                            vars->Js.Array2.push(`${hyp.label} ${state.frmCtx->ctxIntsToStrExn(hyp.expr)}`)->ignore
                        }
                    }
                }
                None
            })->ignore

            let disjArr = []
            let frmDisj = disjMake()
            state.frame.disj->Belt_MapInt.forEach((n,ms) => {
                ms->Belt_SetInt.forEach(m => {
                    frmDisj->disjAddPair(n,m)
                })
            })
            frmDisj->disjForEachArr(disjGrp => {
                disjArr->Js.Array2.push(
                    preCtxData.ctxV.val->frmIntsToSymsExn(state.frame, disjGrp)->Js.Array2.joinWith(",")
                )->ignore
            })
            switch state.dummyVarDisj {
                | None => ()
                | Some(dummyVarDisj) => {
                    dummyVarDisj->disjForEachArr(disjGrp => {
                        disjArr->Js.Array2.push(
                            state.frmCtx->ctxIntsToSymsExn(disjGrp)->Js.Array2.joinWith(",")
                        )->ignore
                    })
                }
            }
            
            let maxUsedStepNum = ref(0)
            let stmts = []
            state.frame.hyps->Js_array2.forEach(hyp => {
                if (hyp.typ == E) {
                    switch Belt_Int.fromString(hyp.label) {
                        | None => ()
                        | Some(i) => {
                            if (maxUsedStepNum.contents < i) {
                                maxUsedStepNum := i
                            }
                        }
                    }
                    stmts->Js.Array2.push(
                        {
                            label: hyp.label, 
                            typ: userStmtTypeToStr(E), 
                            isGoal: false,
                            isBkm: false,
                            cont: preCtxData.ctxV.val->frmIntsToStrExn(state.frame, hyp.expr),
                            jstfText: "",
                        }
                    )->ignore
                }
            })
            switch state.proofTable {
                | None => ()
                | Some(proofTable) => {
                    let idxToLabel = Belt_HashMapInt.make(~hintSize=proofTable->Js_array2.length)
                    proofTable
                        ->Js_array2.mapi((pRec,idx) => (pRec,idx))
                        ->Js_array2.filter(((_,idx)) => state.essIdxs->Belt_HashSetInt.has(idx))
                        ->Js.Array2.forEach(((pRec,idx)) => {
                            switch pRec.proof {
                                | Hypothesis({label}) => idxToLabel->Belt_HashMapInt.set(idx,label)
                                | Assertion({args,label}) => {
                                    let isGoal = idx == proofTable->Js_array2.length - 1
                                    if (loadSteps || isGoal) {
                                        let jstfArgs = []
                                        for i in 0 to args->Js.Array2.length-1 {
                                            let argIdx = args[i]
                                            if (state.essIdxs->Belt_HashSetInt.has(argIdx)) {
                                                let label = switch idxToLabel->Belt_HashMapInt.get(argIdx) {
                                                    | None => "err_" ++ argIdx->Belt_Int.toString
                                                    | Some(str) => str
                                                }
                                                jstfArgs->Js.Array2.push(label)->ignore
                                            }
                                        }
                                        let jstfText = if (loadSteps) {
                                            jstfToStr({args:jstfArgs, label})
                                        } else {
                                            ""
                                        }
                                        let label = if (isGoal) {
                                            state.frame.label
                                        } else {
                                            pRecIdxToLabel(state,proofTable,idx,Some(maxUsedStepNum.contents))
                                        }
                                        idxToLabel->Belt_HashMapInt.set(idx,label)
                                        switch Belt_Int.fromString(label) {
                                            | None => ()
                                            | Some(i) => {
                                                if (maxUsedStepNum.contents < i) {
                                                    maxUsedStepNum := i
                                                }
                                            }
                                        }
                                        stmts->Js.Array2.push(
                                            {
                                                label, 
                                                typ: userStmtTypeToStr(P), 
                                                isGoal,
                                                isBkm:false,
                                                cont: state.frmCtx->ctxIntsToStrExn(pRec.expr),
                                                jstfText,
                                            }
                                        )->ignore
                                    }
                                }
                            }
                        })
                }
            }
            let srcs = if (adjustContext) {
                switch convertMmScopesToMmCtxSrcDtos(~origMmCtxSrcDtos=preCtxData.srcs, ~mmScopes=state.frmMmScopes) {
                    | None => []
                    | Some(srcs) => srcs
                }
            } else {
                []
            }
            loadEditorState(
                {
                    srcs,
                    descr: state.frame.descr->Belt.Option.getWithDefault(""),
                    varsText: vars->Js.Array2.joinWith("\n"),
                    disjText: disjArr->Js.Array2.joinWith("\n"),
                    stmts,
                }
            )
            focusEditorTab()
        })
    }

    let actOpenLoadProofToEditorDialog = state => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_load_proof_to_editor
                    onOk={(~adjustContext:bool,~loadSteps:bool)=>{
                        actLoadProofToEditor(~state,~adjustContext,~loadSteps)
                        closeModal(modalRef, modalId)
                    }}
                    onCancel={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
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

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch preCtxData.frms->frmsGetByLabel(label) {
            | None => None
            | Some(frm) => MM_react_common.getFrmLabelBkgColor(frm.frame, preCtxData.settingsV.val)
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
        <Row
            spacing = 0.
            alignItems=#center
            childXsOffset = {idx => {
                switch idx {
                    | 1 => Some(Js.Json.string("auto"))
                    | _ => None
                }
            }}
        >
            <span>
                asrtType
                {React.string(" ")}
                <span 
                    style=ReactDOM.Style.make(
                        ~fontWeight="bold", 
                        ~cursor="pointer", 
                        ~backgroundColor=?getFrmLabelBkgColor(state.frame.label),
                        ~borderRadius="3px",
                        ()
                    )
                >
                    { React.string(state.frame.label) }
                </span>
                <span 
                    style=ReactDOM.Style.make(~fontFamily="Arial Narrow", ~fontSize="x-small", ~color="grey", ~marginLeft="5px", ())
                >
                    { (" " ++ (state.frms->frmsSize+1)->Belt_Int.toString)->React.string }
                </span>
            </span>
            { 
                rndIconButton(~icon=<MM_Icons.Menu/>, ~onClick=actOpenMainMenu, ~active=true, 
                    ~ref=ReactDOM.Ref.domRef(mainMenuButtonRef),
                    ~title="Additional actions", () )
            }
        </Row>
    }

    let rndMainMenu = state => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Js.Nullable.toOption {
                | None => React.null
                | Some(mainMenuButtonRef) => {
                    <Menu
                        opn=true
                        anchorEl=mainMenuButtonRef
                        onClose=actCloseMainMenu
                    >
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                toggleCtxSelector.current->Js.Nullable.toOption
                                    ->Belt.Option.forEach(toggleCtxSelector => toggleCtxSelector())
                            }}
                        >
                            {React.string(if ctxSelectorIsExpanded {"Hide context"} else {"Show context"})}
                        </MenuItem>
                        <MenuItem
                            disabled={state.frame.isAxiom}
                            onClick={() => {
                                actCloseMainMenu()
                                actOpenLoadProofToEditorDialog(state)
                            }}
                        >
                            {React.string("Load this proof to the editor")}
                        </MenuItem>
                    </Menu>
                }
            }
        } else {
            React.null
        }
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
                                        frameRestrict=preCtxData.settingsV.val.allowedFrms.inSyntax
                                        parenCnt=state.parenCnt
                                        stmt=hyp.expr
                                        symColors=state.symColors
                                        symRename=None
                                        editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                                        openExplorer
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
                            frameRestrict=preCtxData.settingsV.val.allowedFrms.inSyntax
                            parenCnt=state.parenCnt
                            stmt=state.asrt
                            symColors=state.symColors
                            symRename=None
                            editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                            openExplorer
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
        switch state.disjStr {
            | None => <div style=ReactDOM.Style.make(~display="none", ()) />
            | Some(disj) => {
                <div>
                    { (`Distinct variable groups:` ++ MM_cmp_pe_frame_summary_state.disjGrpDelim)->React.string }
                    {MM_cmp_pe_frame_summary_state.rndDisj(disj)}
                </div>
            }
        }
    }

    let rndDummyVarDisj = state => {
        switch state.dummyVarDisjStr {
            | None => <div style=ReactDOM.Style.make(~display="none", ()) />
            | Some(disj) => {
                <div>
                    { (`Distinct dummy variable groups:` ++ MM_cmp_pe_frame_summary_state.disjGrpDelim)->React.string }
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
                <span 
                    style={
                        linkStyle->ReactDOM.Style.combine(
                            ReactDOM.Style.make(~backgroundColor=?getFrmLabelBkgColor(label), ~borderRadius="3px", ())
                        )
                    }
                    onClick={clickHnd(~act=()=>openFrameExplorer(label),())}
                >
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
            frameRestrict=preCtxData.settingsV.val.allowedFrms.inSyntax
            parenCnt=state.parenCnt
            stmt=pRec.expr
            symColors=state.symColors
            symRename=None
            editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
            openExplorer
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
                {rndMainMenu(state)}
                {rndLabel(state)}
                {rndDescr(state)}
                {rndDisj(state)}
                {rndDummyVarDisj(state)}
                {rndSummary(state)}
                {rndProof(state)}
                {rndFooter()}
            </Col>
        }
    }

}, propsAreSame)