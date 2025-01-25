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
    proofRecordsPerPage: int,
    pageIdx: int,
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
            frame.hyps->Array.forEachWithIndex((hyp,i) => {
                hypsStr->Array.push(ctx->frmIntsToSymsExn(frame, hyp.expr))
                if (hyp.typ == E) {
                    eHyps->Array.push(i)
                }
            })
            let subs = Belt_HashMapString.make(~hintSize=frame.numOfVars)
            frame.hyps->Array.forEachWithIndex((hyp,i) => {
                if (hyp.typ == F) {
                    subs->Belt_HashMapString.set(
                        frame.frameVarToSymb->Array.getUnsafe(hyp.expr->Array.getUnsafe(1)),
                        exprsStr->Array.getUnsafe(args->Array.getUnsafe(i))->Array.sliceToEnd(~start=1)
                    )
                }
            })
            let frmColors = Belt_HashMapString.make(~hintSize=frame.numOfVars)
            for i in 0 to frame.numOfVars-1 {
                switch typeColors->Belt_HashMapString.get(ctx->ctxIntToSymExn(frame.varTypes->Array.getUnsafe(i))) {
                    | None => ()
                    | Some(color) => frmColors->Belt_HashMapString.set( frame.frameVarToSymb->Array.getUnsafe(i), color )
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

let calcProofRecordsPerPage = (proofTable:proofTable):int => {
    let totalNumOfSymbols = proofTable->Array.reduce(0, (acc, r) => acc + r.expr->Array.length)
    let totalNumOfSteps = proofTable->Array.length
    let avgNumOfSymbolsPerStep = Belt_Int.fromFloat(Belt_Float.fromInt(totalNumOfSymbols) /. Belt_Float.fromInt(totalNumOfSteps))
    let maxSymbolsPerPage = 50_000
    let maxStepsPerPage = Belt_Int.fromFloat(Belt_Float.fromInt(maxSymbolsPerPage) /. Belt_Float.fromInt(avgNumOfSymbolsPerStep))
    Math.Int.min(maxStepsPerPage, 500)
}

let setProofTable = (st:state, ~proofTable:proofTable, ~dummyVarDisj:disjMutable):state => {
    let frmCtx = st.frmCtx
    let settings = st.settings
    let typeColors = settings->settingsGetTypeColors
    let exprsStr = proofTable->Array.map(r => frmCtx->ctxIntsToSymsExn(r.expr))
    let vData = proofTable->Array.map(pRec => {
        createVDataRec( ~ctx=frmCtx, ~pRec, ~typeColors, ~exprsStr)
    })
    let essIdxs = Belt_HashSetInt.make(~hintSize=1000)
    let stepRenum = Belt_HashMapInt.make(~hintSize=1000)
    proofTable->Array.forEach(pRec => {
        switch pRec.proof {
            | Hypothesis(_) => ()
            | Assertion({args, label}) => {
                let frame = frmCtx->getFrameExn(label)
                frame.hyps->Array.forEachWithIndex((hyp,i) => {
                    if (hyp.typ == E) {
                        essIdxs->Belt_HashSetInt.add(args->Array.getUnsafe(i))
                    }
                })
            }
        }
    })
    essIdxs->Belt_HashSetInt.add(proofTable->Array.length-1)
    proofTable->Array.forEachWithIndex((_,i) => {
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
                ~intToTyp=getTypeOfVar(frmCtx, _),
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
        proofRecordsPerPage: calcProofRecordsPerPage(proofTable),
    }
}

let createInitialState = (
    ~settings:settings, 
    ~frmMmScopes:array<mmScope>,
    ~preCtx:mmContext, 
    ~frmCtx:mmContext, 
    ~frame:frame
):state => {
    let frmCtx = frmCtx->ctxOptimizeForProver(~parens=settings.parens, ~removeAsrtDescr=false, ~removeProofs=false)
    let frms = prepareFrmSubsData( ~ctx=frmCtx )
    let parenCnt = MM_provers.makeParenCnt(~ctx=frmCtx, ~parens=settings.parens)
    let (_, syntaxTypes) = findTypes(frmCtx)

    let frmIntToCtxInt = (i:int):int => {
        switch frmCtx->ctxSymToInt(
            if (i < 0) { preCtx->ctxIntToSymExn(i) } else { frame.frameVarToSymb->Array.getUnsafe(i) }
        ) {
            | None => raise(MmException({msg:`preCtx->ctxSymToInt == None in frmIntToCtxInt`}))
            | Some(n) => n
        }
    }

    let ctxVarToFrmVar = (i:int):option<int> => {
        switch frame.frameVarToSymb->Array.indexOf(frmCtx->ctxIntToSymExn(i)) {
            | -1 => None
            | n => Some(n)
        }
    }

    let frmExprToCtxExpr = (expr:expr):expr => {
        expr->Array.map(frmIntToCtxInt)
    }

    let typeColors = settings->settingsGetTypeColors
    let symColors = createSymbolColors(~ctx=frmCtx, ~typeColors)
    let asrt = frame.asrt->frmExprToCtxExpr

    let typeOrderInDisj = createTypeOrderFromStr(
        ~sortDisjByType=settings.sortDisjByType, 
        ~typeNameToInt=ctxSymToInt(frmCtx,_)
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
        hyps:frame.hyps->Array.map(hyp => {...hyp, expr:frmExprToCtxExpr(hyp.expr)}),
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
        proofRecordsPerPage: 300,
        pageIdx:0,
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
                    st->setProofTable(~proofTable=createProofTableFromProof(~proofNode=proofRoot), ~dummyVarDisj)
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
    if (st.expandedIdxs->Array.includes(idx)) {
        {
            ...st, 
            expandedIdxs: st.expandedIdxs->Array.filter(i => i != idx)
        }
    } else {
        {
            ...st, 
            expandedIdxs: st.expandedIdxs->Array.concat([idx])
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
    openExplorer:(~initPatternFilterStr:string=?)=>unit,
    openEditor: editorStateLocStor => unit,
    toggleCtxSelector:React.ref<Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
}

let propsAreSame = (a:props, b:props):bool => {
    a.top === b.top 
    && a.ctxSelectorIsExpanded === b.ctxSelectorIsExpanded
    && a.preCtxData.ctxV.ver === b.preCtxData.ctxV.ver
}

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool, 
    ~notifyEditInTempMode:option<(unit=>'a)=>'a>=?,
    ~ref:option<ReactDOM.domRef>=?,
    ~title:option<string>=?, 
    ~smallBtns:bool=false
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
    let canConvert = mmScopes->Array.length <= origMmCtxSrcDtos->Array.length 
                        && mmScopes->Array.everyWithIndex((mmScope,i) => {
                            (origMmCtxSrcDtos->Array.getUnsafe(i)).ast
                                ->Belt_Option.map(origAst => origAst == mmScope.ast)
                                ->Belt.Option.getWithDefault(false)
                        })
    if (!canConvert) {
        None
    } else {
        Some(
            mmScopes->Array.mapWithIndex((mmScope,i) => {
                let origMmCtxSrcDto = origMmCtxSrcDtos->Array.getUnsafe(i)
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
                                    ~preCtx=preCtxData.ctxV.val.full,
                                    ~frmCtx,
                                    ~frame=preCtxData.ctxV.val.full->getFrameExn(label)
                                )
                            )
                        )
                    }
                }
            },
        )
    })
}

let getStepNum = (state,pRecIdx:int):int => {
    if (state.showTypes) {
        pRecIdx + 1
    } else {
        state.stepRenum->Belt_HashMapInt.get(pRecIdx)->Belt.Option.map(n => n + 1)->Belt.Option.getWithDefault(0)
    }
}

let pRecIdxToLabel = (state, proofTable, pRecIdx:int, maxUsedStepNum:option<int>):string => {
    switch (proofTable->Array.getUnsafe(pRecIdx)).proof {
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

let frameProofDataToEditorStateLocStor = (
    ~preCtxData:preCtxData,
    ~frameProofData:frameProofData, 
    ~adjustContext:bool, 
    ~loadSteps:bool
):editorStateLocStor => {
    let vars = []
    frameProofData.frmCtx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F) {
            switch preCtxData.ctxV.val.full->getTokenType(frameProofData.frmCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(1))) {
                | Some(V) => ()
                | None | Some(C) | Some(F) | Some(E) | Some(A) | Some(P) => {
                    vars->Array.push(`${hyp.label} ${frameProofData.frmCtx->ctxIntsToStrExn(hyp.expr)}`)
                }
            }
        }
        None
    })->ignore

    let disjArr = []
    let frmDisj = disjMake()
    frameProofData.frame.disj->Belt_MapInt.forEach((n,ms) => {
        ms->Belt_SetInt.forEach(m => {
            frmDisj->disjAddPair(n,m)
        })
    })
    frmDisj->disjForEachArr(disjGrp => {
        disjArr->Array.push(
            preCtxData.ctxV.val.full->frmIntsToSymsExn(frameProofData.frame, disjGrp)->Array.joinUnsafe(" ")
        )
    })
    switch frameProofData.dummyVarDisj {
        | None => ()
        | Some(dummyVarDisj) => {
            dummyVarDisj->disjForEachArr(disjGrp => {
                disjArr->Array.push(
                    frameProofData.frmCtx->ctxIntsToSymsExn(disjGrp)->Array.joinUnsafe(" ")
                )
            })
        }
    }
    
    let maxUsedStepNum = ref(0)
    let stmts = []
    frameProofData.frame.hyps->Array.forEach(hyp => {
        if (hyp.typ == E) {
            switch Belt_Int.fromString(hyp.label) {
                | None => ()
                | Some(i) => {
                    if (maxUsedStepNum.contents < i) {
                        maxUsedStepNum := i
                    }
                }
            }
            stmts->Array.push(
                {
                    label: hyp.label, 
                    typ: userStmtTypeToStr(E), 
                    isGoal: false,
                    isBkm: false,
                    cont: preCtxData.ctxV.val.full->frmIntsToStrExn(frameProofData.frame, hyp.expr),
                    jstfText: "",
                }
            )
        }
    })
    switch frameProofData.proofTable {
        | None => ()
        | Some(proofTable) => {
            let idxToLabel = Belt_HashMapInt.make(~hintSize=proofTable->Array.length)
            proofTable
                ->Array.mapWithIndex((pRec,idx) => (pRec,idx))
                ->Array.filter(((_,idx)) => frameProofData.essIdxs->Belt_HashSetInt.has(idx))
                ->Array.forEach(((pRec,idx)) => {
                    switch pRec.proof {
                        | Hypothesis({label}) => idxToLabel->Belt_HashMapInt.set(idx,label)
                        | Assertion({args,label}) => {
                            let isGoal = idx == proofTable->Array.length - 1
                            if (loadSteps || isGoal) {
                                let jstfArgs = []
                                for i in 0 to args->Array.length-1 {
                                    let argIdx = args->Array.getUnsafe(i)
                                    if (frameProofData.essIdxs->Belt_HashSetInt.has(argIdx)) {
                                        let label = switch idxToLabel->Belt_HashMapInt.get(argIdx) {
                                            | None => "err_" ++ argIdx->Belt_Int.toString
                                            | Some(str) => str
                                        }
                                        jstfArgs->Array.push(label)
                                    }
                                }
                                let jstfText = if (loadSteps) {
                                    jstfToStr({args:jstfArgs, label})
                                } else {
                                    ""
                                }
                                let label = if (isGoal) {
                                    frameProofData.frame.label
                                } else {
                                    pRecIdxToLabel(frameProofData,proofTable,idx,Some(maxUsedStepNum.contents))
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
                                stmts->Array.push(
                                    {
                                        label, 
                                        typ: userStmtTypeToStr(P), 
                                        isGoal,
                                        isBkm:false,
                                        cont: frameProofData.frmCtx->ctxIntsToStrExn(pRec.expr),
                                        jstfText,
                                    }
                                )
                            }
                        }
                    }
                })
        }
    }
    let srcs = if (adjustContext) {
        switch convertMmScopesToMmCtxSrcDtos(~origMmCtxSrcDtos=preCtxData.srcs, ~mmScopes=frameProofData.frmMmScopes) {
            | None => []
            | Some(srcs) => srcs
        }
    } else {
        []
    }
    {
        tabTitle: frameProofData.frame.label,
        srcs,
        descr: frameProofData.frame.descr->Belt.Option.getWithDefault(""),
        varsText: vars->Array.joinUnsafe("\n"),
        disjText: disjArr->Array.joinUnsafe("\n"),
        stmts,
    }
}

let frameProofDataToStmtsDto = (
    ~preCtxData:preCtxData,
    ~wrkCtx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~args:array<int>, 
    ~frameProofData:frameProofData,
):result<stmtsDto,string> => {
    let frmVarToCtxExpr:Belt_HashMapString.t<string> = args->Array.mapWithIndex((arg,i) => {
            let hyp = frameProofData.frame.hyps->Array.getUnsafe(i)
            if (hyp.typ == F) {
                Some(
                    (
                        wrkCtx->frmIntToSymExn(frameProofData.frame, hyp.expr->Array.getUnsafe(1)),
                        wrkCtx->ctxIntsToStrExn((proofTreeDto.nodes->Array.getUnsafe(arg)).expr->Array.sliceToEnd(~start=1)),
                    )
                )
            } else {
                None
            }
        })
        ->Array.filter(Belt_Option.isSome(_))
        ->Array.map(Belt_Option.getExn(_))
        ->Belt_HashMapString.fromArray
    let locSt = frameProofDataToEditorStateLocStor(
        ~preCtxData, ~frameProofData, ~adjustContext=false, ~loadSteps=true
    )
    let stmts = locSt.stmts->Array.reduce(Ok([]), (res,stmt) => {
        switch res {
            | Error(_) => res
            | Ok(stmts) => {
                stmts->Array.push({
                    label:stmt.label,
                    expr:
                        stmt.cont->getSpaceSeparatedValuesAsArray->Array.map(sym => {
                            switch frmVarToCtxExpr->Belt_HashMapString.get(sym) {
                                | Some(exprStr) => exprStr
                                | None => sym
                            }
                        })->Array.joinUnsafe(" ")->ctxStrToIntsExn(wrkCtx, _),
                    exprStr:"",
                    jstf:stmt.jstfText->parseJstf->Belt_Result.getWithDefault(None),
                    isProved: true,
                })
                res
            }
        }
    })
    // let res = locSt.varsText->MM_wrk_ctx_data.textToVarDefs->Belt_Result.map(parsedVars => {
    //     let numOfExistingVars = wrkCtx->getNumOfVars
    //     {
    //         newVars: Belt_Array.range(numOfExistingVars, numOfExistingVars + parsedVars->Array.length - 1),
    //         newVarTypes: parsedVars->Array.map(@warning("-8")([_, typ, _]) => wrkCtx->ctxSymToIntExn(typ)),
    //         newDisj: disjMake(),
    //         newDisjStr: [],
    //         stmts: [],
    //     }
    // })
    stmts->Belt_Result.map(stmts => {
        {
            newVars: [],
            newVarTypes: [],
            newDisj: disjMake(),
            newDisjStr: [],
            stmts: stmts,
        }
    })
}

let make = React.memoCustomCompareProps(({
    top,
    modalRef,
    preCtxData,
    label,
    openFrameExplorer,
    openExplorer,
    openEditor,
    toggleCtxSelector,
    ctxSelectorIsExpanded,
}:props) => {
    let (lastPreCtxVer, setLastPreCtxVer) = React.useState(() => preCtxData.ctxV.ver)
    let (refreshIsNeeded, setRefreshIsNeeded) = React.useState(() => false)

    let (loadPct, setLoadPct) = React.useState(() => 0.)
    let (loadErr, setLoadErr) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (stepWidth, setStepWidth) = React.useState(() => "100px")
    let (hypWidth, setHypWidth) = React.useState(() => "100px")
    let (refWidth, setRefWidth) = React.useState(() => "100px")

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Nullable.null)

    let actPreCtxDataChanged = () => {
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
                                setLoadPct(_ => 1.0)
                                switch catchExn(() => {
                                    createInitialState(
                                        ~settings=preCtxData.settingsV.val, 
                                        ~frmMmScopes,
                                        ~preCtx=preCtxData.ctxV.val.full,
                                        ~frmCtx,
                                        ~frame=preCtxData.ctxV.val.full->getFrameExn(label)
                                    )
                                }) {
                                    | Ok(state) => setState(_ => Some(state))
                                    | Error({msg}) => setLoadErr(_ => Some(msg))
                                }
                            }
                        }
                    },
                )
            },
            10
        )->ignore
    }

    let actRefreshOnPreCtxDataChange = () => {
        actPreCtxDataChanged()
        setLastPreCtxVer( _ => preCtxData.ctxV.ver )
        setRefreshIsNeeded(_ => false)
    }

    React.useEffect1(() => {
        if (lastPreCtxVer != preCtxData.ctxV.ver) {
            setRefreshIsNeeded(_ => true)
            setState(_ => None)
            setLoadErr(_ => None)
        } else {
            actPreCtxDataChanged()
        }
        None
    }, [preCtxData.ctxV.ver])

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
                ~syms = [ctx->ctxIntsToSymsExn(st.asrt->Array.sliceToEnd(_, ~start=1))],
                ~syntaxTypes = st.syntaxTypes, 
                ~frms = st.frms,
                ~frameRestrict=preCtxData.settingsV.val.allowedFrms.inSyntax, 
                ~parenCnt = st.parenCnt, 
                ~lastSyntaxType=MM_cmp_user_stmt.getLastSyntaxType(),
                ~onLastSyntaxTypeChange=MM_cmp_user_stmt.setLastSyntaxType,
            ) {
                | Error(msg) => st->setSyntaxProofTableError(Some(msg))
                | Ok(proofTables) => {
                    switch proofTables->Array.getUnsafe(0) {
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

    let actLoadProofToEditor = (~state:state, ~adjustContext:bool, ~loadSteps:bool) => {
        openEditor(
            frameProofDataToEditorStateLocStor(
                ~preCtxData,
                ~frameProofData=state, 
                ~adjustContext, 
                ~loadSteps
            )
        )
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
                            proofTable->Array.length
                        } else {
                            proofTable->Array.reduceWithIndex(
                                0,
                                (cnt,_,idx) => {
                                    cnt + if (state.essIdxs->Belt_HashSetInt.has(idx)) {1} else {0}
                                }
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
        let proofTableIdCssSelector = proofTableId->String.replaceRegExp(dotPattern, "\\.")
        setStepWidth(_ => calcColumnWidth(`#${proofTableIdCssSelector} .${classColStep}`, 30, 1000)->Belt.Int.toString ++ "px")
        setHypWidth(_ => (calcColumnWidth(`#${proofTableIdCssSelector} .${classColHyp}`, 30, 100)+5)->Belt.Int.toString ++ "px")
        setRefWidth(_ => calcColumnWidth(`#${proofTableIdCssSelector} .${classColRef}`, 30, 1000)->Belt.Int.toString ++ "px")
        None
    }, [numberOfRowsInProofTable, state->Option.mapOr(0, st => st.pageIdx)])

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
                    | 1 => Some(JSON.Encode.string("auto"))
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
                    ~title="Additional actions" )
            }
        </Row>
    }

    let rndMainMenu = state => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Nullable.toOption {
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
                                toggleCtxSelector.current->Nullable.toOption
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
                            {React.string("Open this proof in a new editor tab")}
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
                        ->Array.filter(hyp => hyp.typ == E)
                        ->Array.mapWithIndex((hyp,i) => {
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
                                        openExplorer=Some(openExplorer)
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
                            openExplorer=Some(openExplorer)
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

    let getAllProofRecordsToShow = (state:state):array<(proofRecord,int)> => {
        switch state.proofTable {
            | None => []
            | Some(proofTable) => {
                proofTable->Array.mapWithIndex((pRec,idx) => (pRec,idx))->Array.filter(((_,idx)) => {
                    if (state.showTypes) {true} else {state.essIdxs->Belt_HashSetInt.has(idx)}
                })
            }
        }
    }

    let actGoToPage = (pageIdx) => {
        setState(st => st->Option.map(st => {...st, pageIdx}))
    }

    let actGoToPageWithStep = (idx:int) => {
        switch state {
            | None => ()
            | Some(state) => {
                switch getAllProofRecordsToShow(state)->Array.findIndexOpt(((_,pIdx)) => pIdx == idx) {
                    | None => ()
                    | Some(showedIdx) => {
                        let pageIdx = Math.Int.floor(
                            Belt_Float.fromInt(showedIdx) /. Belt_Float.fromInt(state.proofRecordsPerPage)
                        )
                        actGoToPage(pageIdx)
                    }
                }
            }
        }
    }

    let actHypIdxClicked = (clickedIdx:int) => {
        actGoToPageWithStep(clickedIdx)
        setTimeout(
            () => removeQueryParamsFromUrl("Removing proof record id."),
            2000
        )->ignore
    }

    let rndHyp = (state,pRec:proofRecord):reElem => {
        switch pRec.proof {
            | Hypothesis(_) => React.null
            | Assertion({args}) => {
                let elems = []
                for i in 0 to args->Array.length-1 {
                    let argIdx = args->Array.getUnsafe(i)
                    if (state.showTypes || state.essIdxs->Belt_HashSetInt.has(argIdx)) {
                        let iStr = i->Belt_Int.toString
                        if (elems->Array.length > 0) {
                            elems->Array.push(
                                <span key={"delim-" ++ iStr}>
                                    {React.string(", ")}
                                </span>
                            )
                        }
                        elems->Array.push(
                            <a href={"#" ++ proofTableId ++ "-" ++ argIdx->Belt_Int.toString} key={"hyp-" ++ iStr}
                                onClick=clickHnd(~act=()=>actHypIdxClicked(argIdx))
                            >
                                {React.string(getStepNum(state,argIdx)->Belt_Int.toString)}
                            </a>
                        )
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
                    onClick={clickHnd(~act=()=>openFrameExplorer(label))}
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
        let style = switch state.vData->Array.getUnsafe(pRecIdx) {
            | None => expBtnBaseStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~opacity="0", ()))
            | Some(_) => expBtnBaseStyle
        }
        <a style onClick={_=>actToggleIdxExpanded(pRecIdx)}>
            {React.string(if (state.expandedIdxs->Array.includes(pRecIdx)) {"-"} else {"+"})}
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
            openExplorer=Some(openExplorer)
        />
    }

    let rndExprSvg = (~state:state, ~vDataRec:vDataRec):reElem => {
        let labelIdxs = if (state.showTypes) {
            vDataRec.hypLabels
        } else {
            vDataRec.eHyps->Array.map(i => vDataRec.hypLabels->Array.getUnsafe(i))
        }
        <MM_cmp_jstf_to_svg
            hyps={if (state.showTypes) {vDataRec.hyps} else {vDataRec.eHyps->Array.map(i => vDataRec.hyps->Array.getUnsafe(i))}}
            hypLabels={labelIdxs->Array.map(i => getStepNum(state,i)->Belt_Int.toString)}
            asrt=vDataRec.asrt
            frmColors=Some(vDataRec.frmColors)
            ctxColors1=Some(state.symColors)
            ctxColors2=None
            subs=vDataRec.subs
            onLabelClick = {(i,_) => {
                location["hash"] = "#" ++ proofTableId ++ "-" ++ labelIdxs->Array.getUnsafe(i)->Belt_Int.toString
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
                            switch state.vData->Array.getUnsafe(pRecIdx) {
                                | None => rndExprText(~state, ~pRec)
                                | Some(vDataRec) => {
                                    if (state.expandedIdxs->Array.includes(pRecIdx)) {
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

    let rndPagination = state => {
        switch state.proofTable {
            | None => React.null
            | Some(_) => {
                let proofTableSize = getAllProofRecordsToShow(state)->Array.length
                if (proofTableSize < state.proofRecordsPerPage) {
                    React.null
                } else {
                    <div style=ReactDOM.Style.make(~padding="5px", ())>
                        <PaginationCmp
                            numOfPages=Math.Int.ceil(
                                Belt_Float.fromInt(proofTableSize) /. Belt_Float.fromInt(state.proofRecordsPerPage)
                            )
                            pageIdx=state.pageIdx
                            siblingCount=1000
                            showGoToPage=false
                            onPageIdxChange=actGoToPage
                            itemsPerPage=state.proofRecordsPerPage
                            showItemsPerPage=false
                        />
                    </div>
                }
            }
        }
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
            | Some(_) => {
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
                                let minIdx = state.pageIdx * state.proofRecordsPerPage
                                let maxIdx = minIdx + state.proofRecordsPerPage - 1
                                getAllProofRecordsToShow(state)
                                    ->Array.filterWithIndex((_, idx) => minIdx <= idx && idx <= maxIdx)
                                    ->Array.map(((pRec,idx)) => {
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
        Belt_Array.range(1,12)->Array.map(i => {
            <span key={i->Belt_Int.toString} style=ReactDOM.Style.make(~fontSize="20px", ())>{nbsp->React.string}</span>
        })->React.array
    }

    if (refreshIsNeeded) {
        <Button onClick=(_=>actRefreshOnPreCtxDataChange()) variant=#contained 
            style=ReactDOM.Style.make(~margin="10px", ())
        > 
            { React.string("Refresh") }
        </Button>
    } else {
        switch state {
            | None => {
                switch loadErr {
                    | Some(msg) => {
                        <pre style=ReactDOM.Style.make(~color="red", ~margin="10px", ())>
                            {React.string(`Error: ${msg}`)}
                        </pre>
                    }
                    | None => {
                        if (loadPct < 1.0) {
                            `Loading the context ${floatToPctStr(loadPct)}`->React.string
                        } else {
                            `Building the proof table...`->React.string
                        }
                    }
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
                    {rndPagination(state)}
                    {rndProof(state)}
                    {rndPagination(state)}
                    {rndFooter()}
                </Col>
            }
        }
    }

}, propsAreSame)