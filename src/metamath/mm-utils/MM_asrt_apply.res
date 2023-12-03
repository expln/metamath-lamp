open MM_substitution
open MM_context
open MM_parenCounter
open MM_progress_tracker
open MM_unification_debug
open MM_parser

type applyAssertionResult = {
    newVars: array<int>,
    newVarTypes: array<int>,
    frame: frame,
    subs: subs,
    err:option<unifErr>,
}

let applyAssertionResultEq = (a:applyAssertionResult, b:applyAssertionResult):bool => {
    a.err->Belt_Option.isNone && b.err->Belt_Option.isNone && a.frame.label == b.frame.label && subsEq(a.subs, b.subs)
}

let applyAssertionResultHash = (a:applyAssertionResult):int => {
    Expln_utils_common.hash2(
        Expln_utils_common.hashStr(a.frame.label),
        subsHash(a.subs)
    )
}

module ApplyAssertionResultHash = Belt.Id.MakeHashable({
    type t = applyAssertionResult
    let hash = applyAssertionResultHash
    let eq = applyAssertionResultEq
})

let rec iterateCombinationsRec = (
    ~candidatesPerHyp:array<array<int>>,
    ~comb:array<int>,
    ~hypIdx:int,
    ~skipCombinationsWithEmptyArgs:bool,
    ~skipCombinationsWithoutEmptyArgs:bool,
    ~combCnt:ref<int>,
    ~combCntMax:int,
    ~onCombCntMaxReached:unit=>contunieInstruction,
    ~combinationConsumer:array<int>=>contunieInstruction,
):contunieInstruction => {
    if (hypIdx == comb->Js.Array2.length) {
        combCnt := combCnt.contents + 1
        if (combCnt.contents > combCntMax) {
            onCombCntMaxReached()
        } else {
            let thereIsEmptyArg = comb->Js.Array2.some(a => a == -1)
            if (thereIsEmptyArg) {
                if (skipCombinationsWithEmptyArgs) {
                    Continue
                } else {
                    combinationConsumer(comb)
                }
            } else {
                if (skipCombinationsWithoutEmptyArgs) {
                    Continue
                } else {
                    combinationConsumer(comb)
                }
            }
        }
    } else {
        let res = ref(Continue)
        let c = ref(0)
        let maxC = candidatesPerHyp[hypIdx]->Js.Array2.length-1
        while (res.contents == Continue && c.contents <= maxC && combCnt.contents <= combCntMax) {
            comb[hypIdx] = candidatesPerHyp[hypIdx][c.contents]
            if (!(comb[hypIdx] == -1 && skipCombinationsWithEmptyArgs)) {
                res.contents = iterateCombinationsRec(
                    ~candidatesPerHyp,
                    ~comb,
                    ~hypIdx = hypIdx+1,
                    ~skipCombinationsWithEmptyArgs,
                    ~skipCombinationsWithoutEmptyArgs,
                    ~combCnt,
                    ~combCntMax,
                    ~onCombCntMaxReached,
                    ~combinationConsumer
                )
            }
            c.contents = c.contents + 1
        }
        res.contents
    }
}

let iterateCombinations = (
    ~numOfStmts:int,
    ~numOfHyps:int,
    ~stmtCanMatchHyp:(int,int)=>bool,
    ~debugLevel:int,
    ~combinationConsumer:array<int>=>contunieInstruction,
    ~errConsumer:unifErr=>contunieInstruction,
):contunieInstruction => {
    let candidatesPerHyp = Belt_Array.makeBy(numOfHyps, _=>[])
    let maxH = numOfHyps-1
    let maxS = numOfStmts-1
    for h in 0 to maxH {
        for s in -1 to maxS {
            if (stmtCanMatchHyp(s,h)) {
                candidatesPerHyp[h]->Js_array2.push(s)->ignore
            }
        }
    }
    switch candidatesPerHyp->Js_array2.findIndex(candidates => candidates->Js_array2.length == 0) {
        | -1 => {
            let comb = Belt_Array.make(numOfHyps, 0)
            let combCntMax = 10000
            let tooBigSearchSpaceDetected = ref(false)
            let continue = iterateCombinationsRec(
                ~candidatesPerHyp,
                ~comb,
                ~hypIdx = 0,
                ~skipCombinationsWithEmptyArgs=true,
                ~skipCombinationsWithoutEmptyArgs=false,
                ~combCnt=ref(0),
                ~combCntMax,
                ~onCombCntMaxReached = () => {
                    tooBigSearchSpaceDetected := true
                    errConsumer(TooManyCombinations({frmLabels:None}))
                },
                ~combinationConsumer
            )
            if (continue == Continue) {
                iterateCombinationsRec(
                    ~candidatesPerHyp,
                    ~comb,
                    ~hypIdx = 0,
                    ~skipCombinationsWithEmptyArgs=false,
                    ~skipCombinationsWithoutEmptyArgs=true,
                    ~combCnt=ref(0),
                    ~combCntMax,
                    ~onCombCntMaxReached = () => {
                        if (!tooBigSearchSpaceDetected.contents) {
                            errConsumer(TooManyCombinations({frmLabels:None}))
                        } else {
                            Continue
                        }
                    },
                    ~combinationConsumer
                )
            } else {
                continue
            }
        }
        | idx => {
            if (debugLevel >= 2) {
                errConsumer(NoUnifForArg({args:[], errArgIdx:idx}))
            } else {
                Continue
            }
        }
    }
}

let stmtCanMatchHyp = (
    ~frm:frmSubsData,
    ~hypIdx:int,
    ~stmt:expr,
    ~hyp:expr,
    ~parenCnt:parenCnt,
):bool => {
    if (hyp[0] != stmt[0]) {
        false
    } else {
        let res = ref(false)
        iterateSubstitutions(
            ~frmExpr = hyp,
            ~expr = stmt,
            ~frmConstParts = frm.frmConstParts[hypIdx], 
            ~constParts = frm.constParts[hypIdx], 
            ~varGroups = frm.varGroups[hypIdx],
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                res.contents = true
                Stop
            }
        )->ignore
        res.contents
    }
}

let iterateSubstitutionsWithWorkVars = (
    ~workVars:workVars,
    ~frm:frmSubsData,
    ~allowNewVars:bool,
    ~hypIdx: int,
    ~continue: () => contunieInstruction
):contunieInstruction => {
    let initialNumOfWorkVars = workVars.newVars->Js_array2.length
    let predefinedSubs = frm.subs.isDefined->Js_array2.copy

    let nextVar = ref(workVars.maxVar + 1 + workVars.newVars->Js_array2.length)
    let frmVars = []
    let newVars = []
    let newVarTypes = []
    applySubs(
        ~frmExpr = if (hypIdx < frm.hypsE->Js.Array2.length) {frm.hypsE[hypIdx].expr} else {frm.frame.asrt},
        ~subs=frm.subs,
        ~createWorkVar = frmVar => {
            switch frmVars->Js_array2.indexOf(frmVar) {
                | -1 => {
                    let newVar = nextVar.contents
                    nextVar.contents = nextVar.contents + 1
                    frmVars->Js_array2.push(frmVar)->ignore
                    newVars->Js_array2.push(newVar)->ignore
                    newVarTypes->Js_array2.push(frm.frame.varTypes[frmVar])->ignore
                    newVar
                }
                | idx => newVars[idx]
            }
        }
    )->ignore
    let maxI = frmVars->Js_array2.length - 1
    for i in 0 to maxI {
        let frmVar = frmVars[i]
        let newVar = newVars[i]
        let newVarType = newVarTypes[i]

        frm.subs.exprs[frmVar] = [newVar]
        frm.subs.begins[frmVar] = 0
        frm.subs.ends[frmVar] = 0
        frm.subs.isDefined[frmVar] = true

        workVars.newVars->Js_array2.push(newVar)->ignore
        workVars.newVarTypes->Js_array2.push(newVarType)->ignore
    }

    let res = if (allowNewVars || workVars.newVars->Js_array2.length == 0) {
        continue()
    } else {
        Continue
    }

    predefinedSubs->Js_array2.forEachi((predefined,i) => frm.subs.isDefined[i]=predefined)
    workVars.newVars->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore
    workVars.newVarTypes->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore

    res
}

let getNextNonBlankIdx = (hypIdx:int, comb:array<int>):option<int> => {
    let idx = ref(hypIdx+1)
    while (idx.contents < comb->Js_array2.length && comb[idx.contents] < 0) {
        idx := idx.contents + 1
    }
    if (idx.contents < comb->Js_array2.length && comb[idx.contents] >= 0) {
        Some(idx.contents)
    } else {
        None
    }
}

let getNextBlankIdx = (hypIdx:int, comb:array<int>):option<int> => {
    let idx = ref(hypIdx+1)
    while (idx.contents < comb->Js_array2.length && comb[idx.contents] >= 0) {
        idx := idx.contents + 1
    }
    if (idx.contents < comb->Js_array2.length && comb[idx.contents] < 0) {
        Some(idx.contents)
    } else {
        None
    }
}

let getNextHypIdxToMatch = (hypIdx:int, comb:array<int>):int => {
    if (hypIdx >= comb->Js_array2.length) {
        raise(MmException({msg:`getNextHypIdxToMatch: hypIdx >= comb->Js_array2.length`}))
    } else if (hypIdx < 0 || comb[hypIdx] >= 0) {
        switch getNextNonBlankIdx(hypIdx, comb) {
            | Some(idx) => idx
            | None => {
                switch getNextBlankIdx(-1, comb) {
                    | Some(idx) => idx
                    | None => comb->Js_array2.length
                }
            }
        }
    } else {
        switch getNextBlankIdx(hypIdx, comb) {
            | Some(idx) => idx
            | None => comb->Js_array2.length
        }
    }
}

let rec iterateSubstitutionsForHyps = (
    ~workVars:workVars,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
    ~statements:array<expr>,
    ~allowNewVars:bool,
    ~comb:array<int>,
    ~hypIdx:int,
    ~debugLevel:int,
    ~onMatchFound: () => contunieInstruction,
    ~onErrFound: unifErr => contunieInstruction
):contunieInstruction => {
    let combToArgs = () => {
        comb->Js_array2.map(idx => {
            if (idx >= 0) {
                statements[idx]
            } else {
                []
            }
        })
    }

    if (hypIdx == comb->Js.Array2.length) {
        let subsFound = ref(false)
        let contunieInstruction = iterateSubstitutionsWithWorkVars(
            ~workVars,
            ~frm,
            ~allowNewVars,
            ~hypIdx,
            ~continue = () => {
                subsFound.contents = true
                onMatchFound()
            }
        )
        if (debugLevel >= 2 && !subsFound.contents) {
            onErrFound(NewVarsAreDisabled({args:combToArgs(), errArgIdx:hypIdx}))
        } else {
            contunieInstruction
        }
    } else if (comb[hypIdx] >= 0) {
        let subsFound = ref(false)
        let contunieInstruction = iterateSubstitutions(
            ~frmExpr = frm.hypsE[hypIdx].expr,
            ~expr = statements[comb[hypIdx]],
            ~frmConstParts = frm.frmConstParts[hypIdx], 
            ~constParts = frm.constParts[hypIdx], 
            ~varGroups = frm.varGroups[hypIdx],
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                subsFound.contents = true
                iterateSubstitutionsForHyps(
                    ~workVars,
                    ~frm,
                    ~parenCnt,
                    ~statements,
                    ~allowNewVars,
                    ~comb,
                    ~hypIdx = hypIdx->getNextHypIdxToMatch(comb),
                    ~debugLevel,
                    ~onMatchFound,
                    ~onErrFound,
                )
            }
        )
        if (debugLevel >= 2 && !subsFound.contents) {
            onErrFound(NoUnifForArg({args:combToArgs(), errArgIdx:hypIdx}))
        } else {
            contunieInstruction
        }
    } else {
        let subsFound = ref(false)
        let contunieInstruction = iterateSubstitutionsWithWorkVars(
            ~workVars,
            ~frm,
            ~allowNewVars,
            ~hypIdx,
            ~continue = () => {
                subsFound.contents = true
                iterateSubstitutionsForHyps(
                    ~workVars,
                    ~frm,
                    ~parenCnt,
                    ~statements,
                    ~allowNewVars,
                    ~comb,
                    ~hypIdx = hypIdx->getNextHypIdxToMatch(comb),
                    ~debugLevel,
                    ~onMatchFound,
                    ~onErrFound,
                )
            }
        )
        if (debugLevel >= 2 && !subsFound.contents) {
            onErrFound(NewVarsAreDisabled({args:combToArgs(), errArgIdx:hypIdx}))
        } else {
            contunieInstruction
        }
    }
}

let checkDisj = (
    ~frmDisj:Belt_MapInt.t<Belt_SetInt.t>, 
    ~subs:subs, 
    ~maxCtxVar:int, 
    ~allowNewDisjForExistingVars:bool,
    ~isDisjInCtx:(int,int)=>bool,
    ~debugLevel:int,
):option<unifErr> => {
    verifyDisjoints(~frmDisj, ~subs, ~debugLevel, ~isDisjInCtx = (n,m) => {
        if (n <= maxCtxVar && m <= maxCtxVar) {
            isDisjInCtx(n,m) || allowNewDisjForExistingVars
        } else {
            true
        }
    })
}

let iterateSubstitutionsForResult = (
    ~frm:frmSubsData,
    ~result:option<expr>,
    ~parenCnt:parenCnt,
    ~consumer:subs=>contunieInstruction,
):contunieInstruction => {
    switch result {
        | None => consumer(frm.subs)
        | Some(expr) => {
            iterateSubstitutions(
                ~frmExpr = frm.frame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                ~constParts = frm.constParts[frm.numOfHypsE], 
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt,
                ~consumer
            )
        }
    }
}

let applyAssertions = (
    ~maxVar:int,
    ~frms:array<frmSubsData>,
    ~isDisjInCtx:(int,int)=>bool,
    ~statements:array<expr>,
    ~exactOrderOfStmts:bool=false,
    ~allowEmptyArgs:bool=true,
    ~allowNewVars:bool=true,
    ~result:option<expr>=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frame=>bool=_=>true,
    ~allowNewDisjForExistingVars:bool=false,
    ~onMatchFound:applyAssertionResult=>contunieInstruction,
    ~debugLevel:int=0,
    ~onProgress:option<float=>unit>=?,
    ()
):unit => {
    let sendNoUnifForAsrt = (frm:frmSubsData):contunieInstruction => {
        switch result {
            | None => Continue
            | Some(expr) => {
                onMatchFound(
                    {
                        newVars: [],
                        newVarTypes: [],
                        frame: frm.frame,
                        subs: subsClone(frm.subs),
                        err:Some(NoUnifForAsrt({asrtExpr:frm.frame.asrt, expr}))
                    }
                )
            }
        }
    }

    let numOfStmts = statements->Js_array2.length
    let numOfFrames = frms->Js_array2.length->Belt_Int.toFloat
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?, ())
    let framesProcessed = ref(0.)
    let continueInstr = ref(Continue)
    let sentValidResults = Belt_HashSet.make(~hintSize=16, ~id=module(ApplyAssertionResultHash))
    frms->Js_array2.forEach(frm => {
        if ( continueInstr.contents == Continue && frameFilter(frm.frame) ) {
            if (result->Belt.Option.map(result => result[0] != frm.frame.asrt[0])->Belt_Option.getWithDefault(false)) {
                if (debugLevel >= 2) {
                    continueInstr.contents = sendNoUnifForAsrt(frm)
                }
            } else {
                let subsForResFound = ref(false)
                continueInstr.contents = iterateSubstitutionsForResult(
                    ~frm,
                    ~result,
                    ~parenCnt,
                    ~consumer = _ => {
                        subsForResFound.contents = true
                        let numOfHyps = frm.numOfHypsE
                        let workVars = {
                            maxVar,
                            newVars: [],
                            newVarTypes: [],
                        }
                        iterateCombinations(
                            ~numOfStmts,
                            ~numOfHyps,
                            ~stmtCanMatchHyp = (s,h) => {
                                if (s == -1) {
                                    !exactOrderOfStmts && allowEmptyArgs
                                } else {
                                    (!exactOrderOfStmts || s == h) && stmtCanMatchHyp(
                                        ~frm,
                                        ~hypIdx=h,
                                        ~stmt = statements[s],
                                        ~hyp = frm.hypsE[h].expr,
                                        ~parenCnt,
                                    )
                                }
                            },
                            ~debugLevel,
                            ~errConsumer = err => {
                                onMatchFound(
                                    {
                                        newVars: [],
                                        newVarTypes: [],
                                        frame: frm.frame,
                                        subs: subsClone(frm.subs),
                                        err: Some(err)
                                    }
                                )
                            },
                            ~combinationConsumer = comb => {
                                iterateSubstitutionsForHyps(
                                    ~workVars,
                                    ~frm,
                                    ~parenCnt,
                                    ~statements,
                                    ~allowNewVars,
                                    ~comb,
                                    ~hypIdx=getNextHypIdxToMatch(-1, comb),
                                    ~debugLevel,
                                    ~onErrFound = err => {
                                        onMatchFound(
                                            {
                                                newVars: [],
                                                newVarTypes: [],
                                                frame: frm.frame,
                                                subs: subsClone(frm.subs),
                                                err: Some(err)
                                            }
                                        )
                                    },
                                    ~onMatchFound = () => {
                                        switch checkDisj(
                                            ~isDisjInCtx,
                                            ~frmDisj=frm.frame.disj, 
                                            ~subs=frm.subs,
                                            ~maxCtxVar=maxVar,
                                            ~allowNewDisjForExistingVars,
                                            ~debugLevel,
                                        ) {
                                            | None => {
                                                let res = {
                                                    newVars: workVars.newVars->Js.Array2.copy,
                                                    newVarTypes: workVars.newVarTypes->Js.Array2.copy,
                                                    frame: frm.frame,
                                                    subs: subsClone(frm.subs),
                                                    err:None
                                                }
                                                if (!(sentValidResults->Belt_HashSet.has(res))) {
                                                    sentValidResults->Belt_HashSet.add(res)
                                                    onMatchFound(res)
                                                } else {
                                                    Continue
                                                }
                                            }
                                            | Some(err) => {
                                                if (debugLevel == 0) {
                                                    Continue
                                                } else {
                                                    let res = {
                                                        newVars: workVars.newVars->Js.Array2.copy,
                                                        newVarTypes: workVars.newVarTypes->Js.Array2.copy,
                                                        frame: frm.frame,
                                                        subs: subsClone(frm.subs),
                                                        err:Some(err)
                                                    }
                                                    onMatchFound(res)
                                                }
                                            }
                                        }
                                    }
                                )
                            },
                        )
                    }
                )
                if (debugLevel >= 2 && !subsForResFound.contents) {
                    continueInstr.contents = sendNoUnifForAsrt(frm)
                }
            }
        }
        framesProcessed.contents = framesProcessed.contents +. 1.
        progressState->progressTrackerSetCurrPct(
            framesProcessed.contents /. numOfFrames
        )
    })
}