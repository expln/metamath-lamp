open MM_substitution
open MM_context
open MM_parenCounter
open MM_progress_tracker
open MM_unification_debug

type applyAssertionResult = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    asrtLabel: string,
    subs: subs,
    err:option<unifErr>,
}

let applyAssertionResultEq = (a:applyAssertionResult, b:applyAssertionResult):bool => {
    a.err->Belt_Option.isNone && b.err->Belt_Option.isNone && a.asrtLabel == b.asrtLabel && subsEq(a.subs, b.subs)
}

let applyAssertionResultHash = (a:applyAssertionResult):int => {
    Expln_utils_common.hash2(
        Expln_utils_common.hashStr(a.asrtLabel),
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
    ~combinationConsumer:array<int>=>contunieInstruction,
):contunieInstruction => {
    if (hypIdx == comb->Js.Array2.length) {
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
    } else {
        let res = ref(Continue)
        let c = ref(0)
        let maxC = candidatesPerHyp[hypIdx]->Js.Array2.length-1
        while (res.contents == Continue && c.contents <= maxC) {
            comb[hypIdx] = candidatesPerHyp[hypIdx][c.contents]
            if (!(comb[hypIdx] == -1 && skipCombinationsWithEmptyArgs)) {
                res.contents = iterateCombinationsRec(
                    ~candidatesPerHyp,
                    ~comb,
                    ~hypIdx = hypIdx+1,
                    ~skipCombinationsWithEmptyArgs,
                    ~skipCombinationsWithoutEmptyArgs,
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
    ~combinationConsumer:array<int>=>contunieInstruction,
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
    let thereIsHypWithoutAnyCandidate = candidatesPerHyp->Js_array2.some(candidates => candidates->Js_array2.length == 0)
    if (thereIsHypWithoutAnyCandidate) {
        Continue
    } else {
        let comb = Belt_Array.make(numOfHyps, 0)
        let continue = iterateCombinationsRec(
            ~candidatesPerHyp,
            ~comb,
            ~hypIdx = 0,
            ~skipCombinationsWithEmptyArgs=true,
            ~skipCombinationsWithoutEmptyArgs=false,
            ~combinationConsumer
        )
        if (continue == Continue) {
            iterateCombinationsRec(
                ~candidatesPerHyp,
                ~comb,
                ~hypIdx = 0,
                ~skipCombinationsWithEmptyArgs=false,
                ~skipCombinationsWithoutEmptyArgs=true,
                ~combinationConsumer
            )
        } else {
            continue
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

let rec iterateSubstitutionsForHyps = (
    ~workVars:workVars,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
    ~statements:array<expr>,
    ~allowNewVars:bool,
    ~comb:array<int>,
    ~hypIdx:int,
    ~debugLevel:int,
    ~onMatchFound: result<unit,unifErr> => contunieInstruction
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
                onMatchFound(Ok(()))
            }
        )
        if (debugLevel > 1 && !subsFound.contents) {
            onMatchFound(Error(NewVarsAreDisabled({args:combToArgs(), errArgIdx:hypIdx})))
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
                    ~hypIdx = hypIdx+1,
                    ~debugLevel,
                    ~onMatchFound
                )
            }
        )
        if (debugLevel > 1 && !subsFound.contents) {
            onMatchFound(Error(NoUnifForArg({args:combToArgs(), errArgIdx:hypIdx})))
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
                    ~hypIdx = hypIdx+1,
                    ~debugLevel,
                    ~onMatchFound
                )
            }
        )
        if (debugLevel > 1 && !subsFound.contents) {
            onMatchFound(Error(NewVarsAreDisabled({args:combToArgs(), errArgIdx:hypIdx})))
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
):result<disjMutable,unifErr> => {
    let resultDisj = disjMake()
    let verifRes = verifyDisjoints(~frmDisj, ~subs, ~debugLevel, ~isDisjInCtx = (n,m) => {
        if (n <= maxCtxVar && m <= maxCtxVar) {
            if (isDisjInCtx(n,m)) {
                true
            } else if (allowNewDisjForExistingVars) {
                resultDisj->disjAddPair(n,m)
                true
            } else {
                false
            }
        } else {
            resultDisj->disjAddPair(n,m)
            true
        }
    })
    switch verifRes {
        | None => Ok(resultDisj)
        | Some(err) => Error(err)
    }
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
    ~frms:Belt_MapString.t<frmSubsData>,
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
    let sendNoUnifForAsrt = (frm):contunieInstruction => {
        switch result {
            | None => Continue
            | Some(expr) => {
                onMatchFound(
                    {
                        newVars: [],
                        newVarTypes: [],
                        newDisj: disjMake(),
                        asrtLabel: frm.frame.label,
                        subs: subsClone(frm.subs),
                        err:Some(NoUnifForAsrt({asrtExpr:frm.frame.asrt, expr}))
                    }
                )
            }
        }
    }

    let numOfStmts = statements->Js_array2.length
    let numOfFrames = frms->Belt_MapString.size->Belt_Int.toFloat
    let progressState = progressTrackerMutableMake(~step=0.01, ~onProgress?, ())
    let framesProcessed = ref(0.)
    let continueInstr = ref(Continue)
    let sentValidResults = Belt_HashSet.make(~hintSize=16, ~id=module(ApplyAssertionResultHash))
    frms->Belt_MapString.forEach((_,frm) => {
        if ( continueInstr.contents == Continue && frameFilter(frm.frame) ) {
            if (result->Belt.Option.map(result => result[0] != frm.frame.asrt[0])->Belt_Option.getWithDefault(false)) {
                if (debugLevel > 1) {
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
                            ~combinationConsumer = comb => {
                                iterateSubstitutionsForHyps(
                                    ~workVars,
                                    ~frm,
                                    ~parenCnt,
                                    ~statements,
                                    ~allowNewVars,
                                    ~comb,
                                    ~hypIdx=0,
                                    ~debugLevel,
                                    ~onMatchFound = res => {
                                        switch res {
                                            | Error(err) => {
                                                onMatchFound(
                                                    {
                                                        newVars: [],
                                                        newVarTypes: [],
                                                        newDisj: disjMake(),
                                                        asrtLabel: frm.frame.label,
                                                        subs: subsClone(frm.subs),
                                                        err: Some(err)
                                                    }
                                                )
                                            }
                                            | Ok(_) => {
                                                switch checkDisj(
                                                    ~isDisjInCtx,
                                                    ~frmDisj=frm.frame.disj, 
                                                    ~subs=frm.subs,
                                                    ~maxCtxVar=maxVar,
                                                    ~allowNewDisjForExistingVars,
                                                    ~debugLevel,
                                                ) {
                                                    | Ok(newDisj) => {
                                                        let res = {
                                                            newVars: workVars.newVars->Js.Array2.copy,
                                                            newVarTypes: workVars.newVarTypes->Js.Array2.copy,
                                                            newDisj,
                                                            asrtLabel: frm.frame.label,
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
                                                    | Error(err) => {
                                                        if (debugLevel == 0) {
                                                            Continue
                                                        } else {
                                                            let res = {
                                                                newVars: workVars.newVars->Js.Array2.copy,
                                                                newVarTypes: workVars.newVarTypes->Js.Array2.copy,
                                                                newDisj: disjMake(),
                                                                asrtLabel: frm.frame.label,
                                                                subs: subsClone(frm.subs),
                                                                err:Some(err)
                                                            }
                                                            onMatchFound(res)
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                )
                            },
                        )
                    }
                )
                if (debugLevel > 1 && !subsForResFound.contents) {
                    continueInstr.contents = sendNoUnifForAsrt(frm)
                }
            }
        }
        framesProcessed.contents = framesProcessed.contents +. 1.
        progressState->progressTrackerMutableSetCurrPct(
            framesProcessed.contents /. numOfFrames
        )
    })
}