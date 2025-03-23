open MM_context
open Expln_utils_promise
open MM_wrk_ctx_proc
open MM_statements_dto
open MM_progress_tracker
open MM_wrk_settings
open Expln_utils_common

type patternModifier = Hyp | Asrt | Exact | Adj
type stmtPattern = {
    flags: array<patternModifier>,
    varPat: array<int>,
    constPat: array<int>,
}

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({
        isAxiom:option<bool>,
        typ:option<int>, 
        label:string, 
        searchPattern:array<stmtPattern>,
        isDisc:option<bool>,
        isDepr:option<bool>,
        isTranDepr:option<bool>,
        dependsOn:array<string>,
        dependsOnTran:bool,
    })

type response =
    | OnProgress(float)
    | SearchResult(array<string>)

let reqToStr = req => {
    switch req {
        | FindAssertions({label, searchPattern}) => 
            `FindAssertions(label="${label}", pattern=${stringify(searchPattern)})`
    }
}

let respToStr = resp => {
    switch resp {
        | OnProgress(pct) => `OnProgress(pct=${pct->Belt_Float.toString})`
        | SearchResult(_) => `SearchResult`
    }
}

let strToPatternModifier = (str:string):result<patternModifier, string> => {
    switch str {
        | "h" => Ok(Hyp)
        | "a" => Ok(Asrt)
        | "!" => Ok(Exact)
        | "+" => Ok(Adj)
        | _ => Error(`Cannot convert '${str}' to patternModifier`)
    }
}

let parseSearchStr = (searchStr:string): result<array<(array<patternModifier>, array<string>)>,string> => {
    let result = []
    let inFlags = ref(false)
    let flags:array<patternModifier> = []
    let syms = []
    let sym = []
    let pos = ref(0)
    let maxPos = searchStr->String.length-1
    let err = ref(None)

    let addPos = (msg:string) => {
        msg ++ ` at position ${(pos.contents+1)->Belt_Int.toString}.`
    }

    let setError = (msg:string) => {
        err := Some(addPos(msg))
    }

    let clearArray: 'a. array<'a> => unit = arr => {
        arr->Array.splice(~start=0, ~remove=arr->Array.length, ~insert=[])
    }

    let saveLastReadSymbol = () => {
        if (Array.length(sym) > 0) {
            syms->Array.push(sym->Array.join(""))
            clearArray(sym)
        }
    }

    let commitStmtPattern = () => {
        saveLastReadSymbol()
        if (flags->Array.length > 0 && syms->Array.length == 0) {
            setError("At least one symbol is expected")
        } else if (Array.length(syms) > 0) {
            result->Array.push((flags->Array.copy, syms->Array.copy))
            clearArray(flags)
            clearArray(syms)
        }
    }

    while (pos.contents <= maxPos && err.contents->Option.isNone) {
        let ch = searchStr->String.charAt(pos.contents)
        if (ch->String.trim == "") {
            if (inFlags.contents) {
                inFlags := false
            } else {
                saveLastReadSymbol()
            }
        } else if (inFlags.contents) {
            switch strToPatternModifier(ch) {
                | Error(msg) => setError(msg)
                | Ok(flag) => {
                    flags->Array.push(flag)
                    if (flags->Array.includes(Hyp) && flags->Array.includes(Asrt)) {
                        setError("A sub-pattern cannot be both a hypothesis and an assertion")
                    } else if (flags->Array.includes(Adj) && flags->Array.includes(Exact)) {
                        setError("A sub-pattern cannot be both Adjacent and Exact")
                    }
                }
            }
        } else if (ch == "$") {
            commitStmtPattern()
            inFlags := true
        } else {
            sym->Array.push(ch)
        }
        pos.contents = pos.contents + 1
    }

    switch err.contents {
        | Some(msg) => Error(msg)
        | None => {
            commitStmtPattern()
            switch err.contents {
                | Some(msg) => Error(msg)
                | None => {
                    if (
                        result->Array.length > 1
                        && result->Array.some(
                            ((flags, _)) => !(flags->Array.includes(Hyp) || flags->Array.includes(Asrt))
                        )
                    ) {
                        Error("Each sub-pattern must be either a hypothesis or an assertion.")
                    } else {
                        Ok(result)
                    }
                }
            }
        }
    }
    
}

let makeSearchPattern = (~searchStr:string, ~ctx:mmContext): result<array<stmtPattern>,string> => {
    switch parseSearchStr(searchStr) {
        | Error(msg) => Error(msg)
        | Ok(parsed) => {
            parsed->Array.reduce(Ok([]), (res, (flags, syms)) => {
                switch res {
                    | Error(_) => res
                    | Ok(resArr) => {
                        let incorrectSymbol = syms->Array.find(sym => {
                            ctx->ctxSymToInt(sym)->Option.isNone
                        })
                        switch incorrectSymbol {
                            | Some(sym) => Error(`'${sym}' - is not a constant or a variable.`)
                            | None => {
                                let varPat = ctx->ctxSymsToIntsExn(syms)
                                let constPat = varPat->Array.map(sym => sym < 0 ? sym : ctx->getTypeOfVarExn(sym))
                                resArr->Array.push({ flags, varPat, constPat, })
                                Ok(resArr)
                            }
                        }
                    }
                }
            })
        }
    }
}

let frmExprMatchesConstPatternAdj = (
    ~frmExpr:expr, 
    ~constPat:array<int>,
    ~varTypes:array<int>,
    ~fromIdx:int
):bool => {
    let pIdx = ref(0)
    let maxPIdx = constPat->Array.length-1
    let eIdx = ref(fromIdx)
    let maxEIdx = frmExpr->Array.length-1
    let match = ref(true)
    while (match.contents && pIdx.contents <= maxPIdx && eIdx.contents <= maxEIdx) {
        let frmSym = frmExpr->Array.getUnsafe(eIdx.contents)
        let constPatSym = constPat->Array.getUnsafe(pIdx.contents)
        match := frmSym < 0 && frmSym == constPatSym
            || frmSym >= 0 && varTypes->Array.getUnsafe(frmSym) == constPatSym
        pIdx := pIdx.contents + 1
        eIdx := eIdx.contents + 1
    }
    match.contents && pIdx.contents > maxPIdx
}

let frmExprMatchesConstPattern = (
    ~frmExpr:expr, 
    ~constPat:array<int>, 
    ~varTypes:array<int>, 
    ~flags:array<patternModifier>,
):bool => {
    let patLen = constPat->Array.length
    let frmExprLen = frmExpr->Array.length
    if (flags->Array.includes(Exact)) {
        patLen == frmExprLen && frmExprMatchesConstPatternAdj( ~frmExpr, ~constPat, ~varTypes, ~fromIdx=0 )
    } else if (flags->Array.includes(Adj)) {
        let found = ref(false)
        let i = ref(0)
        let maxI = frmExprLen - patLen
        while (!found.contents && i.contents <= maxI) {
            found := frmExprMatchesConstPatternAdj( ~frmExpr, ~constPat, ~varTypes, ~fromIdx=i.contents )
            i.contents = i.contents + 1
        }
        found.contents
    } else {
        let pIdx = ref(0)
        let aIdx = ref(0)
        while (pIdx.contents < patLen && aIdx.contents < frmExprLen) {
            let frmSym = frmExpr->Array.getUnsafe(aIdx.contents)
            let patSym = constPat->Array.getUnsafe(pIdx.contents)
            if (
                frmSym < 0 && frmSym == patSym
                || frmSym >= 0 && varTypes->Array.getUnsafe(frmSym) == patSym
            ) {
                pIdx.contents = pIdx.contents + 1
            }
            aIdx.contents = aIdx.contents + 1
        }
        pIdx.contents == patLen
    }
}

let frmExprMatchesVarPatternAdj = (
    ~frmExpr:expr, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~varTypes:array<int>,
    ~mapping:Belt_HashMapInt.t<int>,
    ~fromIdx:int
):bool => {
    let pIdx = ref(0)
    let maxPIdx = varPat->Array.length-1
    let eIdx = ref(fromIdx)
    let maxEIdx = frmExpr->Array.length-1
    let match = ref(true)
    while (match.contents && pIdx.contents <= maxPIdx && eIdx.contents <= maxEIdx) {
        let frmSym = frmExpr->Array.getUnsafe(eIdx.contents)
        let varPatSym = varPat->Array.getUnsafe(pIdx.contents)
        if ( frmSym < 0 ) {
            match := frmSym == varPatSym
        } else {
            match := varTypes->Array.getUnsafe(frmSym) == constPat->Array.getUnsafe(pIdx.contents)
            if (match.contents && varPatSym >= 0) {
                switch mapping->Belt_HashMapInt.get(varPatSym) {
                    | None => mapping->Belt_HashMapInt.set(varPatSym, frmSym)
                    | Some(savedFrmSym) => match := savedFrmSym == frmSym
                }
            }
        }

        pIdx := pIdx.contents + 1
        eIdx := eIdx.contents + 1
    }
    mapping->Belt_HashMapInt.clear
    match.contents && pIdx.contents > maxPIdx
}

let rec frmExprMatchesVarPatternRec = (
    ~frmExpr:expr, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~varTypes:array<int>,
    ~mapping:Belt_HashMapInt.t<int>,
    ~pIdx:int,
    ~minAIdx:int,
):bool => {
    if (pIdx == varPat->Array.length) {
        true
    } else {
        let aIdx = ref(minAIdx)
        let remainingMatches = ():bool => {
            frmExprMatchesVarPatternRec(
                ~frmExpr, 
                ~varPat, 
                ~constPat,
                ~varTypes,
                ~mapping,
                ~pIdx=pIdx+1,
                ~minAIdx=aIdx.contents+1,
            )
        }

        let found = ref(false)
        let maxAIdx = frmExpr->Array.length - (varPat->Array.length - pIdx)
        while (!found.contents && aIdx.contents <= maxAIdx) {
            let frmSym = frmExpr->Array.getUnsafe(aIdx.contents)
            let varPatSym = varPat->Array.getUnsafe(pIdx)
            if ( frmSym < 0 && frmSym == varPatSym ) {
                found := remainingMatches()
            } else if ( frmSym >= 0 && varTypes->Array.getUnsafe(frmSym) == constPat->Array.getUnsafe(pIdx) ) {
                if ( varPatSym < 0 ) {
                    found := remainingMatches()
                } else {
                    switch mapping->Belt_HashMapInt.get(varPatSym) {
                        | None => {
                            mapping->Belt_HashMapInt.set(varPatSym, frmSym)
                            found := remainingMatches()
                            mapping->Belt_HashMapInt.remove(varPatSym)
                        }
                        | Some(savedFrmSym) => {
                            if (savedFrmSym == frmSym) {
                                found := remainingMatches()
                            }
                        }
                    }
                }
            }
            aIdx.contents = aIdx.contents + 1
        }
        found.contents
    }
}

let frmExprMatchesVarPattern = (
    ~frmExpr:expr, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~varTypes:array<int>,
    ~flags:array<patternModifier>,
    ~mapping:Belt_HashMapInt.t<int>,
):bool => {
    let patLen = constPat->Array.length
    let frmExprLen = frmExpr->Array.length
    if (flags->Array.includes(Exact)) {
        patLen == frmExprLen 
            && frmExprMatchesVarPatternAdj(~frmExpr, ~varPat, ~constPat, ~varTypes, ~mapping, ~fromIdx=0)
    } else if (flags->Array.includes(Adj)) {
        let found = ref(false)
        let i = ref(0)
        let maxI = frmExprLen - patLen
        while (!found.contents && i.contents <= maxI) {
            found := frmExprMatchesVarPatternAdj(~frmExpr, ~varPat, ~constPat, ~varTypes, ~mapping, ~fromIdx=i.contents)
            i := i.contents + 1
        }
        found.contents
    } else {
        frmExprMatchesVarPatternRec( ~frmExpr, ~varPat, ~constPat, ~varTypes, ~mapping, ~pIdx=0, ~minAIdx=0, )
    }
}

let frmExprMatchesPattern = (
    ~frmExpr:array<int>, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~varTypes:array<int>,
    ~flags:array<patternModifier>,
    ~mapping:Belt_HashMapInt.t<int>
):bool => {
    frmExprMatchesConstPattern(~frmExpr,~constPat,~varTypes,~flags,) 
        && frmExprMatchesVarPattern( ~frmExpr, ~varPat, ~constPat, ~varTypes, ~flags, ~mapping, )
}

let frameMatchesPattern1 = (
    ~frame:frame, 
    ~varPat:array<int>, 
    ~constPat:array<int>,
    ~flags:array<patternModifier>,
    ~mapping:Belt_HashMapInt.t<int>
):bool => {
    let varTypes = frame.varTypes
    frmExprMatchesPattern(~frmExpr=frame.asrt, ~varPat, ~constPat, ~varTypes, ~flags, ~mapping)
        || frame.hyps->Array.reduce(false, (res, hyp) => {
            res || hyp.typ == E && frmExprMatchesPattern(
                ~frmExpr=hyp.expr, ~varPat, ~constPat, ~varTypes, ~flags, ~mapping
            )
        })
}

let frameMatchesPattern2 = (
    ~frame:frame, 
    ~searchPattern:array<stmtPattern>, 
    ~mapping:Belt_HashMapInt.t<int>
):bool => {
    let rec hasNonRepeatingPath = (~foundMatches:array<array<int>>, ~patIdx:int, ~path:array<int>):bool => {
        if (patIdx == foundMatches->Array.length) {
            true
        } else {
            foundMatches->Array.getUnsafe(patIdx)->Array.some(hypIdx => {
                if (path->Array.includes(hypIdx)) {
                    false
                } else {
                    path->Array.push(hypIdx)
                    let result = hasNonRepeatingPath(~foundMatches, ~patIdx=patIdx+1, ~path)
                    path->Array.pop->ignore
                    result
                }
            })
        }
    }

    let varTypes = frame.varTypes
    let asrtPatterns = searchPattern->Array.filter(pat => pat.flags->Array.includes(Asrt))
    switch asrtPatterns->Array.find(pat => {
        !frmExprMatchesPattern(~frmExpr=frame.asrt, 
            ~varPat=pat.varPat, ~constPat=pat.constPat, ~varTypes, ~flags=pat.flags, ~mapping)
    }) {
        | Some(_) => false
        | None => {
            let hypPatterns = searchPattern->Array.filter(pat => pat.flags->Array.includes(Hyp))
            if (hypPatterns->Array.length == 0) {
                true
            } else {
                let eHyps = frame.hyps->Array.filter(hyp => hyp.typ == E)
                if (eHyps->Array.length < hypPatterns->Array.length) {
                    false
                } else {
                    let foundMatches = hypPatterns->Array.reduce(Some([]), (found, pat) => {
                        switch found {
                            | None => None
                            | Some(matches) => {
                                let matchedIdxs = eHyps->Array.mapWithIndex((hyp,i) => (hyp,i))
                                    ->Array.filter(((hyp,_)) => 
                                        frmExprMatchesPattern(~frmExpr=hyp.expr, 
                                            ~varPat=pat.varPat, ~constPat=pat.constPat, ~varTypes, ~flags=pat.flags, 
                                            ~mapping
                                        )
                                    )
                                    ->Array.map(((_,i)) => i)
                                if (matchedIdxs->Array.length == 0) {
                                    None
                                } else {
                                    matches->Array.push(matchedIdxs)
                                    Some(matches)
                                }
                            }
                        }
                    })
                    switch foundMatches {
                        | None => false
                        | Some(foundMatches) => hasNonRepeatingPath(~foundMatches, ~patIdx=0, ~path=[])
                    }
                }
            }
        }
    }
}

let frameMatchesPattern = (
    ~frame:frame, 
    ~searchPattern:array<stmtPattern>, 
    ~mapping:Belt_HashMapInt.t<int>
):bool => {
    if (
        searchPattern->Array.length == 1 
        && !(
            (searchPattern->Array.getUnsafe(0)).flags->Array.includes(Hyp) 
            || (searchPattern->Array.getUnsafe(0)).flags->Array.includes(Asrt)
        )
    ) {
        let pat = searchPattern->Array.getUnsafe(0)
        frameMatchesPattern1( ~frame, ~varPat=pat.varPat, ~constPat=pat.constPat, ~flags=pat.flags, ~mapping )
    } else {
        frameMatchesPattern2( ~frame, ~searchPattern, ~mapping )
    }
}

let searchAssertions = (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~searchPattern:array<stmtPattern>,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~dependsOn:array<string>,
    ~dependsOnTran:bool,
    ~onProgress:float=>unit,
): promise<array<string>> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~settingsVer,
            ~settings,
            ~preCtxVer,
            ~preCtx,
            ~varsText="",
            ~disjText="",
            ~procName,
            ~initialRequest = FindAssertions({ 
                isAxiom, typ, label, searchPattern, isDisc, isDepr, isTranDepr, dependsOn, dependsOnTran
            }),
            ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | SearchResult(foundLabels) => {
                        endWorkerInteraction()
                        resolve(foundLabels)
                    }
                }
            },
            ~enableTrace=false
        )
    })
}

let frameToStmtsDto = (
    ~wrkCtx:mmContext,
    ~frame:frame,
):stmtsDto => {
    let newDisj = disjMake()
    frame.disj->Belt_MapInt.forEach((n,ms) => {
        ms->Belt_SetInt.forEach(m => {
            newDisj->disjAddPair(n,m)
        })
    })
    let newDisjStr = []
    newDisj->disjForEachArr(disjArr => {
        newDisjStr->Array.push(frmIntsToStrExn(wrkCtx, frame, disjArr))
    })
    let stmts = []
    let argLabels = []
    frame.hyps->Array.forEach(hyp => {
        if (hyp.typ == E) {
            let argLabel = hyp.label
            argLabels->Array.push(argLabel)
            stmts->Array.push(
                {
                    label: argLabel,
                    expr:hyp.expr,
                    exprStr:frmIntsToStrExn(wrkCtx, frame, hyp.expr),
                    jstf:None,
                    isProved: false,
                }
            )
        }
    })
    stmts->Array.push(
        {
            label: frame.label,
            expr:frame.asrt,
            exprStr:frmIntsToStrExn(wrkCtx, frame, frame.asrt),
            jstf:Some({args:argLabels,label:frame.label}),
            isProved: false,
        }
    )
    {
        newVars: Belt_Array.range(0, frame.numOfVars-1),
        newVarTypes: frame.varTypes,
        newDisj,
        newDisjStr,
        stmts,
    }
}

let threeStateBoolMatchesTwoStateBool = (threeStateBool:option<bool>, twoStateBool:bool):bool => {
    switch threeStateBool {
        | None => true
        | Some(trueOrFalse) => trueOrFalse == twoStateBool
    }
}

let doSearchAssertions = (
    ~allFramesInDeclarationOrder:array<frame>,
    ~frameDependencies:Belt_HashMapString.t<Belt_HashSetString.t>,
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~searchPattern:array<stmtPattern>,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~dependsOn:array<string>,
    ~dependsOnTran:bool,
    ~onProgress:option<float=>unit>=?
):array<frame> => {
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?)
    let framesProcessed = ref(0.)
    let numOfFrames = allFramesInDeclarationOrder->Array.length->Belt_Int.toFloat
    let mapping = Belt_HashMapInt.make(~hintSize=10)

    let labelTrim = label->String.trim->String.toLowerCase
    allFramesInDeclarationOrder->Array.filter(frame => {
        switch onProgress {
            | None => ()
            | Some(_) => {
                framesProcessed.contents = framesProcessed.contents +. 1.
                progressState->progressTrackerSetCurrPct(
                    framesProcessed.contents /. numOfFrames
                )
            }
        }
        isAxiom->Option.mapOr( true, isAxiom => isAxiom == frame.isAxiom ) 
            && typ->Option.mapOr( true, typ => typ == frame.asrt->Array.getUnsafe(0) )
            && frame.label->String.toLowerCase->String.includes(labelTrim)
            && (threeStateBoolMatchesTwoStateBool(isDisc, frame.isDisc))
            && (threeStateBoolMatchesTwoStateBool(isDepr, frame.isDepr))
            && (threeStateBoolMatchesTwoStateBool(isTranDepr, frame.isTranDepr))
            && frameMatchesPattern(~frame, ~searchPattern, ~mapping)
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({
            isAxiom, typ, label, searchPattern, isDisc, isDepr, isTranDepr, dependsOn, dependsOnTran
        }) => {
            let filteredFrames = doSearchAssertions(
                ~allFramesInDeclarationOrder=getAllFramesInDeclarationOrderExn(),
                ~frameDependencies=getFrameDependenciesExn(),
                ~isAxiom,
                ~typ,
                ~label,
                ~searchPattern,
                ~isDisc,
                ~isDepr,
                ~isTranDepr,
                ~dependsOn,
                ~dependsOnTran,
                ~onProgress = pct => sendToClient(OnProgress(pct))
            )
            sendToClient(SearchResult(filteredFrames->Array.map(frame => frame.label)))
        }
    }
}