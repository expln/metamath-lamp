open MM_context
open Expln_utils_promise
open MM_wrk_ctx_proc
open MM_substitution
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
    | FindAssertions({label:string, typ:int, searchPattern:array<stmtPattern>})

type response =
    | OnProgress(float)
    | SearchResult({found:array<stmtsDto>})

let reqToStr = req => {
    switch req {
        | FindAssertions({label, typ, searchPattern}) => 
            `FindAssertions(label="${label}", typ=${typ->Belt_Int.toString}, `
                ++ `pattern=${stringify(searchPattern)})`
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

let frmExprMatchesConstPattern = (
    ~frmExpr:expr, 
    ~constPat:array<int>, 
    ~varTypes:array<int>, 
    ~flags:array<patternModifier>,
):bool => {
    let patLen = constPat->Array.length
    let asrtLen = frmExpr->Array.length
    if (flags->Array.includes(Exact)) {
        frmExpr == constPat
    } else if (flags->Array.includes(Adj)) {
        let found = ref(false)
        let i = ref(0)
        let maxI = asrtLen - patLen
        while (!found.contents && i.contents <= maxI) {
            found := compareSubArrays(~src=frmExpr, ~srcFromIdx=i.contents, ~dst=constPat, ~dstFromIdx=0, ~len=patLen)
            i.contents = i.contents + 1
        }
        found.contents
    } else {
        let pIdx = ref(0)
        let aIdx = ref(0)
        while (pIdx.contents < patLen && aIdx.contents < asrtLen) {
            let asrtSym = frmExpr->Array.getUnsafe(aIdx.contents)
            let patSym = constPat->Array.getUnsafe(pIdx.contents)
            if (
                asrtSym < 0 && asrtSym == patSym
                || asrtSym >= 0 && varTypes->Array.getUnsafe(asrtSym) == patSym
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
    let maxPIdx = varPat->Array.length
    let eIdx = ref(fromIdx)
    let maxEIdx = frmExpr->Array.length
    let match = ref(true)
    while (match.contents && pIdx.contents <= maxPIdx && eIdx.contents <= maxEIdx) {
        let asrtSym = frmExpr->Array.getUnsafe(eIdx.contents)
        let varPatSym = varPat->Array.getUnsafe(pIdx.contents)
        if ( asrtSym < 0 ) {
            match := asrtSym == varPatSym
        } else {
            match := varTypes->Array.getUnsafe(asrtSym) == constPat->Array.getUnsafe(pIdx.contents)
            if (match.contents && varPatSym >= 0) {
                switch mapping->Belt_HashMapInt.get(varPatSym) {
                    | None => mapping->Belt_HashMapInt.set(varPatSym, asrtSym)
                    | Some(asrtVar) => match := asrtVar == asrtSym
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
            let asrtSym = frmExpr->Array.getUnsafe(aIdx.contents)
            let varPatSym = varPat->Array.getUnsafe(pIdx)
            if ( asrtSym < 0 && asrtSym == varPatSym ) {
                found := remainingMatches()
            } else if ( asrtSym >= 0 && varTypes->Array.getUnsafe(asrtSym) == constPat->Array.getUnsafe(pIdx) ) {
                if ( varPatSym < 0 ) {
                    found := remainingMatches()
                } else {
                    switch mapping->Belt_HashMapInt.get(varPatSym) {
                        | None => {
                            mapping->Belt_HashMapInt.set(varPatSym, asrtSym)
                            found := remainingMatches()
                            mapping->Belt_HashMapInt.remove(varPatSym)
                        }
                        | Some(asrtVar) => {
                            if (asrtVar == asrtSym) {
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
    let asrtLen = frmExpr->Array.length
    if (flags->Array.includes(Exact)) {
        patLen == asrtLen && frmExprMatchesVarPatternAdj(~frmExpr, ~varPat, ~constPat, ~varTypes, ~mapping, ~fromIdx=0)
    } else if (flags->Array.includes(Adj)) {
        let found = ref(false)
        let i = ref(0)
        let maxI = asrtLen - patLen
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
    ~varsText: string,
    ~disjText: string,
    ~label:string,
    ~typ:int, 
    ~searchPattern:array<stmtPattern>,
    ~onProgress:float=>unit,
): promise<array<stmtsDto>> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~settingsVer,
            ~settings,
            ~preCtxVer,
            ~preCtx,
            ~varsText,
            ~disjText,
            ~procName,
            ~initialRequest = FindAssertions({label:label->String.toLowerCase, typ, searchPattern}),
            ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | SearchResult({found}) => {
                        endWorkerInteraction()
                        resolve(found)
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

//todo: review this function
let doSearchAssertions = (
    ~wrkCtx:mmContext,
    ~frms:frms,
    ~label:string, 
    ~typ:int, 
    ~searchPattern:array<stmtPattern>,
    ~onProgress:option<float=>unit>=?
):array<stmtsDto> => {
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?)
    let framesProcessed = ref(0.)
    let numOfFrames = frms->frmsSize->Belt_Int.toFloat
    let mapping = Belt_HashMapInt.make(~hintSize=10)

    let results = []
    let framesInDeclarationOrder = frms->frmsSelect
        ->Expln_utils_common.sortInPlaceWith((a,b) => Belt_Float.fromInt(a.frame.ord - b.frame.ord))
    framesInDeclarationOrder->Array.forEach(frm => {
        let frame = frm.frame
        if (
            frame.label->String.toLowerCase->String.includes(label)
            && frame.asrt->Array.getUnsafe(0) == typ 
            && frameMatchesPattern(~frame, ~searchPattern, ~mapping)
        ) {
            results->Array.push(frameToStmtsDto( ~wrkCtx, ~frame, ))
        }

        framesProcessed.contents = framesProcessed.contents +. 1.
        progressState->progressTrackerSetCurrPct(
            framesProcessed.contents /. numOfFrames
        )
    })
    results
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({label, typ, searchPattern}) => {
            let results = doSearchAssertions(
                ~wrkCtx=getWrkCtxExn(), 
                ~frms = getWrkFrmsExn(),
                ~label, 
                ~typ, 
                ~searchPattern, 
                ~onProgress = pct => sendToClient(OnProgress(pct))
            )
            sendToClient(SearchResult({found:results}))
        }
    }
}