open MM_context
open Expln_utils_promise
open MM_wrk_ctx_proc
open MM_statements_dto
open MM_progress_tracker
open MM_wrk_settings
open MM_wrk_pattern_search

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({
        isAxiom:option<bool>,
        typ:option<int>, 
        label:string, 
        pattern:string,
        patternVersion:int,
        ctx:mmContext,
        isDisc:option<bool>,
        isDepr:option<bool>,
        isTranDepr:option<bool>,
    })

type response =
    | OnProgress(float)
    | SearchResult(array<(string,option<matchedIndices>)>)

let reqToStr = req => {
    switch req {
        | FindAssertions({label, pattern}) => 
            `FindAssertions(label="${label}", pattern=${pattern})`
    }
}

let respToStr = resp => {
    switch resp {
        | OnProgress(pct) => `OnProgress(pct=${pct->Belt_Float.toString})`
        | SearchResult(_) => `SearchResult`
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
    ~pattern:string,
    ~patternVersion:int,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~onProgress:float=>unit,
): promise<array<(string,option<matchedIndices>)>> => {
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
                isAxiom, typ, label, pattern, patternVersion, ctx:preCtx, isDisc, isDepr, isTranDepr
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
    ~isAxiom:option<bool>,
    ~typ:option<int>, 
    ~label:string, 
    ~pattern:string,
    ~patternVersion:int,
    ~ctx:mmContext,
    ~isDisc:option<bool>,
    ~isDepr:option<bool>,
    ~isTranDepr:option<bool>,
    ~onProgress:option<float=>unit>=?
):array<(frame,option<matchedIndices>)> => {
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?)
    let framesProcessed = ref(0.)
    let numOfFrames = allFramesInDeclarationOrder->Array.length->Belt_Int.toFloat
    let pattern = switch parsePattern(~patternStr=pattern, ~patternVersion, ~ctx) {
        | Error(msg) => Exn.raiseError(`Cannot parse the search pattern in doSearchAssertions: ${msg}`)
        | Ok(pattern) => pattern
    }

    let labelTrim = label->String.trim->String.toLowerCase
    allFramesInDeclarationOrder->Array.map(frame => {
        switch onProgress {
            | None => ()
            | Some(_) => {
                framesProcessed.contents = framesProcessed.contents +. 1.
                progressState->progressTrackerSetCurrPct(
                    framesProcessed.contents /. numOfFrames
                )
            }
        }
        let matched = isAxiom->Option.mapOr( true, isAxiom => isAxiom == frame.isAxiom ) 
            && typ->Option.mapOr( true, typ => typ == frame.asrt->Array.getUnsafe(0) )
            && frame.label->String.toLowerCase->String.includes(labelTrim)
            && (threeStateBoolMatchesTwoStateBool(isDisc, frame.isDisc))
            && (threeStateBoolMatchesTwoStateBool(isDepr, frame.isDepr))
            && (threeStateBoolMatchesTwoStateBool(isTranDepr, frame.isTranDepr))
        if (matched) {
            switch frameMatchesPattern(frame, pattern) {
                | NotMatched => None
                | Matched(idxs) => Some((frame,idxs))
            }
        } else {
            None
        }
    })->Array.filter(res => res->Option.isSome)
        ->Array.map(Option.getExn(_))
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({isAxiom, typ, label, pattern, patternVersion, ctx, isDisc, isDepr, isTranDepr}) => {
            let filteredFrames = doSearchAssertions(
                ~allFramesInDeclarationOrder=getAllFramesInDeclarationOrderExn(),
                ~isAxiom,
                ~typ,
                ~label,
                ~pattern,
                ~patternVersion,
                ~ctx,
                ~isDisc,
                ~isDepr,
                ~isTranDepr,
                ~onProgress = pct => sendToClient(OnProgress(pct))
            )
            sendToClient(SearchResult(filteredFrames->Array.map(((frame,idxs)) => (frame.label,idxs))))
        }
    }
}