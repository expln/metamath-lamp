open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx
open MM_substitution
open MM_parenCounter
open MM_statements_dto
open MM_progress_tracker

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({label:string, typ:int, pattern:array<int>})

type response =
    | OnProgress(float)
    | SearchResult({found:array<newStmtsDto>})

let rec frameMatchesPattern = (frm:frame, pat:array<int>):bool => {
    let patLen = pat->Js.Array2.length
    let asrtLen = frm.asrt->Js.Array2.length
    let pIdx = ref(0)
    let aIdx = ref(0)
    while (pIdx.contents < patLen && aIdx.contents < asrtLen) {
        if (
            frm.asrt[aIdx.contents] < 0 && frm.asrt[aIdx.contents] == pat[pIdx.contents]
            || frm.asrt[aIdx.contents] >= 0 && frm.varTypes[frm.asrt[aIdx.contents]] == pat[pIdx.contents]
        ) {
            pIdx.contents = pIdx.contents + 1
        }
        aIdx.contents = aIdx.contents + 1
    }
    pIdx.contents == patLen
}

let searchAssertions = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~label:string,
    ~typ:int, 
    ~pattern:array<int>,
    ~onProgress:float=>unit,
): promise<array<newStmtsDto>> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~preCtxVer,
            ~preCtx,
            ~parenStr,
            ~varsText,
            ~disjText,
            ~hyps,
            ~procName,
            ~initialRequest = FindAssertions({label:label->Js.String2.toLowerCase, typ, pattern}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | SearchResult({found}) => {
                        endWorkerInteraction()
                        resolve(found)
                    }
                }
            },
            ~enableTrace=false,
            ()
        )
    })
}

let doSearchAssertions = (
    ~wrkCtx:mmContext,
    ~frms:Belt_MapString.t<frmSubsData>,
    ~label:string, 
    ~typ:int, 
    ~pattern:array<int>, 
    ~onProgress:option<float=>unit>=?,
    ()
) => {
    let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))
    let framesProcessed = ref(0.)
    let numOfFrames = frms->Belt_MapString.size->Belt_Int.toFloat

    let results = []
    frms->Belt_MapString.forEach((_,frm) => {
        let frame = frm.frame
        if (
            frame.label->Js.String2.toLowerCase->Js_string2.includes(label)
            && frame.asrt[0] == typ 
            && frameMatchesPattern(frame, pattern)
        ) {
            let newDisj = disjMutableMake()
            frame.disj->Belt_MapInt.forEach((n,ms) => {
                ms->Belt_SetInt.forEach(m => {
                    newDisj->disjAddPair(n,m)
                })
            })
            let newDisjStr = []
            newDisj->disjForEachArr(disjArr => {
                newDisjStr->Js.Array2.push(frmIntsToStrExn(wrkCtx, frame, disjArr))->ignore
            })
            let stmts = []
            let argLabels = []
            frame.hyps->Js_array2.forEachi((hyp, i) => {
                if (hyp.typ == E) {
                    let argLabel = hyp.label
                    argLabels->Js_array2.push(argLabel)->ignore
                    stmts->Js_array2.push(
                        {
                            label: argLabel,
                            expr:hyp.expr,
                            exprStr:frmIntsToStrExn(wrkCtx, frame, hyp.expr),
                            jstf:None,
                            isProved: false,
                        }
                    )->ignore
                }
            })
            stmts->Js_array2.push(
                {
                    label: frame.label,
                    expr:frame.asrt,
                    exprStr:frmIntsToStrExn(wrkCtx, frame, frame.asrt),
                    jstf:Some({args:argLabels,label:frame.label}),
                    isProved: false,
                }
            )->ignore
            results->Js.Array2.push({
                newVars: Belt_Array.range(0, frame.numOfVars-1),
                newVarTypes: frame.varTypes,
                newDisj,
                newDisjStr,
                stmts,
            })->ignore
        }

        framesProcessed.contents = framesProcessed.contents +. 1.
        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
            framesProcessed.contents /. numOfFrames
        )
    })
    results
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({label, typ, pattern}) => {
            let results = doSearchAssertions(
                ~wrkCtx=getWrkCtxExn(), 
                ~frms = getWrkFrmsExn(),
                ~label, 
                ~typ, 
                ~pattern, 
                ~onProgress = pct => sendToClient(OnProgress(pct)), 
                ()
            )
            sendToClient(SearchResult({found:results}))
        }
    }
}