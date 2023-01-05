open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({label:string, typ:int, pattern:array<int>})

type response =
    | OnProgress(float)
    | SearchResult({found:array<applyAssertionResult>})

let rec frameMatchesPatternPriv = (frm:frame, pat:array<int>, aIdx:int, pIdx:int):bool => {
    let asrtLen = frm.asrt->Js_array2.length
    if (pIdx >= pat->Js_array2.length) {
        true
    } else if (aIdx >= asrtLen) {
        false
    } else {
        let aIdx = ref(aIdx)
        let matchFound = ref(false)
        while (!matchFound.contents && aIdx.contents < asrtLen) {
            if (
                frm.asrt[aIdx.contents] < 0 && frm.asrt[aIdx.contents] == pat[pIdx]
                || frm.asrt[aIdx.contents] >= 0 && frm.varTypes[frm.asrt[aIdx.contents]] == pat[pIdx]
            ) {
                matchFound.contents = frameMatchesPatternPriv(frm, pat, aIdx.contents+1, pIdx+1)
            }
            aIdx.contents = aIdx.contents + 1
        }
        matchFound.contents
    }
}

let rec frameMatchesPattern = (frm:frame, pat:array<int>):bool => frameMatchesPatternPriv(frm,pat,0,0)

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
): promise<array<applyAssertionResult>> => {
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

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({label, typ, pattern}) => {
            let results = []
            applyAssertions(
                ~maxVar = getWrkCtxExn()->getNumOfVars - 1,
                ~frms = getWrkFrmsExn(),
                ~isDisjInCtx = getWrkCtxExn()->isDisj,
                ~statements = [],
                ~parenCnt = getWrkParenCntExn(),
                ~frameFilter = frame => 
                    frame.label->Js.String2.toLowerCase->Js_string2.includes(label)
                    && frame.asrt[0] == typ 
                    && frameMatchesPattern(frame, pattern),
                ~onMatchFound = res => {
                    results->Js_array2.push(res)->ignore
                    Continue
                },
                ~onProgress = pct => sendToClient(OnProgress(pct)),
                ()
            )
            sendToClient(SearchResult({found:results}))
        }
    }
}