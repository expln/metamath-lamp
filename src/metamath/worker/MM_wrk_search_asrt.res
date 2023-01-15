open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx
open MM_substitution
open MM_parenCounter

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({label:string, typ:int, pattern:array<int>})

type response =
    | OnProgress(float)
    | SearchResult({found:array<applyAssertionResult>})

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

let doSearchAssertions = (
    ~wrkCtx:mmContext,
    ~frms:Belt_MapString.t<frmSubsData>,
    ~parenCnt:parenCnt,
    ~label:string, 
    ~typ:int, 
    ~pattern:array<int>, 
    ~onProgress:option<float=>unit>=?, 
    ()
) => {
    let results = []
    applyAssertions(
        ~maxVar = wrkCtx->getNumOfVars - 1,
        ~frms,
        ~isDisjInCtx = wrkCtx->isDisj,
        ~statements = [],
        ~parenCnt,
        ~frameFilter = frame => 
            frame.label->Js.String2.toLowerCase->Js_string2.includes(label)
            && frame.asrt[0] == typ 
            && frameMatchesPattern(frame, pattern),
        ~onMatchFound = res => {
            results->Js_array2.push(res)->ignore
            Continue
        },
        ~onProgress?,
        ()
    )
    results
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({label, typ, pattern}) => {
            let results = doSearchAssertions(
                ~wrkCtx=getWrkCtxExn(), 
                ~frms = getWrkFrmsExn(), 
                ~parenCnt = getWrkParenCntExn(),
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