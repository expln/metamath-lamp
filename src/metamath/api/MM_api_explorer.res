open MM_context
open MM_api
open MM_wrk_pre_ctx_data

let frameToJson = (~preCtxData:preCtxData, ~typeColors:Belt_HashMapString.t<string>, frm:frame):JSON.t => {
    let preCtx = preCtxData.ctxV.val.min
    let frmData = MM_cmp_pe_frame_summary_state.makeInitialState(
        ~preCtx,
        ~frame=frm,
        ~typeColors,
        ~typeOrderInDisj=preCtxData.typeOrderInDisj
    )
    let disj = frmData.disj->Option.getOr([])->Array.map(disjGrp =>
        disjGrp->Array.map(((var,_)) => var->JSON.Encode.string)->JSON.Encode.array
    )->JSON.Encode.array
    let hyps = frmData.eHyps->Array.map(hyp =>
        ctxIntsToStrExn(preCtx, hyp)->JSON.Encode.string
    )->JSON.Encode.array
    Dict.fromArray([
        ("label", frm.label->JSON.Encode.string),
        ("isAxiom", frm.isAxiom->JSON.Encode.bool),
        ("disj", disj),
        ("hyps", hyps),
        ("asrt", ctxIntsToStrExn(preCtx, frmData.asrt)->JSON.Encode.string),
    ])->JSON.Encode.object
}

let framesToJson = (~preCtxData:preCtxData, frms:array<frame>):JSON.t => {
    let typeColors = Belt_HashMapString.make(~hintSize=0)
    frms->Array.map(frameToJson(~preCtxData, ~typeColors, _))->JSON.Encode.array
}

let defaultPageSize = 100

type backend = {
    preCtxData:preCtxData,
    searchAssertions:(~pattern:string)=>promise<result<array<frame>,string>>,
    getEditorScope:int=>result<(int,string),string>,
}

type cachedSearchResult = {
    settingsVer:int,
    preCtxVer:int,
    pattern:string,
    frms:array<frame>,
}

// The result of the last search, so that requesting other pages of the same search doesn't repeat the search.
// Results are not limited by an editor scope, so the same cached result is used for all editors.
let lastSearchResult:ref<option<cachedSearchResult>> = ref(None)

let searchWithCache = (~backend:backend, ~pattern:string):promise<result<array<frame>,string>> => {
    let settingsVer = backend.preCtxData.settingsV.ver
    let preCtxVer = backend.preCtxData.ctxV.ver
    switch lastSearchResult.contents {
        | Some(cached) if
            cached.settingsVer == settingsVer && cached.preCtxVer == preCtxVer && cached.pattern == pattern => {
            Promise.resolve(Ok(cached.frms))
        }
        | _ => {
            backend.searchAssertions(~pattern)->Promise.thenResolve(res => {
                switch res {
                    | Error(_) => ()
                    | Ok(frms) => lastSearchResult := Some({settingsVer, preCtxVer, pattern, frms})
                }
                res
            })
        }
    }
}

let searchAssertions = (
    ~backend:backend, ~pattern:string, ~maxFrmOrd:option<int>
):promise<result<array<frame>,string>> => {
    searchWithCache(~backend, ~pattern)->Promise.thenResolve(frms => frms->Result.map(frms => {
        switch maxFrmOrd {
            | None => frms
            | Some(maxFrmOrd) => frms->Array.filter(frm => frm.ord <= maxFrmOrd)
        }
    }))
}

let findAssertions = (~params:apiInput, ~backend:backend):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let positive = (name:string, i:int):result<int,string> =>
        if (i < 1) { Error(`'${name}' must be a positive integer.`) } else { Ok(i) }
    let parseResult:result<{"pattern":string, "pageNum":int, "pageSize":int, "editorId":option<int>},string> =
        fromJson(params->apiInputToJson, asObj(_, d=>{
            {
                "pattern": d->str("pattern"),
                "pageNum": d->int("pageNum", ~default=()=>1, ~validator=positive("pageNum", _)),
                "pageSize": d->int("pageSize", ~default=()=>defaultPageSize, ~validator=positive("pageSize", _)),
                "editorId": d->intOpt("editorId"),
            }
        }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(parsedParams) => {
            let scope = switch parsedParams["editorId"] {
                | None => Ok(None)
                | Some(editorId) => backend.getEditorScope(editorId)->Result.map(scope => Some(scope))
            }
            switch scope {
                | Error(msg) => Promise.resolve(Error(msg))
                | Ok(scope) => {
                    let pageNum = parsedParams["pageNum"]
                    let pageSize = parsedParams["pageSize"]
                    let maxFrmOrd = scope->Option.map(((maxFrmOrd,_)) => maxFrmOrd)
                    searchAssertions(~backend, ~pattern=parsedParams["pattern"], ~maxFrmOrd)
                        ->Promise.thenResolve(frms => frms->Result.map(frms => {
                            let totalCount = frms->Array.length
                            let minIdx = (pageNum - 1) * pageSize
                            let res = [
                                ("totalCount", totalCount->Int.toFloat->JSON.Encode.float),
                                ("pageNum", pageNum->Int.toFloat->JSON.Encode.float),
                                ("pageSize", pageSize->Int.toFloat->JSON.Encode.float),
                                (
                                    "numOfPages",
                                    ((totalCount + pageSize - 1) / pageSize)->Int.toFloat->JSON.Encode.float
                                ),
                                (
                                    "asrts",
                                    frms->Array.slice(~start=minIdx, ~end=minIdx + pageSize)
                                        ->framesToJson(~preCtxData=backend.preCtxData, _)
                                ),
                            ]
                            switch scope {
                                | None => ()
                                | Some((_, scopeDescr)) => {
                                    res->Array.push(("editorScope", scopeDescr->JSON.Encode.string))
                                }
                            }
                            Dict.fromArray(res)->JSON.Encode.object
                        }))
                }
            }
        }
    }
}

let updateExplorerApi = (
    ~preCtxData:preCtxData,
    ~searchAssertions:(~pattern:string)=>promise<result<array<frame>,string>>,
    ~getEditorScope:int=>result<(int,string),string>,
):unit => {
    let backend = {preCtxData, searchAssertions, getEditorScope}
    setExplorerApi({
        "findAssertions": makeApiFunc("explorer.findAssertions", params => findAssertions(~params, ~backend)),
    })
}
