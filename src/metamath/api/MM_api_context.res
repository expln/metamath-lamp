open MM_api
open MM_wrk_editor

type ctxSrcToLoad = {
    url:string,
    readInstr:readInstr,
    label:option<string>,
}

type loadedCtxInfo = {
    summary:string,
    numOfAsrts:int,
}

type rawCtxSrc = {
    rawUrl:string,
    stopBefore:option<string>,
    stopAfter:option<string>,
}

let rawSrcToCtxSrc = (raw:rawCtxSrc):result<ctxSrcToLoad,string> => {
    let url = raw.rawUrl->String.trim
    let nonEmptyLabel = (attrName:string, label:string):result<string,string> => {
        let label = label->String.trim
        if (label == "") {
            Error(`'${attrName}' must not be empty for '${url}'.`)
        } else {
            Ok(label)
        }
    }
    if (url == "") {
        Error(`'url' must not be empty.`)
    } else {
        switch (raw.stopBefore, raw.stopAfter) {
            | (Some(_), Some(_)) => Error(`Only one of 'stopBefore' and 'stopAfter' may be specified for '${url}'.`)
            | (Some(label), None) => nonEmptyLabel("stopBefore", label)
                ->Result.map(label => {url, readInstr:StopBefore, label:Some(label)})
            | (None, Some(label)) => nonEmptyLabel("stopAfter", label)
                ->Result.map(label => {url, readInstr:StopAfter, label:Some(label)})
            | (None, None) => Ok({url, readInstr:ReadAll, label:None})
        }
    }
}

let parseParams = (params:apiInput):result<array<ctxSrcToLoad>,string> => {
    open Expln_utils_jsonParse
    let rawSrcs:result<array<rawCtxSrc>,string> = fromJson(params->apiInputToJson, asObj(_, d => {
        d->arr("srcs", asObj(_, s => {
            {
                rawUrl: s->str("url"),
                stopBefore: s->strOpt("stopBefore"),
                stopAfter: s->strOpt("stopAfter"),
            }
        }))
    }))
    switch rawSrcs {
        | Error(msg) => Error(`Could not parse input parameters: ${msg}`)
        | Ok([]) => Error(`'srcs' must not be empty.`)
        | Ok(rawSrcs) => {
            rawSrcs->Array.reduce(Ok([]), (res, raw) => {
                res->Result.flatMap(srcs => {
                    rawSrcToCtxSrc(raw)->Result.map(src => {
                        srcs->Array.push(src)
                        srcs
                    })
                })
            })
        }
    }
}

let loadedCtxInfoToJson = (info:loadedCtxInfo):JSON.t => {
    Dict.fromArray([
        ("summary", info.summary->JSON.Encode.string),
        ("numOfAsrts", info.numOfAsrts->Int.toFloat->JSON.Encode.float),
    ])->JSON.Encode.object
}

let intToJson = (i:int):JSON.t => i->Int.toFloat->JSON.Encode.float

type asrtCounts = {
    numOfAsrts:int,
    numOfAxioms:int,
}

/*
* Counts the assertions with the given labels which are present in the context.
* Labels of assertions which were not loaded (e.g. because of stopBefore/stopAfter) are not counted.
* */
let countAsrts = (ctx:MM_context.mmContext, labels:array<string>):asrtCounts => {
    labels->Array.reduce({numOfAsrts:0, numOfAxioms:0}, (cnt, label) => {
        switch ctx->MM_context.getFrame(label) {
            | None => cnt
            | Some(frame) => {
                numOfAsrts: cnt.numOfAsrts + 1,
                numOfAxioms: cnt.numOfAxioms + (frame.isAxiom ? 1 : 0),
            }
        }
    })
}

let asrtCountsToJsonFields = (cnt:asrtCounts):array<(string,JSON.t)> => {
    [
        ("numOfAsrts", cnt.numOfAsrts->intToJson),
        ("numOfAxioms", cnt.numOfAxioms->intToJson),
    ]
}

let readInstrToApiStr = (readInstr:string):string => {
    switch readInstrFromStr(readInstr) {
        | ReadAll => "readAll"
        | StopBefore => "stopBefore"
        | StopAfter => "stopAfter"
    }
}

let srcToJson = (
    ~ctx:MM_context.mmContext,
    ~webSrcSettings:array<MM_wrk_settings.webSrcSettings>,
    src:MM_wrk_pre_ctx_data.mmCtxSrcDto,
):JSON.t => {
    let isWeb = src.typ == Web->mmFileSourceTypeToStr
    let alias = if (isWeb) {
        webSrcSettings
            ->Array.find(ws => ws.url == src.url && ws.alias->String.trim != "" && ws.alias != src.url)
            ->Option.map(ws => ws.alias->JSON.Encode.string)
            ->Option.getOr(JSON.Encode.null)
    } else {
        JSON.Encode.null
    }
    let readInstr = src.readInstr->readInstrToApiStr
    // fileToLabels is empty when the source doesn't include other files.
    let files = src.fileToLabels->Array.map(((fileName, labels)) => {
        Dict.fromArray(
            [("fileName", fileName->JSON.Encode.string)]->Array.concat(countAsrts(ctx, labels)->asrtCountsToJsonFields)
        )->JSON.Encode.object
    })
    Dict.fromArray(
        [
            ("type", (isWeb ? "web" : "local")->JSON.Encode.string),
            ("url", (isWeb ? src.url : "")->JSON.Encode.string),
            ("fileName", (isWeb ? "" : src.fileName)->JSON.Encode.string),
            ("alias", alias),
            ("readInstr", readInstr->JSON.Encode.string),
            ("label", (readInstr == "readAll" ? JSON.Encode.null : src.label->JSON.Encode.string)),
            ("files", files->JSON.Encode.array),
        ]->Array.concat(countAsrts(ctx, src.allLabels)->asrtCountsToJsonFields)
    )->JSON.Encode.object
}

/*
* Describes the loaded context: the sources in the order of loading (with the files they include),
* how each source is read (whole or up to a label), and how many assertions are loaded in total and per source.
* */
let getLoadedContextInfo = (preCtxData:MM_wrk_pre_ctx_data.preCtxData):JSON.t => {
    let ctx = preCtxData.ctxV.val.full
    let numOfAsrts = ctx->MM_context.getNumOfFrames
    let numOfAxioms = ref(0)
    ctx->MM_context.forEachFrame(frame => {
        if (frame.isAxiom) {
            numOfAxioms := numOfAxioms.contents + 1
        }
        None
    })->ignore
    Dict.fromArray([
        ("numOfAsrts", numOfAsrts->intToJson),
        ("numOfAxioms", numOfAxioms.contents->intToJson),
        (
            "srcs",
            preCtxData.srcs->Array.map(
                srcToJson(~ctx, ~webSrcSettings=preCtxData.settingsV.val.webSrcSettings, _)
            )->JSON.Encode.array
        ),
    ])->JSON.Encode.object
}

// The context API is provided by two components: the context selector loads contexts,
// and the root component knows the loaded context. Each of them updates its own part.
let loadContextRef:ref<option<array<ctxSrcToLoad>=>promise<result<loadedCtxInfo,string>>>> = ref(None)
let preCtxDataRef:ref<option<MM_wrk_pre_ctx_data.preCtxData>> = ref(None)

let setApi = ():unit => {
    setContextApi({
        "load": makeApiFunc("context.load", params => {
            switch loadContextRef.contents {
                | None => Promise.resolve(Error("Loading a context is not available yet."))
                | Some(loadContext) => {
                    switch parseParams(params) {
                        | Error(msg) => Promise.resolve(Error(msg))
                        | Ok(srcs) => {
                            loadContext(srcs)->Promise.thenResolve(res => res->Result.map(loadedCtxInfoToJson))
                        }
                    }
                }
            }
        }),
        "getLoadedContextInfo": makeApiFunc("context.getLoadedContextInfo", _ => {
            switch preCtxDataRef.contents {
                | None => Promise.resolve(Error("Information about the loaded context is not available yet."))
                | Some(preCtxData) => Promise.resolve(Ok(getLoadedContextInfo(preCtxData)))
            }
        }),
    })
}

let updateContextApi = (
    ~loadContext:array<ctxSrcToLoad>=>promise<result<loadedCtxInfo,string>>,
):unit => {
    loadContextRef := Some(loadContext)
    setApi()
}

let updateLoadedContext = (~preCtxData:MM_wrk_pre_ctx_data.preCtxData):unit => {
    preCtxDataRef := Some(preCtxData)
    setApi()
}
