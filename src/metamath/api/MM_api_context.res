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

let updateContextApi = (
    ~loadContext:array<ctxSrcToLoad>=>promise<result<loadedCtxInfo,string>>,
):unit => {
    setContextApi({
        "load": makeApiFunc("context.load", params => {
            switch parseParams(params) {
                | Error(msg) => Promise.resolve(Error(msg))
                | Ok(srcs) => loadContext(srcs)->Promise.thenResolve(res => res->Result.map(loadedCtxInfoToJson))
            }
        }),
    })
}
