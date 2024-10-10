open MM_wrk_client
open MM_parser
open MM_context
open Common
open MM_wrk_pre_ctx_data
open MM_wrk_editor

let procName = "MM_wrk_LoadCtx"

type mmScope = {
    ast: mmAstNode,
    expectedNumOfAssertions:int,
    stopBefore: option<string>,
    stopAfter: option<string>,
    resetNestingLevel:bool,
}

type request = 
    | LoadMmContext({
        scopes:array<mmScope>, 
        descrRegexToDisc:string, 
        labelRegexToDisc:string,
        descrRegexToDepr:string, 
        labelRegexToDepr:string,
    })

type response =
    | MmContextLoadProgress({pct:float})
    | MmContextLoaded({ctx:result<mmContext,string>})

let reqToStr = req => {
    switch req {
        | LoadMmContext(_) => "LoadMmContext"
    }
}

let respToStr = resp => {
    switch resp {
        | MmContextLoadProgress({pct}) => `MmContextLoadProgress(pct=${pct->Belt_Float.toString})`
        | MmContextLoaded(_) => `MmContextLoaded`
    }
}

let beginLoadingMmContext = (
    ~scopes:array<mmScope>, 
    ~descrRegexToDisc:string,
    ~labelRegexToDisc:string,
    ~descrRegexToDepr:string,
    ~labelRegexToDepr:string,
    ~onProgress:float=>unit, 
    ~onDone:result<mmContext,string>=>unit,
) => {
    beginWorkerInteraction(
        ~procName,
        ~initialRequest = LoadMmContext({ 
            scopes:scopes, 
            descrRegexToDisc, 
            labelRegexToDisc, 
            descrRegexToDepr, 
            labelRegexToDepr 
        }),
        ~onResponse = (~resp:response, ~sendToWorker as _, ~endWorkerInteraction:unit=>unit) => {
            switch resp {
                | MmContextLoadProgress({pct}) => onProgress(pct)
                | MmContextLoaded({ctx}) => {
                    endWorkerInteraction()
                    onDone(ctx)
                }
            }
        },
        ()
    )
}

let strToRegexOpt = (str:string):option<Js_re.t> => {
    let str = str->Js_string2.trim
    if (str->Js.String2.length == 0) {
        None
    } else {
        switch str->strToRegex {
            | Error(msg) => raise(MmException({msg:`Could not parse a regex: ${msg}`}))
            | Ok(re) => Some(re)
        }
    }
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | LoadMmContext({scopes, descrRegexToDisc, labelRegexToDisc, descrRegexToDepr, labelRegexToDepr}) => {
            let totalNumOfAssertions = scopes->Js_array2.reduce((a,e) => a+e.expectedNumOfAssertions, 0)->Belt_Int.toFloat
            let weights = scopes->Js_array2.map(s => s.expectedNumOfAssertions->Belt_Int.toFloat /. totalNumOfAssertions)
            try {
                let ctx = createContext(())
                for i in 0 to scopes->Js_array2.length-1 {
                    let scope = scopes->Array.getUnsafe(i)
                    let basePct = weights->Js_array2.reducei((a,w,idx) => if idx < i {a +. w} else {a}, 0.)
                    let weight = weights->Array.getUnsafe(i)
                    loadContext(
                        (scopes->Array.getUnsafe(i)).ast,
                        ~initialContext=ctx,
                        ~stopBefore=?scope.stopBefore,
                        ~stopAfter=?scope.stopAfter,
                        ~expectedNumOfAssertions=scope.expectedNumOfAssertions,
                        ~descrRegexToDisc=?strToRegexOpt(descrRegexToDisc),
                        ~labelRegexToDisc=?strToRegexOpt(labelRegexToDisc),
                        ~descrRegexToDepr=?strToRegexOpt(descrRegexToDepr),
                        ~labelRegexToDepr=?strToRegexOpt(labelRegexToDepr),
                        ~onProgress = pct => {
                            sendToClient(MmContextLoadProgress({pct: basePct +. pct *. weight}))
                        },
                        ()
                    )->ignore
                    if (scope.resetNestingLevel) {
                        while (ctx->getNestingLevel != 0) {
                            ctx->closeChildContext
                        }
                    }
                }
                sendToClient(MmContextLoadProgress({pct: 1.}))
                sendToClient(MmContextLoaded({ctx:Ok(ctx)}))
            } catch {
                | MmException({msg}) => {
                    sendToClient(MmContextLoaded({ctx:Error(msg)}))
                }
                | Js.Exn.Error(exn) => {
                    sendToClient(MmContextLoaded({ctx:Error(exn->Js.Exn.message->Belt_Option.getWithDefault("Internal error."))}))
                }
            }
        }
    }
}

let getAllLabelsAfterReading = (src:mmCtxSrcDto):(option<string>, option<string>, array<string>) => {
    switch src.readInstr->readInstrFromStr {
        | ReadAll => (None, None, src.allLabels)
        | StopBefore => {
            switch src.allLabels->Js_array2.findIndex(label => label == src.label) {
                | -1 => (None, None, src.allLabels)
                | idx => (Some(src.label), None, src.allLabels->Js_array2.slice(~start=0, ~end_=idx))
            }
        }
        | StopAfter => {
            switch src.allLabels->Js_array2.findIndex(label => label == src.label) {
                | -1 => (None, None, src.allLabels)
                | idx => (None, Some(src.label), src.allLabels->Js_array2.slice(~start=0, ~end_=idx+1))
            }
        }
    }
}

let convertSrcDtoAndAddToRes = (~src:mmCtxSrcDto, ~label:string, ~res:array<mmScope>):bool => {
    let (stopBeforeOrig, stopAfterOrig, allLabels) = getAllLabelsAfterReading(src)
    let (stopBefore, stopAfter, expectedNumOfAssertions, resetNestingLevel) =
        if (allLabels->Js_array2.includes(label)) {
            (
                Some(label),
                None,
                allLabels->Js_array2.indexOf(label),
                false
            )
        } else {
            (
                stopBeforeOrig,
                stopAfterOrig,
                allLabels->Js_array2.length,
                true
            )
        }
    let ast = switch src.ast {
        | Some(ast) => ast
        | _ => raise(MmException({msg:`Cannot create MM context for a frame without ast.`}))
    }
    let mmScope = {
        ast,
        expectedNumOfAssertions,
        stopBefore,
        stopAfter,
        resetNestingLevel,
    }
    res->Array.push(mmScope)
    allLabels->Js_array2.length != expectedNumOfAssertions
}

let createMmScopesForFrame = ( ~srcs:array<mmCtxSrcDto>, ~label:string, ):array<mmScope> => {
    let res = []
    srcs->Js_array2.reduce(
        (found,src) => {
            if (found) {
                found
            } else {
                convertSrcDtoAndAddToRes(~src, ~label, ~res)
            }
        },
        false
    )->ignore
    res
}