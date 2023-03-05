open MM_wrk_client
open MM_parser
open MM_context

let procName = "MM_wrk_LoadCtx"

type mmScope = {
    ast: mmAstNode,
    expectedNumOfAssertions:int,
    stopBefore: option<string>,
    stopAfter: option<string>,
}

type request = 
    | LoadMmContext({scopes:array<mmScope>})

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

let beginLoadingMmContext = (~scopes:array<mmScope>, ~onProgress:float=>unit, ~onDone:result<mmContext,string>=>unit) => {
    beginWorkerInteraction(
        ~procName,
        ~initialRequest = LoadMmContext({ scopes:scopes }),
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

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | LoadMmContext({scopes}) => {
            let totalNumOfAssertions = scopes->Js_array2.reduce((a,e) => a+e.expectedNumOfAssertions, 0)->Belt_Int.toFloat
            let weights = scopes->Js_array2.map(s => s.expectedNumOfAssertions->Belt_Int.toFloat /. totalNumOfAssertions)
            try {
                let i = ref(0)
                let len = scopes->Js_array2.length
                let ctx = createContext(())
                while (i.contents < len) {
                    let scope = scopes[i.contents]
                    let basePct = weights->Js_array2.reducei((a,w,idx) => if idx < i.contents {a +. w} else {a}, 0.)
                    let weight = weights[i.contents]
                    loadContext(
                        scopes[i.contents].ast,
                        ~initialContext=ctx,
                        ~stopBefore=?scope.stopBefore,
                        ~stopAfter=?scope.stopAfter,
                        ~expectedNumOfAssertions=scope.expectedNumOfAssertions,
                        ~onProgress = pct => {
                            sendToClient(MmContextLoadProgress({pct: basePct +. pct *. weight}))
                        },
                        ()
                    )->ignore
                    while (ctx->getNestingLevel != 0) {
                        ctx->closeChildContext
                    }
                    i.contents = i.contents + 1
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
