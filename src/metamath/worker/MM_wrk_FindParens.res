open MM_wrk_client
open MM_context

let procName = "MM_wrk_FindParens"

type request = 
    | FindParens({ctx: mmContext})

type response =
    | FindParensProgress({pct: float})
    | FindParensDone({parens: string})

let beginFindParens = (~ctx, ~onProgress:float=>unit, ~onDone:string=>unit) => {
    beginWorkerInteraction(
        ~procName,
        ~initialRequest = FindParens({ctx:ctx}),
        ~onResponse = (~resp:response, ~sendToWorker as _, ~endWorkerInteraction:unit=>unit) => {
            switch resp {
                | FindParensProgress({pct}) => onProgress(pct)
                | FindParensDone({parens}) => {
                    endWorkerInteraction()
                    onDone(parens)
                }
            }
        },
        ()
    )
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindParens({ctx}) => {
            sendToClient(FindParensDone({
                parens: findParentheses(ctx, ~onProgress = pct => sendToClient(FindParensProgress({pct:pct})), ())->ctxIntsToStrExn(ctx, _)
            }))
        }
    }
}
