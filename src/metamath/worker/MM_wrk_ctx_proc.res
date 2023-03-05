open MM_context
open MM_wrk_client
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_parser
open Common
open MM_wrk_ctx_data

let procName = "MM_wrk_ctx"

let thisProcName = procName

type request = 
    | PrepareWrkPrecalcData({
        settingsVer: int,
        preCtxVer: int,
        varsText: string,
        disjText: string,
        hyps: array<wrkCtxHyp>,
    })
    | SetSettings({ settingsVer: int, settings: settings, })
    | SetPreCtx({ preCtxVer: int, preCtx: mmContext, })

type response =
    | GetSettings({ settingsVer: int })
    | GetPreCtx({preCtxVer: int})
    | Ok

type wrkPrecalcData = {
    wrkFrms: Belt_MapString.t<frmSubsData>,
    wrkParenCnt: parenCnt,
    wrkCtx: mmContext,
}

let wrkPrecalcData = ref(None)

let getWrkCtxExn = () => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`wrkCtx is not created in the worker thread.`}))
        | Some({wrkCtx}) => wrkCtx
    }
}

let getWrkParenCntExn = () => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`wrkParenCnt is not created in the worker thread.`}))
        | Some({wrkParenCnt}) => wrkParenCnt
    }
}

let getWrkFrmsExn = () => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`wrkFrms is not created in the worker thread.`}))
        | Some({wrkFrms}) => wrkFrms
    }
}

let settingsCache = cacheMake(
    ~recalc = settings => settings,
    ~depVerEq = (v1,v2) => v1 == v2
)

let preCtxCache = cacheMake(
    ~recalc = ctx => ctx,
    ~depVerEq = (v1,v2) => v1 == v2
)

let frmsCache = cacheMake(
    ~recalc = ((settings,ctx)) => {
        prepareFrmSubsData(~ctx, ~asrtsToSkip=Belt_HashSetString.fromArray(settings.asrtsToSkip), ())
    },
    ~depVerEq = ((sv1,cv1),(sv2,cv2)) => sv1 == sv2 && cv1 == cv2
)

let parenCntCache = cacheMake(
    ~recalc = ((settings,ctx)) => parenCntMake(prepareParenInts(ctx, settings.parens), ()),
    ~depVerEq = ((sv1,cv1),(sv2,cv2)) => sv1 == sv2 && cv1 == cv2
)

let makeWrkPrecalcData = (
    ~settingsVer:int, 
    ~preCtxVer:int,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
):result<wrkPrecalcData,response> => {
    switch settingsCache->cacheGetByDepVer(settingsVer) {
        | None => Error(GetSettings({ settingsVer:settingsVer }))
        | Some(settings) => {
            switch preCtxCache->cacheGetByDepVer(preCtxVer) {
                | None => Error(GetPreCtx({ preCtxVer:preCtxVer }))
                | Some(preCtx) => {
                    switch createWrkCtx( ~preCtx, ~varsText, ~disjText, ~hyps, ) {
                        | Error(_) => raise(MmException({msg:`There was an error creating wrkCtx in the worker thread.`}))
                        | Ok(wrkCtx) => {
                            Ok({
                                wrkFrms: frmsCache->cacheGet((settingsVer, preCtxVer), (settings, preCtx)),
                                wrkParenCnt: parenCntCache->cacheGet((settingsVer, preCtxVer), (settings, preCtx)),
                                wrkCtx,
                            })
                        }
                    }
                }
            }
        }
    }
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | PrepareWrkPrecalcData({ settingsVer, preCtxVer, varsText, disjText, hyps, }) => {
            wrkPrecalcData.contents = None
            switch makeWrkPrecalcData( ~settingsVer, ~preCtxVer, ~varsText, ~disjText, ~hyps, ) {
                | Error(resp) => sendToClient(resp)
                | Ok(data) => {
                    wrkPrecalcData.contents = Some(data)
                    sendToClient(Ok)
                }
            }
        }
        | SetSettings({ settingsVer, settings }) => {
            settingsCache->cacheGet(settingsVer, settings)->ignore
        }
        | SetPreCtx({ preCtxVer, preCtx }) => {
            preCtxCache->cacheGet(preCtxVer, preCtx)->ignore
        }
    }
}

let beginWorkerInteractionUsingCtx = (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    let prepareWrkPrecalcDataReq = PrepareWrkPrecalcData({ settingsVer, preCtxVer, varsText, disjText, hyps, })
    beginWorkerInteraction(
        ~procName = thisProcName,
        ~initialRequest = prepareWrkPrecalcDataReq, 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetSettings({settingsVer:requestedSettingsVer}) => {
                    if (requestedSettingsVer == settingsVer) {
                        sendToWorker(SetSettings({ settingsVer, settings }))
                        sendToWorker(prepareWrkPrecalcDataReq)
                    }
                }
                | GetPreCtx({preCtxVer:requestedPreCtxVer}) => {
                    if (requestedPreCtxVer == preCtxVer) {
                        sendToWorker(SetPreCtx({ preCtxVer, preCtx }))
                        sendToWorker(prepareWrkPrecalcDataReq)
                    }
                }
                | Ok => {
                    endWorkerInteraction()
                    beginWorkerInteraction(~procName, ~initialRequest, ~onResponse, ~enableTrace, ())
                }
            }
        },
        ~enableTrace,
        ()
    )
}