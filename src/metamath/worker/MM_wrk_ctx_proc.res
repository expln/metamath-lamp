open MM_context
open MM_wrk_client
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open Common
open MM_wrk_ctx_data
open MM_wrk_pre_ctx_data

let procName = "MM_wrk_ctx"

let thisProcName = procName

type request = 
    | PrepareWrkPrecalcData({
        settingsVer: int,
        preCtxVer: int,
        varsText: string,
        disjText: string,
    })
    | SetSettings({ settingsVer: int, settings: settings, })
    | SetPreCtx({ preCtxVer: int, preCtx: mmContext, })

type response =
    | GetSettings({ settingsVer: int })
    | GetPreCtx({preCtxVer: int})
    | Ok

let reqToStr = req => {
    switch req {
        | PrepareWrkPrecalcData(_) => "PrepareWrkPrecalcData"
        | SetSettings({ settingsVer}) => `SetSettings(settingsVer=${settingsVer->Belt_Int.toString})`
        | SetPreCtx({ preCtxVer }) => `SetPreCtx(preCtxVer=${preCtxVer->Belt_Int.toString})`
    }
}

let respToStr = resp => {
    switch resp {
        | GetSettings({ settingsVer }) => `GetSettings(settingsVer=${settingsVer->Belt_Int.toString})`
        | GetPreCtx({preCtxVer }) => `GetPreCtx(preCtxVer=${preCtxVer->Belt_Int.toString})`
        | Ok => `Ok`
    }
}

type wrkPrecalcData = {
    settings:settings,
    wrkFrms: frms,
    wrkParenCnt: parenCnt,
    wrkCtx: mmContext,
    allFramesInDeclarationOrder: array<frame>,
    frameDependencies:Belt_HashMapString.t<Belt_HashSetString.t>,
    syntaxTypes: array<int>,
}

let wrkPrecalcData = ref(None)

let getSettingsExn = ():settings => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`settings are not created in the worker thread.`}))
        | Some({settings}) => settings
    }
}

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

let getAllFramesInDeclarationOrderExn = () => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`wrkFrms is not created in the worker thread.`}))
        | Some({allFramesInDeclarationOrder}) => allFramesInDeclarationOrder
    }
}

let getFrameDependenciesExn = () => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`wrkFrms is not created in the worker thread.`}))
        | Some({frameDependencies}) => frameDependencies
    }
}

let getSyntaxTypesExn = ():array<int> => {
    switch wrkPrecalcData.contents {
        | None => raise(MmException({msg:`syntaxTypes are not created in the worker thread.`}))
        | Some({syntaxTypes}) => syntaxTypes
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
    ~recalc = ctx => prepareFrmSubsData(~ctx),
    ~depVerEq = (ctxV1,ctxV2) => ctxV1 == ctxV2
)

let parenCntCache = cacheMake(
    ~recalc = ((settings,ctx)) => MM_provers.makeParenCnt(~ctx, ~parens=settings.parens),
    ~depVerEq = ((sv1,cv1),(sv2,cv2)) => sv1 == sv2 && cv1 == cv2
)

let allFramesInDeclarationOrderCache = cacheMake(
    ~recalc = frms => {
        frms->frmsSelect
            ->Expln_utils_common.sortInPlaceWith((a,b) => Belt_Float.fromInt(a.frame.ord - b.frame.ord))
            ->Array.map(frm => frm.frame)
    },
    ~depVerEq = (ctxV1,ctxV2) => ctxV1 == ctxV2
)

let frameDependenciesCache = cacheMake(
    ~recalc = makeFrameDependencies,
    ~depVerEq = (ctxV1,ctxV2) => ctxV1 == ctxV2
)

let syntaxTypesCache = cacheMake(
    ~recalc = ctx => {
        let (_,syntaxTypes) = findTypes(ctx)
        syntaxTypes
    },
    ~depVerEq = (ctxV1,ctxV2) => ctxV1 == ctxV2
)

let makeWrkPrecalcData = (
    ~settingsVer:int, 
    ~preCtxVer:int,
    ~varsText: string,
    ~disjText: string,
):result<wrkPrecalcData,response> => {
    switch settingsCache->cacheGetByDepVer(settingsVer) {
        | None => Error(GetSettings({ settingsVer:settingsVer }))
        | Some(settings) => {
            switch preCtxCache->cacheGetByDepVer(preCtxVer) {
                | None => Error(GetPreCtx({ preCtxVer:preCtxVer }))
                | Some(preCtx) => {
                    switch createWrkCtx( ~preCtx, ~varsText, ~disjText, ) {
                        | Error(_) => raise(MmException({msg:`There was an error creating wrkCtx in the worker thread.`}))
                        | Ok(wrkCtx) => {
                            let wrkFrms = frmsCache->cacheGet(preCtxVer, preCtx)
                            let allFramesInDeclarationOrder = allFramesInDeclarationOrderCache->cacheGet(
                                preCtxVer, wrkFrms
                            )
                            Ok({
                                settings,
                                wrkFrms,
                                wrkParenCnt: parenCntCache->cacheGet((settingsVer, preCtxVer), (settings, preCtx)),
                                wrkCtx,
                                allFramesInDeclarationOrder,
                                frameDependencies: frameDependenciesCache->cacheGet(
                                    preCtxVer, allFramesInDeclarationOrder
                                ),
                                syntaxTypes: syntaxTypesCache->cacheGet(preCtxVer, preCtx),
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
        | PrepareWrkPrecalcData({ settingsVer, preCtxVer, varsText, disjText, }) => {
            wrkPrecalcData.contents = None
            switch makeWrkPrecalcData( ~settingsVer, ~preCtxVer, ~varsText, ~disjText, ) {
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
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false
) => {
    let prepareWrkPrecalcDataReq = PrepareWrkPrecalcData({ settingsVer, preCtxVer, varsText, disjText, })
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
                    beginWorkerInteraction(~procName, ~initialRequest, ~onResponse, ~enableTrace)
                }
            }
        },
        ~enableTrace
    )
}