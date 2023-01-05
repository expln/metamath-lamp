open MM_context
open MM_wrk_client
open MM_parenCounter
open MM_substitution
open MM_parser
open Common

let procName = "MM_wrk_ctx"

type wrkCtxHyp = {
    id: string,
    label: string,
    text: string,
}

type wrkPrecalcData = {
    preCtxVer: int,
    preCtx: option<mmContext>,
    wrkFrms: option<Belt_MapString.t<frmSubsData>>,
    parenStr: string,
    wrkParenInts: option<array<int>>,
    wrkParenCnt: option<parenCnt>,
    varsText: string,
    disjText: string,
    hyps: array<wrkCtxHyp>,
    wrkCtx: option<mmContext>,
}

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
    hypErr: option<(string,string)>,
}

let makeEmptyWrkPrecalcDataErr = () => {
    varsErr: None,
    disjErr: None,
    hypErr: None,
}

let createInitialWrkPrecalcData = (
    ~preCtxVer: int,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
) => {
    {
        preCtxVer,
        preCtx: None,
        wrkFrms: None,
        parenStr,
        wrkParenInts: None,
        wrkParenCnt: None,
        varsText,
        disjText,
        hyps,
        wrkCtx: None,
    }
}

let wrkPrecalcData = ref(createInitialWrkPrecalcData(
    ~preCtxVer=-1,
    ~parenStr="",
    ~varsText="",
    ~disjText="",
    ~hyps=[],
))

let getWrkCtxExn = () => {
    switch wrkPrecalcData.contents.wrkCtx {
        | None => raise(MmException({msg:`wrkCtx is not created in the worker thread.`}))
        | Some(wrkCtx) => wrkCtx
    }
}

let getWrkParenCntExn = () => {
    switch wrkPrecalcData.contents.wrkParenCnt {
        | None => raise(MmException({msg:`wrkParenCnt is not created in the worker thread.`}))
        | Some(wrkParenCnt) => wrkParenCnt
    }
}

let getWrkFrmsExn = () => {
    switch wrkPrecalcData.contents.wrkFrms {
        | None => raise(MmException({msg:`wrkFrms is not created in the worker thread.`}))
        | Some(wrkFrms) => wrkFrms
    }
}

let preCtxCache = cacheMake(
    ~recalc = ctx => (ctx,prepareFrmSubsData(ctx)),
    ~depVerEq = (preCtxV1,preCtxV2) => preCtxV1 == preCtxV2
)

type request = 
    | PrepareWrkPrecalcData({
        preCtxVer: int,
        parenStr: string,
        varsText: string,
        disjText: string,
        hyps: array<wrkCtxHyp>,
    })
    | SetPreCtx({ preCtxVer: int, preCtx: mmContext, })

type response =
    | GetPreCtx({preCtxVer: int})
    | Ok

let thisProcName = procName

let prepareParenInts = (wrkCtx, parenStr) => {
    let parenSyms = parenStr->getSpaceSeparatedValuesAsArray
    let parensInt = []
    let maxI = parenSyms->Js_array2.length / 2 - 1
    for i in 0 to maxI {
        let leftParen = parenSyms[i*2]
        let rightParen = parenSyms[i*2+1]
        switch wrkCtx->ctxSymToInt(leftParen) {
            | Some(leftParenInt) if wrkCtx->isConst(leftParen) => {
                switch wrkCtx->ctxSymToInt(rightParen) {
                    | Some(rightParenInt) if wrkCtx->isConst(rightParen) => {
                        parensInt->Js.Array2.push(leftParenInt)->ignore
                        parensInt->Js.Array2.push(rightParenInt)->ignore
                    }
                    | _ => ()
                }
            }
            | _ => ()
        }
    }
    parensInt
}

let addVarFromString = (wrkCtx, str) => {
    let arr = getSpaceSeparatedValuesAsArray(str)
    if (arr->Js_array2.length != 3) {
        raise(MmException({msg:`Cannot convert '${str}' to Var statement.`}))
    } else {
        wrkCtx->applySingleStmt(Var({symbols:[arr[2]]}))
        wrkCtx->applySingleStmt(Floating({label:arr[0], expr:[arr[1], arr[2]]}))
    }
}

let newLineRegex = %re("/[\n\r]/")
let parseVariables = (wrkCtx, varsText):option<wrkCtxErr> => {
    let varLines = varsText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    if (varLines->Js.Array2.length == 0) {
        None
    } else {
        try {
            varLines->Js_array2.forEach(addVarFromString(wrkCtx))
            None
        } catch {
            | MmException({msg}) => Some({...makeEmptyWrkPrecalcDataErr(), varsErr:Some(msg)})
        }
    }
}

let addDisjFromString = (wrkCtx, disjStr) => {
    wrkCtx->applySingleStmt(Disj({vars:disjStr->Js.String2.split(",")->Js_array2.map(Js_string2.trim)}))
}

let parseDisjoints = (wrkCtx, disjText):option<wrkCtxErr> => {
    let disjLines = disjText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    try {
        disjLines->Js_array2.forEach(addDisjFromString(wrkCtx))
        None
    } catch {
        | MmException({msg}) => Some({...makeEmptyWrkPrecalcDataErr(), disjErr:Some(msg)})
    }
}

let addStmtToCtx = (wrkCtx:mmContext, stmt:wrkCtxHyp):option<wrkCtxErr> => {
    try {
        wrkCtx->applySingleStmt(Essential({label:stmt.label, expr:getSpaceSeparatedValuesAsArray(stmt.text)}))
        None
    } catch {
        | MmException({msg}) => Some({...makeEmptyWrkPrecalcDataErr(), hypErr:Some((stmt.id,msg))})
    }
}

let createWrkCtx = (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
): result<mmContext,wrkCtxErr> => {
    let wrkCtx = createContext(~parent=preCtx, ())
    let err:option<wrkCtxErr> = [
        () => parseVariables(wrkCtx, varsText),
        () => parseDisjoints(wrkCtx, disjText),
        () => hyps->Js.Array2.reduce(
            (err,stmt) => {
                switch err {
                    | Some(_) => err
                    | None => addStmtToCtx(wrkCtx, stmt)
                }
            },
            None
        )
    ]->Js.Array2.reduce(
        (err,nextStep) => {
            switch err {
                | Some(_) => err
                | None => nextStep()
            }
        },
        None
    )
    switch err {
        | Some(err) => Error(err)
        | None => Ok(wrkCtx)
    }
}

let rec updateWrkPrecalcData = (wrkPrecalcData):wrkPrecalcData => {
    switch wrkPrecalcData.preCtx {
        | None => {
            switch preCtxCache->cacheGetByDepVer(wrkPrecalcData.preCtxVer) {
                | None => wrkPrecalcData
                | Some((preCtx,wrkFrms)) => {
                    updateWrkPrecalcData(
                        {
                            ...wrkPrecalcData,
                            preCtx: Some(preCtx),
                            wrkFrms: Some(wrkFrms),
                        }
                    )
                }
            }
            
        }
        | Some(preCtx) => {
            switch createWrkCtx(
                ~preCtx,
                ~varsText=wrkPrecalcData.varsText,
                ~disjText=wrkPrecalcData.disjText,
                ~hyps=wrkPrecalcData.hyps,
            ) {
                | Error(_) => raise(MmException({msg:`There was an error creating wrkCtx in the worker thread.`}))
                | Ok(wrkCtx) => {
                    let parenInts = prepareParenInts(wrkCtx, wrkPrecalcData.parenStr)
                    {
                        ...wrkPrecalcData,
                        wrkParenInts: Some(parenInts),
                        wrkParenCnt: Some(parenCntMake(parenInts)),
                        wrkCtx: Some(wrkCtx),
                    }
                }
            }
        }
    }
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | PrepareWrkPrecalcData({ preCtxVer, parenStr, varsText, disjText, hyps, }) => {
            wrkPrecalcData.contents = createInitialWrkPrecalcData(
                ~preCtxVer, ~parenStr, ~varsText, ~disjText, ~hyps
            )
        }
        | SetPreCtx({ preCtxVer, preCtx }) => {
            preCtxCache->cacheGet(preCtxVer, preCtx)->ignore
        }
    }
    wrkPrecalcData.contents = updateWrkPrecalcData( wrkPrecalcData.contents )
    switch wrkPrecalcData.contents.preCtx {
        | None => sendToClient(GetPreCtx({preCtxVer: wrkPrecalcData.contents.preCtxVer}))
        | Some(_) => {
            switch wrkPrecalcData.contents.wrkCtx {
                | Some(_) => sendToClient(Ok)
                | None => raise(MmException({msg:`Could not create wrkCtx in the worker thread.`}))
            }
        }
    }
}

let beginWorkerInteractionUsingCtx = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    beginWorkerInteraction(
        ~procName = thisProcName,
        ~initialRequest = PrepareWrkPrecalcData({ preCtxVer, parenStr, varsText, disjText, hyps, }), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetPreCtx({preCtxVer:requestedPreCtxVer}) => {
                    if (requestedPreCtxVer == preCtxVer) {
                        sendToWorker(SetPreCtx({ preCtxVer, preCtx }))
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