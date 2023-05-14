open MM_context
open MM_wrk_ctx_data
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open Common

type mmCtxSrcDto = {
    typ: string,
    fileName: string,
    url: string,
    readInstr: string,
    label: string,
}

type preCtxData = {
    settingsV: version<settings>,
    srcs: array<mmCtxSrcDto>,
    ctxV: version<mmContext>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,
}

let preCtxDataMake = (~settings:settings):preCtxData => {
    {
        settingsV:versionMake(settings),

        srcs: [],
        ctxV: versionMake(createContext(())),
        frms: Belt_MapString.empty,
        parenCnt: parenCntMake([], ()),
        syntaxTypes:[],
    }
}

let findSyntaxTypes = (ctx:mmContext, frms: Belt_MapString.t<frmSubsData>): array<int> => {
    let syntaxTypes = Belt_HashSetInt.make(~hintSize=16)
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F) {
            syntaxTypes->Belt_HashSetInt.add(hyp.expr[0])
        }
        None
    })->ignore
    frms->Belt_MapString.forEach((_,frm) => {
        frm.frame.hyps->Js_array2.forEach(hyp => {
            if (hyp.typ == F) {
                syntaxTypes->Belt_HashSetInt.add(hyp.expr[0])
            }
        })
    })
    syntaxTypes->Belt_HashSetInt.toArray
}

let preCtxDataUpdate = (
    preCtxData:preCtxData,
    ~settings:option<settings>=?,
    ~ctx:option<(array<mmCtxSrcDto>,mmContext)>=?,
    ()
): preCtxData => {
    let settingsV = settings->Belt_Option.mapWithDefault(
        preCtxData.settingsV, 
        settings => preCtxData.settingsV->versionSet(settings)
    )
    let (srcs,ctxV) = ctx->Belt_Option.mapWithDefault(
        (preCtxData.srcs, preCtxData.ctxV),
        ((srcs,ctx)) => (srcs, preCtxData.ctxV->versionSet(ctx))
    )

    ctxV.val->moveConstsToBegin(settingsV.val.parens)
    let ctxV = ctxV->versionSet(ctxV.val)
    let frms = prepareFrmSubsData(
        ~ctx=ctxV.val, ~asrtsToSkip=settingsV.val.asrtsToSkip->Belt_HashSetString.fromArray, ()
    )
    let parenCnt = parenCntMake(prepareParenInts(ctxV.val, settingsV.val.parens), ~checkParensOptimized=true, ())
    let syntaxTypes = findSyntaxTypes(ctxV.val, frms)

    {
        settingsV,
        srcs,
        ctxV,
        frms,
        parenCnt,
        syntaxTypes,
    }
}
