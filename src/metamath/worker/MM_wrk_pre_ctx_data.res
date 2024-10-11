open MM_context
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open Common
open MM_parser

type mmCtxSrcDto = {
    typ: string,
    fileName: string,
    url: string,
    readInstr: string,
    label: string,
    resetNestingLevel:bool,
    ast: option<mmAstNode>,
    allLabels: array<string>,
}

type preCtxData = {
    settingsV: version<settings>,
    srcs: array<mmCtxSrcDto>,
    ctxV: version<mmContext>,
    frms: frms,
    parenCnt: parenCnt,
    allTypes:array<int>,
    syntaxTypes:array<int>,
}

let preCtxDataMake = (~settings:settings):preCtxData => {
    {
        settingsV:versionMake(settings),

        srcs: [],
        ctxV: versionMake(createContext(())),
        frms: frmsEmpty(),
        parenCnt: parenCntMake(~parenMin=0, ~canBeFirstMin=0, ~canBeFirstMax=0, ~canBeLastMin=0, ~canBeLastMax=0),
        allTypes:[],
        syntaxTypes:[],
    }
}

let findTypes = (ctx:mmContext): (array<int>,array<int>) => {
    let syntaxTypes = Belt_HashSetInt.make(~hintSize=16)
    let allTypes = Belt_HashSetInt.make(~hintSize=16)
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        let typ = hyp.expr->Array.getUnsafe(0)
        allTypes->Belt_HashSetInt.add(typ)
        if (hyp.typ == F) {
            syntaxTypes->Belt_HashSetInt.add(typ)
        }
        None
    })->ignore
    ctx->forEachFrame(frame => {
        allTypes->Belt_HashSetInt.add(frame.asrt->Array.getUnsafe(0))
        frame.hyps->Array.forEach(hyp => {
            let typ = hyp.expr->Array.getUnsafe(0)
            allTypes->Belt_HashSetInt.add(typ)
            if (hyp.typ == F) {
                syntaxTypes->Belt_HashSetInt.add(typ)
            }
        })
        None
    })->ignore
    (allTypes->Belt_HashSetInt.toArray, syntaxTypes->Belt_HashSetInt.toArray)
}

let preCtxDataUpdate = (
    preCtxData:preCtxData,
    ~settings:option<settings>=?,
    ~ctx:option<(array<mmCtxSrcDto>,mmContext)>=?
): preCtxData => {
    let settingsV = settings->Belt_Option.mapWithDefault(
        preCtxData.settingsV, 
        settings => preCtxData.settingsV->versionSet(settings)
    )
    let (srcs,ctxV) = ctx->Belt_Option.mapWithDefault(
        (preCtxData.srcs, preCtxData.ctxV),
        ((srcs,ctx)) => (srcs, preCtxData.ctxV->versionSet(ctx))
    )

    let ctxV = ctxV->versionSet(
        ctxV.val->ctxOptimizeForProver(~parens=settingsV.val.parens, ~removeAsrtDescr=false, ~removeProofs=false)
    )
    let frms = prepareFrmSubsData(
        ~ctx=ctxV.val
    )
    let parenCnt = MM_provers.makeParenCnt(~ctx=ctxV.val, ~parens=settingsV.val.parens)
    let (allTypes, syntaxTypes) = findTypes(ctxV.val)

    {
        settingsV,
        srcs,
        ctxV,
        frms,
        parenCnt,
        allTypes,
        syntaxTypes,
    }
}
