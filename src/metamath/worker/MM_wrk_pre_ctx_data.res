open MM_context
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open Common
open MM_parser
open MM_wrk_ctx_data

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
    ctxMinV: version<mmContext>,
    frms: frms,
    parenCnt: parenCnt,
    allTypes:array<int>,
    syntaxTypes:array<int>,
    parensMap: Belt_HashMapString.t<string>,
    typeOrderInDisj:Belt_HashMapInt.t<int>,
}

let preCtxDataMake = (~settings:settings):preCtxData => {
    {
        settingsV:versionMake(settings),

        srcs: [],
        ctxV: versionMake(createContext(())),
        ctxMinV: versionMake(createContext(())),
        frms: frmsEmpty(),
        parenCnt: parenCntMake(~parenMin=0, ~canBeFirstMin=0, ~canBeFirstMax=0, ~canBeLastMin=0, ~canBeLastMax=0),
        allTypes:[],
        syntaxTypes:[],
        parensMap: Belt_HashMapString.make(~hintSize=0),
        typeOrderInDisj:Belt_HashMapInt.make(~hintSize=0),
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

    let ctxMinV = ctxV->versionSet(
        ctxV.val->ctxOptimizeForProver(~parens=settingsV.val.parens, ~removeAsrtDescr=true, ~removeProofs=true)
    )

    let frms = prepareFrmSubsData( ~ctx=ctxMinV.val )
    let parenCnt = MM_provers.makeParenCnt(~ctx=ctxMinV.val, ~parens=settingsV.val.parens)
    let (allTypes, syntaxTypes) = findTypes(ctxMinV.val)

    let parenInts = prepareParenInts(ctxMinV.val, settingsV.val.parens)
    let numOfParens = parenInts->Array.length / 2
    let parensMap = Belt_HashMapString.make(~hintSize=numOfParens)
    for i in 0 to numOfParens-1 {
        parensMap->Belt_HashMapString.set(
            ctxMinV.val->ctxIntToSymExn(parenInts->Array.getUnsafe(2*i)), 
            ctxMinV.val->ctxIntToSymExn(parenInts->Array.getUnsafe(2*i+1))
        )
    }

    let typeOrderInDisj = createTypeOrderFromStr(
        ~sortDisjByType=settingsV.val.sortDisjByType, 
        ~typeNameToInt=ctxSymToInt(ctxMinV.val, _)
    )

    {
        settingsV,
        srcs,
        ctxV,
        ctxMinV,
        frms,
        parenCnt,
        allTypes,
        syntaxTypes,
        parensMap,
        typeOrderInDisj,
    }
}
