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

type preCtx = {
    full:mmContext,
    min:mmContext,
}

type preCtxData = {
    settingsV: version<settings>,
    srcs: array<mmCtxSrcDto>,
    ctxV: version<preCtx>,
    frms: frms,
    parenCnt: parenCnt,
    allTypes:array<int>,
    syntaxTypes:array<int>,
    parensMap: Belt_HashMapString.t<string>,
    typeOrderInDisj:Belt_HashMapInt.t<int>,
    typeColors: Belt_HashMapString.t<string>,
    symColors: Belt_HashMapString.t<string>,
    mutable asrtSyntaxTrees: option<Belt_HashMapString.t<MM_syntax_tree.syntaxTreeNode>>,
}

let preCtxDataMake = (~settings:settings):preCtxData => {
    {
        settingsV:versionMake(settings),
        srcs: [],
        ctxV: versionMake({full:createContext(), min:createContext()}),
        frms: frmsEmpty(),
        parenCnt: parenCntMake(~parenMin=0, ~canBeFirstMin=0, ~canBeFirstMax=0, ~canBeLastMin=0, ~canBeLastMax=0),
        allTypes:[],
        syntaxTypes:[],
        parensMap: Belt_HashMapString.make(~hintSize=0),
        typeOrderInDisj:Belt_HashMapInt.make(~hintSize=0),
        typeColors: Belt_HashMapString.make(~hintSize=0),
        symColors: Belt_HashMapString.make(~hintSize=0),
        asrtSyntaxTrees: None,
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

let createSymbolColors = (~ctx:mmContext, ~typeColors: Belt_HashMapString.t<string>):Belt_HashMapString.t<string> => {
    let symbolColors = Belt_HashMapString.make(~hintSize=100)
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F) {
            switch ctx->ctxIntToSym(hyp.expr->Array.getUnsafe(0)) {
                | None => ()
                | Some(typeStr) => {
                    switch typeColors->Belt_HashMapString.get(typeStr) {
                        | None => ()
                        | Some(color) => {
                            symbolColors->Belt_HashMapString.set(
                                ctx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(1)),
                                color
                            )
                        }
                    }
                }
            }
        }
        None
    })->ignore
    symbolColors
}

let preCtxDataUpdate = (
    preCtxData:preCtxData,
    ~settings:option<settings>=?,
    ~ctx:option<(array<mmCtxSrcDto>,mmContext)>=?
): preCtxData => {
    let settingsV = settings->Option.mapOr(
        preCtxData.settingsV, 
        settings => preCtxData.settingsV->versionSet(settings)
    )
    let (srcs,ctxV) = ctx->Option.mapOr(
        (preCtxData.srcs, preCtxData.ctxV),
        ((srcs,ctx)) => (srcs, preCtxData.ctxV->versionSet({full:ctx,min:ctx}))
    )

    let ctxFull = ctxV.val.full->ctxOptimizeForProver(
        ~parens=settingsV.val.parens, ~removeAsrtDescr=false, ~removeProofs=false
    )

    let ctxMin = ctxFull->ctxOptimizeForProver(
        ~parens=settingsV.val.parens, ~removeAsrtDescr=true, ~removeProofs=true
    )

    let ctxV = ctxV->versionSet({full:ctxFull, min:ctxMin})

    let ctxMin = ctxV.val.min
    let frms = prepareFrmSubsData( ~ctx=ctxMin )
    let parenCnt = MM_provers.makeParenCnt(~ctx=ctxMin, ~parens=settingsV.val.parens)
    let (allTypes, syntaxTypes) = findTypes(ctxMin)

    let parenInts = prepareParenInts(ctxMin, settingsV.val.parens)
    let numOfParens = parenInts->Array.length / 2
    let parensMap = Belt_HashMapString.make(~hintSize=numOfParens)
    for i in 0 to numOfParens-1 {
        parensMap->Belt_HashMapString.set(
            ctxMin->ctxIntToSymExn(parenInts->Array.getUnsafe(2*i)), 
            ctxMin->ctxIntToSymExn(parenInts->Array.getUnsafe(2*i+1))
        )
    }

    let typeOrderInDisj = createTypeOrderFromStr(
        ~sortDisjByType=settingsV.val.sortDisjByType, 
        ~typeNameToInt=ctxSymToInt(ctxMin, _)
    )

    let typeColors = settingsV.val->settingsGetTypeColors
    let symColors = createSymbolColors(~ctx=ctxMin, ~typeColors=typeColors)

    {
        settingsV,
        srcs,
        ctxV,
        frms,
        parenCnt,
        allTypes,
        syntaxTypes,
        parensMap,
        typeOrderInDisj,
        typeColors,
        symColors,
        asrtSyntaxTrees: None,
    }
}
