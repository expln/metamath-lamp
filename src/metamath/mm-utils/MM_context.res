open MM_parser
open MM_progress_tracker
open MM_wrk_settings
open Common

type expr = array<int>

let exprEq: (expr,expr) => bool = (a,b) => {
    let len1 = a->Js_array2.length
    let len2 = b->Js_array2.length
    if (len1 != len2) {
        false
    } else {
        let eq = ref(true)
        let i = ref(0)
        while (eq.contents && i.contents < len1) {
            eq.contents = a[i.contents] == b[i.contents]
            i.contents = i.contents + 1
        }
        eq.contents
    }
}

let exprCmp = (e1,e2) => {
    let len1 = e1->Js_array2.length
    let len2 = e2->Js_array2.length
    switch Expln_utils_common.intCmp(len1, len2) {
        | 0 => {
            let res = ref(0)
            let i = ref(0)
            while (i.contents < len1 && res.contents == 0) {
                res.contents = Expln_utils_common.intCmp(e1[i.contents], e2[i.contents])
                i.contents = i.contents + 1
            }
            res.contents
        }
        | r => r
    }
}

module ExprCmp = Belt.Id.MakeComparable({
    type t = expr
    let cmp = exprCmp
})

module ExprHash = Belt.Id.MakeHashable({
    type t = expr
    let hash = Expln_utils_common.hashArrInt
    let eq = exprEq
})

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

type exprToHyp = Belt_HashMapInt.t<Belt_HashMapInt.t<Belt_HashMapInt.t<array<hypothesis>>>>

type frameDbg = {
    disj: array<string>,
    hyps: array<string>,
    asrt: string,
}

type frame = {
    ord:int,
    isAxiom:bool,
    disj: Belt_MapInt.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    frameVarToSymb: array<string>,
    varTypes: array<int>,
    varHyps: array<int>,
    numOfVars: int,
    numOfArgs: int,
    descr:option<string>,
    proof:option<proof>,
    isDisc:bool, /* is discouraged */
    isDepr:bool, /* is deprecated */
    isTranDepr:bool, /* is transitively deprecated (depends on an isDepr frame or another isTranDepr frame) */
    dbg: option<frameDbg>,
}

type disjMutable = Belt_HashMapInt.t<Belt_HashSetInt.t>

type rec mmContextContents = {
    mutable root: option<mmContextContents>,
    parent: option<mmContextContents>,
    consts: array<string>,
    varsBaseIdx: int,
    vars: array<string>,
    symToInt: Belt_HashMapString.t<int>,
    disj: disjMutable,
    hyps: array<hypothesis>,
    labelToHyp: Belt_HashMapString.t<hypothesis>,
    exprToHyp: exprToHyp,
    varTypes: Belt_HashMapInt.t<int>,
    mutable lastComment: option<string>,
    frames: Belt_HashMapString.t<frame>,
    mutable totalNumOfFrames:int,
    deprOrTranDeprFrms:Belt_HashSetString.t,
    debug:bool,
}

type mmContext = ref<mmContextContents>

type optimizedConstsOrder = {
    allConsts:array<string>,
    parenMin:int,
    canBeFirstMin:int,
    canBeFirstMax:int,
    canBeLastMin:int,
    canBeLastMax:int,
}

let disjAddPair = (disjMap:disjMutable, n, m) => {
    if (n != m) {
        let min = if (n <= m) {n} else {m}
        let max = if (n <= m) {m} else {n}

        switch disjMap->Belt_HashMapInt.get(min) {
            | None => disjMap->Belt_HashMapInt.set(min, Belt_HashSetInt.fromArray([max]))
            | Some(set) => set->Belt_HashSetInt.add(max)
        }
    }
}

let disjMutToDisjImm = (disj:disjMutable):Belt_MapInt.t<Belt_SetInt.t> => {
    disj->Belt_HashMapInt.toArray
        ->Js.Array2.map(((n,ms)) => (n,ms->Belt_HashSetInt.toArray->Belt_SetInt.fromArray))
        ->Belt_MapInt.fromArray
}

let rec forEachCtxInDeclarationOrder = (ctx:mmContextContents,consumer:mmContextContents=>option<'a>):option<'a> => {
    switch ctx.parent {
        | Some(parent) => {
            switch forEachCtxInDeclarationOrder(parent, consumer) {
                | Some(res) => Some(res)
                | None => consumer(ctx)
            }
        }
        | None => consumer(ctx)
    }
}

let rec forEachCtxInReverseOrder = (ctx:mmContextContents,consumer:mmContextContents=>option<'a>):option<'a> => {
    switch consumer(ctx) {
        | Some(res) => Some(res)
        | None => {
            switch ctx.parent {
                | Some(parent) => forEachCtxInReverseOrder(parent, consumer)
                | None => None
            }
        }
    }
}

let isDebug = ctx => ctx.contents.debug

type tokenType = C | V | F | E | A | P

let getTokenType = (ctx:mmContext, token:string):option<tokenType> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        switch ctx.frames->Belt_HashMapString.get(token) {
            | Some(frame) => if (frame.proof->Belt_Option.isNone) {Some(A)} else {Some(P)}
            | None => {
                switch ctx.labelToHyp->Belt_HashMapString.get(token) {
                    | Some(hyp) => if (hyp.typ == F) {Some(F)} else {Some(E)}
                    | None => {
                        switch ctx.symToInt->Belt_HashMapString.get(token) {
                            | Some(i) => if (i < 0) {Some(C)} else {Some(V)}
                            | None => None
                        }
                    }
                }
            }
        }
    })
}

let isConst = (ctx:mmContext, sym:string):bool => {
    (ctx.contents.root->Belt.Option.getExn).symToInt
        ->Belt_HashMapString.get(sym)
        ->Belt_Option.map(i => i < 0)
        ->Belt_Option.getWithDefault(false)
}

let isVar = (ctx:mmContext, sym:string) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->Belt_HashMapString.get(sym)
    })
        ->Belt_Option.map(i => 0 <= i)
        ->Belt_Option.getWithDefault(false)
}

let isHyp = (ctx:mmContext, label:string) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.labelToHyp->Belt_HashMapString.get(label)
    })->Belt_Option.isSome
}

let isAsrt = (ctx:mmContext, label:string) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->Belt_HashMapString.get(label)
    })->Belt_Option.isSome
}

let disjContains = (disj:disjMutable, n, m):bool => {
    let min = if (n <= m) {n} else {m}
    let max = if (n <= m) {m} else {n}
    switch disj->Belt_HashMapInt.get(min) {
        | None => false
        | Some(ms) => ms->Belt_HashSetInt.has(max)
    }
}

let disjImmContains = (disj:Belt_MapInt.t<Belt_SetInt.t>, n, m):bool => {
    let min = if (n <= m) {n} else {m}
    let max = if (n <= m) {m} else {n}
    switch disj->Belt_MapInt.get(min) {
        | None => false
        | Some(ms) => ms->Belt_SetInt.has(max)
    }
}

let disjForEach = (disjMutable, consumer) => {
    disjMutable->Belt_HashMapInt.forEach((n,ms) => {
        ms->Belt_HashSetInt.forEach(m => {
            consumer(n,m)
        })
    })
}

let disjGetAllVars = (disj:disjMutable):Belt_HashSetInt.t => {
    let res = Belt_HashSetInt.make(~hintSize=100)
    disj->Belt_HashMapInt.forEach((n,ms) => {
        res->Belt_HashSetInt.add(n)
        ms->Belt_HashSetInt.forEach(m => {
            res->Belt_HashSetInt.add(m)
        })
    })
    res
}

let disjToArr = (
    disj:disjMutable, 
    ~sortByTypeAndName:bool=false,
    ~varIntToVarName:option<int=>option<string>>=?,
    ~varIntToVarType:option<int=>option<int>>=?,
    ~typeOrder:option<Belt_HashMapInt.t<int>>=?,
    ()
):array<array<int>> => {
    let res = []
    disj->disjForEach((n,m) => res->Js_array2.push([n,m])->ignore)

    let canMerge = (d1:array<int>,d2:array<int>):bool => {
        let canMerge = ref(true)
        d1->Js_array2.forEach(v1 => {
            if (canMerge.contents) {
                d2->Js_array2.forEach(v2 => {
                    if (canMerge.contents && v1 != v2) {
                        canMerge.contents = disj->disjContains(v1,v2)
                    }
                })
            }
        })
        canMerge.contents
    }

    let merge = (d1:array<int>,d2:array<int>):unit => {
        res->Js_array2.removeCountInPlace(
            ~pos=res->Js_array2.findIndex(d => d->exprEq(d2)),
            ~count=1
        )->ignore
        d2->Js_array2.forEach(v2 => {
            if (!(d1->Js_array2.includes(v2))) {
                d1->Js_array2.push(v2)->ignore  
            }
        })
    }

    let findWhatToMerge = ():option<(array<int>,array<int>)> => {
        let found = ref(None)
        for i in 0 to res->Js_array2.length-2 {
            if (found.contents->Belt_Option.isNone) {
                for j in i+1 to res->Js_array2.length-1 {
                    if (found.contents->Belt_Option.isNone) {
                        let d1 = res[i]
                        let d2 = res[j]
                        if (canMerge(d1,d2)) {
                            found.contents = Some((d1,d2))
                        }
                    }
                }
            }
        }
        found.contents
    }

    let mergeFound = ref(true)
    while (mergeFound.contents) {
        switch findWhatToMerge() {
            | None => mergeFound.contents = false
            | Some((d1,d2)) => merge(d1,d2)
        }
    }

    let sortBy = if (sortByTypeAndName) {
        switch varIntToVarType {
            | None => Expln_utils_common.intCmp
            | Some(varIntToVarType) => {
                switch typeOrder {
                    | None => Expln_utils_common.intCmp
                    | Some(typeOrder) => {
                        switch varIntToVarName {
                            | None => Expln_utils_common.intCmp
                            | Some(varIntToVarName) => {
                                let allDisjVars = disj->disjGetAllVars
                                let allDisjVarTypes = Belt_HashMapInt.make(~hintSize=allDisjVars->Belt_HashSetInt.size)
                                let allDisjVarNames = Belt_HashMapInt.make(~hintSize=allDisjVars->Belt_HashSetInt.size)
                                allDisjVars->Belt_HashSetInt.forEach(i => {
                                    switch varIntToVarType(i) {
                                        | Some(typ) => allDisjVarTypes->Belt_HashMapInt.set(i,typ)
                                        | None => ()
                                    }
                                    switch varIntToVarName(i) {
                                        | Some(str) => allDisjVarNames->Belt_HashMapInt.set(i,str)
                                        | None => ()
                                    }
                                })
                                let varTypeCmp = createVarTypeComparator( 
                                    ~varTypes=allDisjVarTypes, 
                                    ~typeOrder, 
                                )
                                let varNameCmp = createVarNameComparator(allDisjVarNames)
                                varTypeCmp->Expln_utils_common.comparatorAndThen(varNameCmp)
                            }
                        }
                    }
                }
            }
        }
    } else {
        Expln_utils_common.intCmp
    }
    res->Js.Array2.forEach(d =>
        d->Js_array2.sortInPlaceWith(sortBy)->ignore
    )
    res->Js_array2.sortInPlaceWith(exprCmp)
}

let disjForEachArr = (
    disj:disjMutable, 
    ~sortByTypeAndName:bool=false,
    ~varIntToVarName:option<int=>option<string>>=?,
    ~varIntToVarType:option<int=>option<int>>=?,
    ~typeOrder:option<Belt_HashMapInt.t<int>>=?,
    consumer:array<int> => unit
) => {
    disj->disjToArr(
        ~sortByTypeAndName,
        ~varIntToVarName?,
        ~varIntToVarType?,
        ~typeOrder?,
        ()
    )->Js_array2.forEach(consumer)
}

let disjIsEmpty = disjMutable => {
    disjMutable->Belt_HashMapInt.size == 0
}

let isDisj = (ctx,n,m) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        if (ctx.disj->disjContains(n,m)) {
            Some(true)
        } else {
            None
        }
    })->Belt_Option.getWithDefault(false)
}

let getHypothesis = (ctx:mmContext,label):option<hypothesis> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.labelToHyp->Belt_HashMapString.get(label)
    })
}

let exprToHypAdd = (ctx:mmContextContents, hyp:hypothesis):unit => {
    let expr = hyp.expr
    let len = expr->Js_array2.length
    let fstSym = if len > 0 {expr[0]} else {0}
    let sndSym = if len > 1 {expr[1]} else {0}
    switch ctx.exprToHyp->Belt_HashMapInt.get(len) {
        | None => {
            ctx.exprToHyp->Belt_HashMapInt.set( 
                len, 
                Belt_HashMapInt.fromArray([(fstSym, Belt_HashMapInt.fromArray([(sndSym, [hyp])]))])
            )
        }
        | Some(fstSymToHyp) => {
            switch fstSymToHyp->Belt_HashMapInt.get(fstSym) {
                | None => fstSymToHyp->Belt_HashMapInt.set( fstSym, Belt_HashMapInt.fromArray([(sndSym, [hyp])]) )
                | Some(sndSymToHyp) => {
                    switch sndSymToHyp->Belt_HashMapInt.get(sndSym) {
                        | None => sndSymToHyp->Belt_HashMapInt.set( sndSym, [hyp] )
                        | Some(hypsArr) => {
                            switch hypsArr->Js_array2.find(h => h.expr->exprEq(hyp.expr)) {
                                | Some(_) => ()
                                | None => hypsArr->Js_array2.push(hyp)->ignore
                            }
                        }
                    }
                }
            }
        }
    }
}

let exprToHypGet = (ctx:mmContextContents, expr:expr):option<hypothesis> => {
    let len = expr->Js_array2.length
    switch ctx.exprToHyp->Belt_HashMapInt.get(len) {
        | None => None
        | Some(fstSymToHyp) => {
            let fstSym = if len > 0 {expr[0]} else {0}
            switch fstSymToHyp->Belt_HashMapInt.get(fstSym) {
                | None => None
                | Some(sndSymToHyp) => {
                    let sndSym = if len > 1 {expr[1]} else {0}
                    switch sndSymToHyp->Belt_HashMapInt.get(sndSym) {
                        | None => None
                        | Some(hypsArr) => hypsArr->Js_array2.find(h => h.expr->exprEq(expr))
                    }
                }
            }
        }
    }
}

let getHypByExpr = (ctx:mmContext, expr:expr) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx->exprToHypGet(expr)
    })
}

let getFrame = (ctx:mmContext,label):option<frame> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->Belt_HashMapString.get(label)
    })
}

let getFrameExn = (ctx:mmContext,label):frame => {
    switch ctx->getFrame(label) {
        | None => raise(MmException({msg: `Could not find a frame by the label '${label}'`}))
        | Some(frame) => frame
    }
}

let frameIsAllowed = (frame:frame, frameRestrict:frameRestrict):bool => {
    (!frame.isDisc || frameRestrict.useDisc) 
    && (!frame.isDepr || frameRestrict.useDepr) 
    && (!frame.isTranDepr || frameRestrict.useTranDepr)
}

let getLocalVars: mmContext => array<string> = ctx => {
    ctx.contents.vars->Js_array2.copy
}

let getLocalHyps: mmContext => array<hypothesis> = ctx => {
    ctx.contents.hyps->Js_array2.copy
}

let getNumOfVars = ctx => {
    ctx.contents.varsBaseIdx + ctx.contents.vars->Js_array2.length
}

let forEachHypothesisInDeclarationOrder = (ctx:mmContext, consumer:hypothesis=>option<'a>):option<'a> => {
    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
        Expln_utils_common.arrForEach(ctx.hyps, consumer)
    })
}

let ctxSymToInt = (ctx:mmContext, sym:string):option<int> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->Belt_HashMapString.get(sym)
    })
}

let ctxSymToIntExn = (ctx:mmContext,sym) => {
    switch ctxSymToInt(ctx,sym) {
        | Some(i) => i
        | None => raise(MmException({msg:`The symbol '${sym}' is not declared.`}))
    }
}

let ctxSymsToIntsExn = (ctx:mmContext, symbols:array<string>):expr => {
    symbols->Js_array2.map(ctx->ctxSymToIntExn)
}

let ctxStrToIntsExn = (ctx, str) => ctxSymsToIntsExn(ctx, str->getSpaceSeparatedValuesAsArray)

let ctxIntToSym = (ctx:mmContext,i:int):option<string> => {
    if (i < 0) {
        (ctx.contents.root->Belt.Option.getExn).consts->Belt_Array.get(-i)
    } else {
        ctx.contents->forEachCtxInReverseOrder(ctx => {
            if (i < ctx.varsBaseIdx) {
                None
            } else {
                Some(ctx.vars[i-ctx.varsBaseIdx])
            }
        })
    }
}

let ctxIntToSymExn = (ctx:mmContext,i:int):string => {
    switch ctxIntToSym(ctx,i) {
        | Some(str) => str
        | None => raise(MmException({msg:`Cannot convert ${i->Belt_Int.toString} to a symbol.`}))
    }
}

let ctxIntsToSymsExn = (ctx,expr) => expr->Js_array2.map(ctxIntToSymExn(ctx, _))

let ctxIntsToStrExn = (ctx:mmContext, expr:expr):string => {
    expr->Js_array2.map(ctxIntToSymExn(ctx, _))->Js_array2.joinWith(" ")
}

let frmIntToSym = (ctx:mmContext, frame:frame, i:int):option<string> => {
    if (i < 0) {ctx->ctxIntToSym(i)} else {frame.frameVarToSymb->Belt_Array.get(i)}
}

let frmIntToSymExn = (ctx:mmContext, frame:frame, i:int):string => {
    switch frmIntToSym(ctx, frame, i) {
        | None => raise(MmException({msg:`Could not convert from int ${i->Belt.Int.toString} to a symbol.`}))
        | Some(sym) => sym
    }
}

let frmIntsToSymsExn = (ctx:mmContext, frame:frame, expr:expr):array<string> => {
    expr->Js_array2.map(frmIntToSymExn(ctx, frame, _))
}

let frmIntsToStrExn = (ctx:mmContext, frame:frame, expr:expr):string => {
    frmIntsToSymsExn(ctx, frame, expr)->Js_array2.joinWith(" ")
}

let getTypeOfVar = (ctx:mmContext, varInt:int):option<int> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.varTypes->Belt_HashMapInt.get(varInt)
    })
}

let getTypeOfVarExn = (ctx:mmContext, varInt:int):int => {
    switch ctx->getTypeOfVar(varInt) {
        | None => {
            let varName = switch ctx->ctxIntToSym(varInt) {
                | None => varInt->Belt_Int.toString
                | Some(sym) => `'${sym}'`
            }
            raise(MmException({msg:`Cannot determine type of the variable ${varName}`}))
        }
        | Some(typ) => typ
    }
}

let extractMandatoryVariables = (
    ctx:mmContext, 
    asrt:expr, 
    ~skipEssentials:bool=false, 
    ~overrideHyps:option<array<expr>>=?,
    ()
):Belt_HashSetInt.t => {
    let res = Belt_HashSetInt.make(~hintSize=16)
    if (!skipEssentials) {
        switch overrideHyps {
            | Some(overrideHyps) => {
                overrideHyps->Js_array2.forEach(hypExpr => {
                    hypExpr->Js_array2.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
                })
            }
            | None => {
                ctx->forEachHypothesisInDeclarationOrder(hyp => {
                    if (hyp.typ == E) {
                        hyp.expr->Js_array2.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
                    }
                    None
                })->ignore
            }
        }
    }
    asrt->Js_array2.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
    res
}

let extractMandatoryDisj = (ctx:mmContext, mandatoryVars:Belt_HashSetInt.t): disjMutable => {
    let mandatoryDisj = Belt_HashMapInt.make(~hintSize=16)
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.disj->Belt_HashMapInt.forEach((n,ms) => {
            if (mandatoryVars->Belt_HashSetInt.has(n)) {
                ms->Belt_HashSetInt.forEach(m => {
                    if (mandatoryVars->Belt_HashSetInt.has(m)) {
                        disjAddPair(mandatoryDisj, n, m)
                    }
                })
            }
        })
        None
    })->ignore
    mandatoryDisj
}

let extractMandatoryHypotheses = (
    ctx:mmContext, 
    mandatoryVars:Belt_HashSetInt.t, 
    ~skipEssentials:bool=false, 
    ~overrideHyps:option<array<expr>>=?,
    ()
):array<hypothesis> => {
    let res = []
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (
            hyp.typ == E && (!skipEssentials && overrideHyps->Belt_Option.isNone)
            || hyp.typ == F && mandatoryVars->Belt_HashSetInt.has(hyp.expr[1])
        ) {
            res->Js.Array2.push(hyp)->ignore
        }
        None
    })->ignore
    switch overrideHyps {
        | None => ()
        | Some(overrideHyps) => {
            overrideHyps->Js_array2.forEachi((hypExpr,i) => {
                res->Js.Array2.push({
                    typ: E,
                    label: i->Belt.Int.toString,
                    expr: hypExpr
                })->ignore
            })
        }
    }
    res
}

let getAllConsts = (ctx:mmContext):array<string> => {
    Belt_Option.getExn(ctx.contents.root).consts->Js_array2.sliceFrom(1)
}

let getMandHyps = (
    ctx:mmContext, 
    expr:expr,
    ~skipEssentials:bool=false, 
    ~overrideHyps:option<array<expr>>=?,
    ()
):array<hypothesis> => {
    let mandatoryVars = extractMandatoryVariables( ctx, expr, ~skipEssentials, ~overrideHyps?, () )
    extractMandatoryHypotheses(ctx, mandatoryVars, ~skipEssentials, ~overrideHyps?, ())
}

let getAllHyps = (ctx:mmContext):Belt_MapString.t<hypothesis> => {
    let hyps = []
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        hyps->Js.Array2.push(hyp)->ignore
        None
    })->ignore
    Belt_MapString.fromArray(hyps->Js_array2.map(hyp => (hyp.label, hyp)))
}

let getAllFramesArr = (ctx:mmContext):array<frame> => {
    let frames = []
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        frames->Js.Array2.pushMany(ctx.frames->Belt_HashMapString.valuesToArray)->ignore
        None
    })->ignore
    frames
}

let getAllFrames = (ctx:mmContext):Belt_MapString.t<frame> => {
    let frames = []
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->Belt_HashMapString.forEach((k,v) => frames->Js.Array2.push((k,v))->ignore)
        None
    })->ignore
    Belt_MapString.fromArray(frames)
}

let forEachFrame = (ctx:mmContext, consumer:frame => option<'a>):option<'a> => {
    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
        let result = ref(None)
        ctx.frames->Belt_HashMapString.forEach((_,frm) => {
            if (result.contents->Belt_Option.isNone) {
                result.contents = consumer(frm)
            }
        })
        result.contents
    })
}

let rec getNestingLevelPriv = (ctx:mmContextContents):int => {
    switch ctx.parent {
        | None => 0
        | Some(pCtx) => 1 + getNestingLevelPriv(pCtx)
    }
}

let getNestingLevel = (ctx:mmContext):int => getNestingLevelPriv(ctx.contents)

let findParentheses = (ctx:mmContext, ~onProgress:option<float=>unit>=?, ()):array<int> => {

    let getAllExprs = ctx => {
        let allExpr = []
        ctx->forEachFrame(frame => {
            frame.hyps->Js_array2.forEach(hyp => {
                if (hyp.typ == E) {
                    allExpr->Js_array2.push(hyp.expr)->ignore
                }
            })
            allExpr->Js_array2.push(frame.asrt)->ignore
            None
        })->ignore
        allExpr
    }

    let checkValidParens = (allExprs, openSym, closeSym):bool => {
        open MM_parenCounter_unoptimized
        let res = ref(true)
        let openUsed = ref(false)
        let closeUsed = ref(false)
        let parenCnt = parenCntMake([openSym, closeSym], ~checkParensOptimized=false, ())
        let parenState = ref(Balanced)
        let allExprsLen = allExprs->Js_array2.length
        let e = ref(0)
        while (e.contents < allExprsLen && res.contents) {
            let expr = allExprs[e.contents]
            let exprLen = expr->Js_array2.length
            let s = ref(0)
            while (s.contents < exprLen && res.contents) {
                let sym = expr[s.contents]
                if (!openUsed.contents && sym == openSym) {
                    openUsed.contents = true
                }
                if (!closeUsed.contents && sym == closeSym) {
                    closeUsed.contents = true
                }
                parenState.contents = parenCnt->parenCntPut(sym)
                res.contents = parenState.contents != Failed
                s.contents = s.contents + 1
            }
            res.contents = parenState.contents == Balanced
            e.contents = e.contents + 1
        }
        res.contents && openUsed.contents && closeUsed.contents
    }

    let allExprs = getAllExprs(ctx)
    let allConsts = "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( ))"
        ->getSpaceSeparatedValuesAsArray
        ->Js.Array2.filter(ctx->isConst)
        ->ctxSymsToIntsExn(ctx, _)
        ->Js_array2.concat(
            Belt_Array.range(
                1,
                (ctx.contents.root->Belt.Option.getExn).consts->Js.Array2.length - 1
            )->Js_array2.map(i => -i)
        )

    let maxC = allConsts->Js.Array2.length - 2
    let maxCF = maxC->Belt_Int.toFloat
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?, ())
    let foundParens = []
    for c in 0 to maxC {
        let openParen = allConsts[c]
        let closeParen = allConsts[c+1]
        if (!(foundParens->Js.Array2.includes(openParen))
            && !(foundParens->Js.Array2.includes(closeParen))
            && checkValidParens(allExprs, openParen, closeParen)
        ) {
            foundParens->Js_array2.push(openParen)->ignore
            foundParens->Js_array2.push(closeParen)->ignore
        }
        progressState->progressTrackerSetCurrPct(
            c->Belt_Int.toFloat /. maxCF
        )
    }
    foundParens
}

let disjMake = () => {
    Belt_HashMapInt.make(~hintSize=16)
}

let getAllDisj = (ctx:mmContext):disjMutable => {
    let disj = disjMake()
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.disj->Belt_HashMapInt.forEach((n,ms) => {
            ms->Belt_HashSetInt.forEach(m => {
                disj->disjAddPair(n,m)
            })
        })
        None
    })->ignore
    disj
}

let getLocalDisj = (ctx:mmContext):disjMutable => {
    let disj = disjMake()
    ctx.contents.disj->Belt_HashMapInt.forEach((n,ms) => {
        ms->Belt_HashSetInt.forEach(m => {
            disj->disjAddPair(n,m)
        })
    })
    disj
}

let createContext = (~parent:option<mmContext>=?, ~debug:bool=false, ()):mmContext => {
    let pCtxContentsOpt = switch parent {
        | Some(pCtx) => Some(pCtx.contents)
        | None => None
    }
    let ctx = ref(
        {
            root: None,
            parent: pCtxContentsOpt,
            consts: switch pCtxContentsOpt {
                | None => [""]
                | Some(_) => []
            },
            varsBaseIdx: switch pCtxContentsOpt {
                | None => 0
                | Some(parent) => parent.varsBaseIdx + parent.vars->Js_array2.length
            },
            vars: [],
            symToInt: Belt_HashMapString.make(~hintSize=0),
            disj: disjMake(),
            hyps: [],
            labelToHyp: Belt_HashMapString.make(~hintSize=4),
            exprToHyp: Belt_HashMapInt.make(~hintSize=4),
            varTypes: Belt_HashMapInt.make(~hintSize=4),
            lastComment: None,
            frames: Belt_HashMapString.make(~hintSize=1),
            totalNumOfFrames: switch pCtxContentsOpt {
                | None => 0
                | Some(parentCtx) => parentCtx.totalNumOfFrames
            },
            deprOrTranDeprFrms: Belt_HashSetString.make(~hintSize=1),
            debug: pCtxContentsOpt->Belt_Option.map(pCtx => pCtx.debug)->Belt.Option.getWithDefault(debug),
        }
    )
    switch pCtxContentsOpt {
        | None => ctx.contents.root = Some(ctx.contents)
        | Some(pCtxContents) => ctx.contents.root = pCtxContents.root
    }
    ctx
}

let openChildContext = (ctx:mmContext):unit => {
    ctx.contents = createContext(~parent=ctx, ()).contents
}

let closeChildContext = (ctx:mmContext):unit => {
    let childCtx = ctx.contents
    ctx.contents = switch childCtx.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            parent.totalNumOfFrames = parent.totalNumOfFrames + childCtx.frames->Belt_HashMapString.size
            childCtx.frames->Belt_HashMapString.forEach((k,v) => parent.frames->Belt_HashMapString.set(k,v))
            childCtx.deprOrTranDeprFrms->Belt_HashSetString.forEach(parent.deprOrTranDeprFrms->Belt_HashSetString.add)
            parent
        }
    }
}

let resetToParentContext = (ctx:mmContext):unit => {
    ctx.contents = switch ctx.contents.parent {
        | None => raise(MmException({msg:`Cannot reset the root context.`}))
        | Some(parent) => parent
    }
}

let addComment = (ctx:mmContext,str:string):unit => {
    ctx.contents.lastComment = Some(str)
}

let assertNameIsUnique = (ctx:mmContext,name:string,tokenType:string):unit => {
    if (name->Js_string2.trim == "") {
        raise(MmException({msg:`Cannot use an empty string as a name of ${tokenType}.`}))
    } else {
        switch getTokenType(ctx,name) {
            | Some(C) => raise(MmException({msg:`An attempt to re-declare the constant '${name}' as ${tokenType}.`}))
            | Some(V) => raise(MmException({msg:`An attempt to re-declare the variable '${name}' as ${tokenType}.`}))
            | Some(F) => raise(MmException({msg:`An attempt to re-declare a floating label '${name}' as ${tokenType}.`}))
            | Some(E) => raise(MmException({msg:`An attempt to re-declare an essential label '${name}' as ${tokenType}.`}))
            | Some(A) => raise(MmException({msg:`An attempt to re-declare an axiom label '${name}' as ${tokenType}.`}))
            | Some(P) => raise(MmException({msg:`An attempt to re-declare a provable label '${name}' as ${tokenType}.`}))
            | None => ()
        }
    }
}

let addConst = (ctx:mmContext,name:string):unit => {
    if (ctx.contents.parent->Belt_Option.isSome) {
        raise(MmException({msg:`An attempt to declare a constant '${name}' in an inner block.`}))
    } else {
        assertNameIsUnique(ctx,name,"a constant")
        let ctx = ctx.contents
        ctx.symToInt->Belt_HashMapString.set(name, -(ctx.consts->Js_array2.length))
        ctx.consts->Js_array2.push(name)->ignore
    }
}

let addVar = (ctx:mmContext,name:string):unit => {
    assertNameIsUnique(ctx,name,"a variable")
    let ctx = ctx.contents
    ctx.symToInt->Belt_HashMapString.set(name, ctx.varsBaseIdx + ctx.vars->Js_array2.length)
    ctx.vars->Js_array2.push(name)->ignore
}

let addDisj = (ctx:mmContext, vars:array<string>):unit => {
    switch vars->Js_array2.find(sym => !(ctx->isVar(sym))) {
        | Some(sym) => 
            raise(MmException({msg:`The symbol '${sym}' is not a variable but it is used in a disjoint statement.`}))
        | None => {
            let varInts = vars->Js_array2.map(ctx->ctxSymToIntExn)
            let maxIdx = varInts->Js_array2.length - 1
            for i in 0 to maxIdx {
                for j in i+1 to maxIdx {
                    ctx.contents.disj->disjAddPair(varInts[i],varInts[j])
                }
            }
        }
    }
}

let addFloating = (ctx:mmContext, ~label:string, ~exprStr:array<string>):unit => {
    if (exprStr->Js_array2.length != 2) {
        raise(MmException({msg:`Length of a floating expression must be 2.`}))
    } else {
        assertNameIsUnique(ctx,label,"a floating label")
        let typName = exprStr[0]
        let varName = exprStr[1]
        if (!(ctx->isConst(typName))) {
            raise(MmException({msg:`The first symbol in the floating '${typName}' must be a constant.`}))
        } else if (!(ctx->isVar(varName))) {
            raise(MmException({msg:`The second symbol in the floating '${varName}' must be a variable.`}))
        } else {
            let varInt = ctx->ctxSymToIntExn(varName)
            if (ctx->getTypeOfVar(varInt)->Belt_Option.isSome) {
                raise(MmException({msg:`Cannot redefine typecode for the variable '${varName}'`}))
            } else {
                let typInt = ctx->ctxSymToIntExn(typName)
                let expr = [typInt, varInt]
                let hyp = {typ:F, label, expr}
                let ctx = ctx.contents
                ctx.hyps->Js_array2.push(hyp)->ignore
                ctx.labelToHyp->Belt_HashMapString.set(label, hyp)
                ctx->exprToHypAdd(hyp)
                ctx.varTypes->Belt_HashMapInt.set(varInt, typInt)
            }
        }
    }
}

let addEssential = (ctx:mmContext, ~label:string, ~exprStr:array<string>):unit => {
    if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an essential expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an essential expression must be a constant.`}))
    } else {
        let expr = ctx->ctxSymsToIntsExn(exprStr)
        let hyp = {typ:E, label, expr}
        let ctx = ctx.contents
        ctx.hyps->Js_array2.push(hyp)->ignore
        ctx.labelToHyp->Belt_HashMapString.set(label, hyp)
        ctx->exprToHypAdd(hyp)
   }
}

let ctxIntToFrameInt = (ctxToFrameRenum: Belt_HashMapInt.t<int>, ctxInt:int):int => {
    if (ctxInt < 0) {
        ctxInt
    } else {
        switch ctxToFrameRenum->Belt_HashMapInt.get(ctxInt) {
            | None => 
                raise(MmException({
                    msg:`Cannot determine frame variable for the context variable ${ctxInt->Belt_Int.toString}.`
                }))
            | Some(frameInt) => frameInt
        }
    }
}

let renumberVarsInExpr = (ctxToFrameRenum: Belt_HashMapInt.t<int>, expr: expr): expr => {
    expr->Js_array2.map(ctxToFrameRenum->ctxIntToFrameInt)
}

let renumberVarsInHypothesis = (ctxToFrameRenum: Belt_HashMapInt.t<int>, hyp: hypothesis): hypothesis => {
    {
        ...hyp,
        expr: ctxToFrameRenum->renumberVarsInExpr(hyp.expr)
    }
}

let renumberVarsInDisj = (ctxToFrameRenum: Belt_HashMapInt.t<int>, disj:disjMutable): Belt_MapInt.t<Belt_SetInt.t> => {
    let res = disjMake()
    disj->disjForEach((n,m) => {
        res->disjAddPair(
            ctxToFrameRenum->ctxIntToFrameInt(n),
            ctxToFrameRenum->ctxIntToFrameInt(m)
        )
    })
    res
        ->Belt_HashMapInt.toArray
        ->Js.Array2.map(((n,ms)) => {
            (
                n,
                ms->Belt_HashSetInt.toArray->Belt_SetInt.fromArray
            )
        })
        ->Belt_MapInt.fromArray
}

let matchesOptRegex = (str:string, regex:option<Js_re.t>):bool => {
    regex->Belt_Option.map(regex => regex->Js_re.test_(str))->Belt.Option.getWithDefault(false)
}

let isMatch = (
    ~descr:option<string>,
    ~label:string,
    ~descrRegexToMatch:option<Js_re.t>,
    ~labelRegexToMatch:option<Js_re.t>,
):bool => {
    descr->Belt.Option.map(matchesOptRegex(_,descrRegexToMatch))->Belt_Option.getWithDefault(false) 
        || label->matchesOptRegex(labelRegexToMatch)
}

let isTranDepr = (
    ~ctx:mmContext,
    ~proof:option<proof>,
):bool => {
    switch proof {
        | None => false
        | Some(proof) => {
            switch proof {
                | Uncompressed({labels}) | Compressed({labels}) => {
                    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
                        if (
                            labels->Js_array2.some(label => {
                                ctx.deprOrTranDeprFrms->Belt_HashSetString.has(label)
                            })
                        ) {
                            Some(true)
                        } else {
                            None
                        }
                    })->Belt_Option.getWithDefault(false)
                }
            }
        }
    }
}

let getAsrtVarHyps = (numOfVars:int, asrtHyps:array<hypothesis>):array<int> => {
    let varHyps = Belt_Array.make(numOfVars, -1)
    asrtHyps->Js_array2.forEachi((hyp,i) => {
        if (hyp.typ == F) {
            let asrtVarNum = hyp.expr[1]
            if (asrtVarNum >= numOfVars) {
                raise(MmException({msg:`Internal error: hyp.expr[1] >= numOfVars`}))
            } else {
                varHyps[asrtVarNum] = i
            }
        }
    })
    if (varHyps->Js.Array2.some(idx => idx < 0)) {
        raise(MmException({msg:`Internal error: varHyps->Js.Array2.some(idx => idx < 0)`}))
    } else {
        varHyps
    }
}

let createFrame = (
    ~ctx:mmContext,
    ~ord:int,
    ~isAxiom:bool, 
    ~label:string, 
    ~exprStr:array<string>,
    ~proof:option<proof>,
    ~tokenType:string="a frame",
    ~skipEssentials:bool=false, 
    ~skipFirstSymCheck:bool=false, 
    ~skipDisj:bool=false, 
    ~overrideHyps:option<array<expr>>=?,
    ~descrRegexToDisc:option<Js_re.t>=?,
    ~labelRegexToDisc:option<Js_re.t>=?,
    ~descrRegexToDepr:option<Js_re.t>=?,
    ~labelRegexToDepr:option<Js_re.t>=?,
    ()
):frame => {
    assertNameIsUnique(ctx,label,tokenType)
    if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an assertion expression must be at least 1.`}))
    } else if (!skipFirstSymCheck && !(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an assertion expression must be a constant.`}))
    } else {
        switch exprStr->Js_array2.find(sym => ctx->ctxSymToInt(sym)->Belt_Option.isNone) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let asrt = exprStr->Js_array2.map(ctx->ctxSymToIntExn)
                let mandatoryVarsSet = extractMandatoryVariables(ctx, asrt, ~skipEssentials, ~overrideHyps?, ())
                let mandatoryVarsArr = mandatoryVarsSet->Belt_HashSetInt.toArray
                let mandatoryDisj = if (skipDisj) {disjMake()} else {
                    extractMandatoryDisj(ctx, mandatoryVarsSet)
                }
                let mandatoryHypotheses = extractMandatoryHypotheses(
                    ctx, mandatoryVarsSet, ~skipEssentials, ~overrideHyps?, ()
                )
                let ctxToFrameRenum = mandatoryVarsArr
                                        ->Js_array2.mapi((cv,fv) => (cv,fv))
                                        ->Belt_HashMapInt.fromArray
                let descr = ctx.contents.lastComment
                let hyps = mandatoryHypotheses->Js_array2.map(ctxToFrameRenum->renumberVarsInHypothesis)
                let numOfVars = mandatoryVarsArr->Js_array2.length
                let frame = {
                    ord,
                    isAxiom,
                    disj: ctxToFrameRenum->renumberVarsInDisj(mandatoryDisj),
                    hyps,
                    asrt: ctxToFrameRenum->renumberVarsInExpr(asrt),
                    label,
                    frameVarToSymb: mandatoryVarsArr->Js_array2.map(ctx->ctxIntToSymExn),
                    varTypes: mandatoryVarsArr->Js_array2.map(ctx->getTypeOfVarExn),
                    varHyps: getAsrtVarHyps(numOfVars, hyps),
                    numOfVars,
                    numOfArgs: mandatoryHypotheses->Js_array2.length,
                    descr,
                    proof,
                    isDisc: isMatch( 
                        ~descr, ~label, ~descrRegexToMatch=descrRegexToDisc, ~labelRegexToMatch=labelRegexToDisc, 
                    ),
                    isDepr: isMatch( 
                        ~descr, ~label, ~descrRegexToMatch=descrRegexToDepr, ~labelRegexToMatch=labelRegexToDepr, 
                    ),
                    isTranDepr: isTranDepr( ~ctx, ~proof, ),
                    dbg:
                        if (ctx.contents.debug) {
                            Some({
                                disj: mandatoryDisj->disjToArr(())->Js_array2.map(ctx->ctxIntsToStrExn),
                                hyps: mandatoryHypotheses->Js_array2.map(hyp => ctx->ctxIntsToStrExn(hyp.expr)),
                                asrt: ctx->ctxIntsToStrExn(asrt),
                            })
                        } else {
                            None
                        }
                }
                frame
            }
        }
    }
}

let addAssertion = (
    ctx:mmContext, 
    ~isAxiom:bool, 
    ~label:string, 
    ~exprStr:array<string>, 
    ~proof:option<proof>,
    ~descrRegexToDisc:option<Js_re.t>=?,
    ~labelRegexToDisc:option<Js_re.t>=?,
    ~descrRegexToDepr:option<Js_re.t>=?,
    ~labelRegexToDepr:option<Js_re.t>=?,
    ()
):unit => {
    let currCtx = ctx.contents
    let frame = createFrame(
        ~ctx, 
        ~ord=currCtx.totalNumOfFrames,
        ~isAxiom, ~label, ~exprStr, ~proof, 
        ~tokenType = if (proof->Belt_Option.isNone) {"an axiom"} else {"a theorem"}, 
        ~descrRegexToDisc?,
        ~labelRegexToDisc?,
        ~descrRegexToDepr?,
        ~labelRegexToDepr?,
        ()
    )
    currCtx.frames->Belt_HashMapString.set( label, frame )
    currCtx.totalNumOfFrames = currCtx.totalNumOfFrames + 1
    if (frame.isDepr || frame.isTranDepr) {
        currCtx.deprOrTranDeprFrms->Belt_HashSetString.add(label)
    }
}

let applySingleStmt = (
    ctx:mmContext, 
    stmt:stmt,
    ~descrRegexToDisc:option<Js_re.t>=?,
    ~labelRegexToDisc:option<Js_re.t>=?,
    ~descrRegexToDepr:option<Js_re.t>=?,
    ~labelRegexToDepr:option<Js_re.t>=?,
    ()
):unit => {
    switch stmt {
        | Comment({text}) => addComment(ctx, text)
        | Const({symbols}) => symbols->Js_array2.forEach(addConst(ctx, _))
        | Block(_) => raise(MmException({msg:`Block statements are not accepted by applySingleStmt().`}))
        | Var({symbols}) => symbols->Js_array2.forEach(addVar(ctx, _))
        | Disj({vars}) => addDisj(ctx, vars)
        | Floating({label, expr}) => addFloating(ctx, ~label, ~exprStr=expr)
        | Essential({label, expr}) => addEssential(ctx, ~label, ~exprStr=expr)
        | Axiom({label, expr}) => {
            addAssertion(
                ctx, ~isAxiom=true, ~label, ~exprStr=expr, ~proof=None, 
                ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?, ()
            )
        }
        | Provable({label, expr, proof}) => {
            addAssertion(
                ctx, ~isAxiom=false, ~label, ~exprStr=expr, ~proof, 
                ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?, ()
            )
        }
    }
}

let loadContext = (
    ast, 
    ~initialContext=?,
    ~stopBefore="",
    ~stopAfter="",
    ~onPreProcess: option<(mmContext,MM_parser.stmt)=>unit>=?,
    ~expectedNumOfAssertions=-1, 
    ~descrRegexToDisc:option<Js_re.t>=?,
    ~labelRegexToDisc:option<Js_re.t>=?,
    ~descrRegexToDepr:option<Js_re.t>=?,
    ~labelRegexToDepr:option<Js_re.t>=?,
    ~onProgress= _=>(), 
    ~debug:bool=false, 
    ()
) => {
    let expectedNumOfAssertionsF = expectedNumOfAssertions->Belt_Int.toFloat
    let assertionsProcessed = ref(0.)
    let progressTracker = progressTrackerMake(~step=0.1, ~onProgress, ())

    let onAsrtProcess = () => {
        if (expectedNumOfAssertions > 0) {
            assertionsProcessed.contents = assertionsProcessed.contents +. 1.
            progressTracker->progressTrackerSetCurrPct(assertionsProcessed.contents /. expectedNumOfAssertionsF)
        }
    }

    let (ctx, _) = traverseAst(
        switch initialContext {
            | Some(ctx) => ctx
            | None => createContext(~debug, ())
        },
        ast,
        ~preProcess = (ctx,node) => {
            switch onPreProcess {
                | None => ()
                | Some(onPreProcess) => onPreProcess(ctx, node.stmt)
            }
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        openChildContext(ctx)
                    }
                    ctx.contents.lastComment = None
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} => {
                    onAsrtProcess()
                    if (stopBefore == label) {
                        Some(())
                    } else {
                        None
                    }
                }
                | _ => None
            }
        },
        ~process = (ctx,node) => {
            switch node {
                | {stmt:Block(_)} => ()
                | {stmt} => {
                    applySingleStmt(
                        ctx, stmt, ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?, ()
                    )
                }
            }
            None
        },
        ~postProcess = (ctx,node) => {
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        closeChildContext(ctx)
                    }
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} if stopAfter == label => Some(())
                | _ => None
            }
        },
        ()
    )
    ctx
}

let generateNewVarNames = (
    ~ctx:mmContext, 
    ~types:array<int>, 
    ~typeToPrefix:Belt_MapString.t<string>=Belt_MapString.empty,
    ~reservedNames:option<Belt_HashSetString.t>=?,
    ()
): array<string> => {
    let prefixToCnt = Belt_HashMapString.make(~hintSize=typeToPrefix->Belt_MapString.size)

    let getCnt = prefix => prefixToCnt->Belt_HashMapString.get(prefix)->Belt.Option.getWithDefault(0)
    let incCnt = prefix => prefixToCnt->Belt_HashMapString.set(prefix,getCnt(prefix)+1)

    let maxI = types->Js.Array2.length - 1
    let res = []
    for i in 0 to maxI {
        let typeStr = ctx->ctxIntToSymExn(types[i])
        let prefix = typeToPrefix->Belt_MapString.getWithDefault(typeStr, typeStr)
        incCnt(prefix)
        let newName = ref(prefix ++ getCnt(prefix)->Belt_Int.toString)
        while (ctx->getTokenType(newName.contents)->Belt_Option.isSome
                || reservedNames
                        ->Belt.Option.map(Belt_HashSetString.has(_,newName.contents))
                        ->Belt_Option.getWithDefault(false)
        ) {
            incCnt(prefix)
            newName.contents = prefix ++ getCnt(prefix)->Belt_Int.toString
        }
        res->Js.Array2.push(newName.contents)->ignore
    }
    res
}

let generateNewLabels = (
    ~ctx:mmContext, 
    ~prefix:string, 
    ~amount:int,
    ~reservedLabels:option<Belt_HashSetString.t>=?,
    ~checkHypsOnly:bool=false,
    ()
): array<string> => {
    let labelIsReserved = label => {
        reservedLabels->Belt.Option.map(Belt_HashSetString.has(_,label))->Belt_Option.getWithDefault(false)
            || (!checkHypsOnly && ctx->getTokenType(label)->Belt_Option.isSome)
            || (checkHypsOnly && ctx->isHyp(label))
    }

    let maxI = amount - 1
    let cnt = ref(0)
    let res = []
    for _ in 0 to maxI {
        cnt.contents = cnt.contents + 1
        let newName = ref(prefix ++ cnt.contents->Belt_Int.toString)
        while (labelIsReserved(newName.contents)) {
            cnt.contents = cnt.contents + 1
            newName.contents = prefix ++ cnt.contents->Belt_Int.toString
        }
        res->Js.Array2.push(newName.contents)->ignore
    }
    res
}

let renumberConst = (constRenum:Belt_HashMapInt.t<int>, c:int):int => {
    constRenum->Belt_HashMapInt.get(c)->Belt_Option.getWithDefault(c)
}

let renumberConstsInExpr = (constRenum:Belt_HashMapInt.t<int>, expr:expr):unit => {
    let maxI = expr->Js_array2.length-1
    for i in 0 to maxI {
        let sym = expr[i]
        if (sym < 0) {
            expr[i] = constRenum->renumberConst(sym)
        }
    }
}

let moveConstsToBegin = (ctx:mmContext, firstConsts:array<int>):unit => {
    let rootCtx = ctx.contents.root->Belt_Option.getExn
    let newConstOrder:array<int> = firstConsts->Js_array2.copy
    for i in -1 downto -(rootCtx.consts->Js_array2.length-1) {
        if (!(newConstOrder->Js_array2.includes(i))) {
            newConstOrder->Js_array2.push(i)->ignore
        }
    }
    let oldConstOrder:array<string> = rootCtx.consts->Js.Array2.copy
    let constRenum = Belt_HashMapInt.make(~hintSize=rootCtx.consts->Js.Array2.length)
    newConstOrder->Js_array2.forEachi((symOldInt,i) => {
        let sym = oldConstOrder[-symOldInt]
        let symNewInt = -(i+1)
        constRenum->Belt_HashMapInt.set(symOldInt, symNewInt)
        rootCtx.consts[-symNewInt] = sym
        rootCtx.symToInt->Belt_HashMapString.set(sym, symNewInt)
    })

    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        constRenum->renumberConstsInExpr(hyp.expr)
        None
    })->ignore
    ctx->forEachFrame(frame => {
        frame.hyps->Js_array2.forEach(hyp => constRenum->renumberConstsInExpr(hyp.expr))
        constRenum->renumberConstsInExpr(frame.asrt)
        constRenum->renumberConstsInExpr(frame.varTypes)
        None
    })->ignore
    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
        let maxI = ctx.vars->Js_array2.length-1
        for i in 0 to maxI {
            let var = ctx.varsBaseIdx + i
            switch ctx.varTypes->Belt_HashMapInt.get(var) {
                | None => ()
                | Some(oldTyp) => ctx.varTypes->Belt_HashMapInt.set(var, constRenum->renumberConst(oldTyp))
            }
        }
        ctx.exprToHyp->Belt_HashMapInt.clear
        ctx.hyps->Js.Array2.forEach(ctx->exprToHypAdd)
        None
    })->ignore
}

let frameRemoveRedundantText = (
    frame:frame,
    ~removeAsrtDescr:bool,
    ~removeProofs:bool,
):frame => {
    {
        ...frame,
        descr: if (removeAsrtDescr) {None} else {frame.descr},
        proof: if (removeProofs) {None} else {frame.proof},
    }
}

let rec ctxRemoveRedundantText = (
    ctx:mmContextContents,
    ~removeAsrtDescr:bool,
    ~removeProofs:bool,
):(mmContextContents, mmContextContents) => {
    let removeRedundantData = ctx => {
        {
            ...ctx,
            lastComment: if (removeAsrtDescr) {None} else {ctx.lastComment},
            frames: ctx.frames->Belt_HashMapString.toArray->Js_array2.map(((label,frame)) => {
                (
                    label,
                    frame->frameRemoveRedundantText(~removeAsrtDescr, ~removeProofs)
                )
            })->Belt_HashMapString.fromArray,
            totalNumOfFrames: switch ctx.parent {
                | None => ctx.frames->Belt_HashMapString.size
                | Some(parent) => parent.totalNumOfFrames + ctx.frames->Belt_HashMapString.size
            },
            deprOrTranDeprFrms:Belt_HashSetString.make(~hintSize=0),
        }
    }

    switch ctx.parent {
        | None => {
            let res = removeRedundantData(ctx)
            res.root = Some(res)
            (res, res)
        }
        | Some(parent) => {
            let (newRoot, newParent) = ctxRemoveRedundantText(parent, ~removeAsrtDescr, ~removeProofs)
            let res = {
                ...removeRedundantData(ctx),
                root: Some(newRoot),
                parent: Some(newParent),
            }
            (newRoot, res)
        }
    }
}

let sortBySym = (ctx:mmContext, expr:array<int>):array<int> => {
    ctx->ctxIntsToSymsExn(expr)->Js.Array2.sortInPlace->ctxSymsToIntsExn(ctx, _)
}

let ctxGetOptimizedConstsOrder = (ctx:mmContext, ~parens:string):optimizedConstsOrder => {
    let allConsts = Belt_HashSetInt.fromArray( ctx->ctxSymsToIntsExn( ctx->getAllConsts ) )
    let numOfConsts = allConsts->Belt_HashSetInt.size

    let canBeFirst = Belt_HashSetInt.make(~hintSize=numOfConsts)
    let canBeLast = Belt_HashSetInt.make(~hintSize=numOfConsts)
    ctx->forEachFrame(frame => {
        let first = frame.asrt[1]
        if (first < 0) {
            canBeFirst->Belt_HashSetInt.add(first)
        }
        let last = frame.asrt[frame.asrt->Js_array2.length-1]
        if (last < 0) {
            canBeLast->Belt_HashSetInt.add(last)
        }
        None
    })->ignore
    let canBeFirstAndLast = canBeFirst->Belt_HashSetInt.toArray
        ->Js.Array2.filter(canBeLast->Belt_HashSetInt.has)
        ->Belt_HashSetInt.fromArray
    canBeFirstAndLast->Belt_HashSetInt.forEach(canBeFirst->Belt_HashSetInt.remove)
    canBeFirstAndLast->Belt_HashSetInt.forEach(canBeLast->Belt_HashSetInt.remove)

    let parenInts = parens->getSpaceSeparatedValuesAsArray
        ->Js_array2.map(ctx->ctxSymToInt)
        ->Js.Array2.filter(intOpt => intOpt->Belt_Option.mapWithDefault(false, i => i < 0))
        ->Js.Array2.map(Belt_Option.getExn)
    parenInts->Js.Array2.forEach(i => {
        canBeFirst->Belt_HashSetInt.remove(i)
        canBeFirstAndLast->Belt_HashSetInt.remove(i)
        canBeLast->Belt_HashSetInt.remove(i)
    })

    let remainingConsts = allConsts->Belt_HashSetInt.toArray->Js.Array2.filter(i => {
        !(parenInts->Js.Array2.includes(i))
        && !(canBeFirst->Belt_HashSetInt.has(i))
        && !(canBeFirstAndLast->Belt_HashSetInt.has(i))
        && !(canBeLast->Belt_HashSetInt.has(i))
    })->Belt_HashSetInt.fromArray

    let newConstsOrder = Belt_Array.concatMany([
        parenInts,
        canBeFirst->Belt_HashSetInt.toArray->sortBySym(ctx,_),
        canBeFirstAndLast->Belt_HashSetInt.toArray->sortBySym(ctx,_),
        canBeLast->Belt_HashSetInt.toArray->sortBySym(ctx,_),
        remainingConsts->Belt_HashSetInt.toArray->sortBySym(ctx,_),
    ])

    let parenMin = -(parenInts->Js.Array2.length)

    let canBeFirstMax = parenMin-1
    let canBeFirstMin = canBeFirstMax - canBeFirst->Belt_HashSetInt.size + 1

    let canBeFirstAndLastMax = canBeFirstMin-1
    let canBeFirstAndLastMin = canBeFirstAndLastMax - canBeFirstAndLast->Belt_HashSetInt.size + 1

    let canBeLastMax = canBeFirstAndLastMin-1
    let canBeLastMin = canBeLastMax - canBeLast->Belt_HashSetInt.size + 1

    {
        allConsts:ctx->ctxIntsToSymsExn(newConstsOrder),
        parenMin,
        canBeFirstMin:canBeFirstAndLastMin,
        canBeFirstMax:canBeFirstMax,
        canBeLastMin,
        canBeLastMax:canBeFirstAndLastMax,
    }
}

let ctxOptimizeForProver = ( 
    ctx:mmContext,
    ~parens:string,
    ~removeAsrtDescr:bool=true,
    ~removeProofs:bool=true,
    ()
):mmContext => {
    let (_,ctx) = ctx.contents->ctxRemoveRedundantText( ~removeAsrtDescr, ~removeProofs, )
    let resCtx = ref(ctx)
    let {allConsts} = resCtx->ctxGetOptimizedConstsOrder(~parens)
    resCtx->moveConstsToBegin(resCtx->ctxSymsToIntsExn(allConsts))
    resCtx
}