open MM_parser
open MM_progress_tracker
open MM_wrk_settings
open Common

type expr = array<int>

let exprEq: (expr,expr) => bool = (a,b) => {
    let len1 = a->Array.length
    let len2 = b->Array.length
    if (len1 != len2) {
        false
    } else {
        let eq = ref(true)
        let i = ref(0)
        while (eq.contents && i.contents < len1) {
            eq.contents = a->Array.getUnsafe(i.contents) == b->Array.getUnsafe(i.contents)
            i.contents = i.contents + 1
        }
        eq.contents
    }
}

let exprCmp = (e1:expr, e2:expr):Ordering.t => {
    let len1 = e1->Array.length
    let len2 = e2->Array.length
    switch Int.compare(len1, len2) {
        | 0.0 => {
            let res = ref(0.0)
            let i = ref(0)
            while (i.contents < len1 && res.contents == 0.0) {
                res.contents = Int.compare(e1->Array.getUnsafe(i.contents), e2->Array.getUnsafe(i.contents))
                i.contents = i.contents + 1
            }
            res.contents
        }
        | r => r
    }
}

module ExprCmp = Belt.Id.MakeComparableU({
    type t = expr
    let cmp = exprCmp->Expln_utils_common.toIntCmp
})

module ExprHash = Belt.Id.MakeHashableU({
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
    descrNorm:option<string>,
    proof:option<proof>,
    isDisc:bool, /* is discouraged */
    isDepr:bool, /* is deprecated */
    isTranDepr:bool, /* is transitively deprecated (depends on an isDepr frame or another isTranDepr frame) */
    dbg: option<frameDbg>,
    usageCnt: int, //the number of theorems which directly depend on this assertion
    mutable allHyps?:array<int>, //concatenation of all essential hyps, used for the pattern search
    mutable allHypsAsrt?:array<int>, //concatenation of all essential hyps and asrt, used for the pattern search
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
        ->Array.map(((n,ms)) => (n,ms->Belt_HashSetInt.toArray->Belt_SetInt.fromArray))
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
            | Some(frame) => if (frame.isAxiom) {Some(A)} else {Some(P)}
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

let disjForEachOrdered = (disjMutable, consumer) => {
    let ns = disjMutable->Belt_HashMapInt.keysToArray
    ns->Array.sort(Int.compare)
    ns->Array.forEach(n => {
        let ms = disjMutable->Belt_HashMapInt.get(n)->Option.getExn->Belt_HashSetInt.toArray
        ms->Array.sort(Int.compare)
        ms->Array.forEach(m => consumer(n,m))
    })
}

let disjImmForEach = (disj:Belt_MapInt.t<Belt_SetInt.t>, consumer) => {
    disj->Belt_MapInt.forEach((n,ms) => {
        ms->Belt_SetInt.forEach(m => {
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
    ~typeOrder:option<Belt_HashMapInt.t<int>>=?
):array<array<int>> => {
    let res = []
    disj->disjForEachOrdered((n,m) => res->Array.push([n,m]))

    let canMerge = (d1:array<int>,d2:array<int>):bool => {
        let canMerge = ref(true)
        d1->Array.forEach(v1 => {
            if (canMerge.contents) {
                d2->Array.forEach(v2 => {
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
            ~pos=res->Array.findIndex(d => d->exprEq(d2)),
            ~count=1
        )->ignore
        d2->Array.forEach(v2 => {
            if (!(d1->Array.includes(v2))) {
                d1->Array.push(v2)  
            }
        })
    }

    let findWhatToMerge = ():option<(array<int>,array<int>)> => {
        let found = ref(None)
        for i in 0 to res->Array.length-2 {
            if (found.contents->Belt_Option.isNone) {
                for j in i+1 to res->Array.length-1 {
                    if (found.contents->Belt_Option.isNone) {
                        let d1 = res->Array.getUnsafe(i)
                        let d2 = res->Array.getUnsafe(j)
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
            | None => Int.compare
            | Some(varIntToVarType) => {
                switch typeOrder {
                    | None => Int.compare
                    | Some(typeOrder) => {
                        switch varIntToVarName {
                            | None => Int.compare
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
        Int.compare
    }
    res->Array.forEach(d =>
        d->Expln_utils_common.sortInPlaceWith(sortBy)->ignore
    )
    res->Expln_utils_common.sortInPlaceWith(exprCmp)
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
        ~typeOrder?
    )->Array.forEach(consumer)
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
    let len = expr->Array.length
    let fstSym = if len > 0 {expr->Array.getUnsafe(0)} else {0}
    let sndSym = if len > 1 {expr->Array.getUnsafe(1)} else {0}
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
                            switch hypsArr->Array.find(h => h.expr->exprEq(hyp.expr)) {
                                | Some(_) => ()
                                | None => hypsArr->Array.push(hyp)
                            }
                        }
                    }
                }
            }
        }
    }
}

let exprToHypGet = (ctx:mmContextContents, expr:expr):option<hypothesis> => {
    let len = expr->Array.length
    switch ctx.exprToHyp->Belt_HashMapInt.get(len) {
        | None => None
        | Some(fstSymToHyp) => {
            let fstSym = if len > 0 {expr->Array.getUnsafe(0)} else {0}
            switch fstSymToHyp->Belt_HashMapInt.get(fstSym) {
                | None => None
                | Some(sndSymToHyp) => {
                    let sndSym = if len > 1 {expr->Array.getUnsafe(1)} else {0}
                    switch sndSymToHyp->Belt_HashMapInt.get(sndSym) {
                        | None => None
                        | Some(hypsArr) => hypsArr->Array.find(h => h.expr->exprEq(expr))
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
    ctx.contents.vars->Array.copy
}

let getLocalHyps: mmContext => array<hypothesis> = ctx => {
    ctx.contents.hyps->Array.copy
}

let getNumOfVars = ctx => {
    ctx.contents.varsBaseIdx + ctx.contents.vars->Array.length
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
    symbols->Array.map(ctxSymToIntExn(ctx, _))
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
                Some(ctx.vars->Array.getUnsafe(i-ctx.varsBaseIdx))
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

let ctxIntsToSymsExn = (ctx,expr) => expr->Array.map(ctxIntToSymExn(ctx, _))

let ctxIntsToStrExn = (ctx:mmContext, expr:expr):string => {
    expr->Array.map(ctxIntToSymExn(ctx, _))->Array.joinUnsafe(" ")
}

let disjToStrDbg = (disj:disjMutable, ctx:mmContext):string => {
    let res = []
    disj->Belt_HashMapInt.keysToArray->Array.toSorted(Int.compare)->Array.forEach(n=>{
        let ms = disj->Belt_HashMapInt.get(n)->Option.getExn
        res->Array.push(
            ctx->ctxIntToSymExn(n) 
            ++ " -> " 
            ++ ctx->ctxIntsToStrExn(ms->Belt_HashSetInt.toArray->Array.toSorted(Int.compare))
        )
    })
    res->Array.join("\n")
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
    expr->Array.map(frmIntToSymExn(ctx, frame, _))
}

let frmIntsToStrExn = (ctx:mmContext, frame:frame, expr:expr):string => {
    frmIntsToSymsExn(ctx, frame, expr)->Array.joinUnsafe(" ")
}

let frmGetAllHyps = (frm:frame):array<int> => {
    switch frm.allHyps {
        | Some(arr) => arr
        | None => {
            let allHyps:array<array<int>> = frm.hyps->Array.filter(hyp => hyp.typ == E)->Array.map(hyp => hyp.expr)
            let res = Array.concatMany([], allHyps)
            frm.allHyps = Some(res)
            res
        }
    }
}

let frmGetAllHypsAsrt = (frm:frame):array<int> => {
    switch frm.allHypsAsrt {
        | Some(arr) => arr
        | None => {
            let res = frmGetAllHyps(frm)->Array.copy->Array.concat(frm.asrt)
            frm.allHypsAsrt = Some(res)
            res
        }
    }
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
    ~overrideHyps:option<array<expr>>=?
):Belt_HashSetInt.t => {
    let res = Belt_HashSetInt.make(~hintSize=16)
    if (!skipEssentials) {
        switch overrideHyps {
            | Some(overrideHyps) => {
                overrideHyps->Array.forEach(hypExpr => {
                    hypExpr->Array.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
                })
            }
            | None => {
                ctx->forEachHypothesisInDeclarationOrder(hyp => {
                    if (hyp.typ == E) {
                        hyp.expr->Array.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
                    }
                    None
                })->ignore
            }
        }
    }
    asrt->Array.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
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
    ~overrideHyps:option<array<expr>>=?
):array<hypothesis> => {
    let res = []
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (
            hyp.typ == E && (!skipEssentials && overrideHyps->Belt_Option.isNone)
            || hyp.typ == F && mandatoryVars->Belt_HashSetInt.has(hyp.expr->Array.getUnsafe(1))
        ) {
            res->Array.push(hyp)
        }
        None
    })->ignore
    switch overrideHyps {
        | None => ()
        | Some(overrideHyps) => {
            overrideHyps->Array.forEachWithIndex((hypExpr,i) => {
                res->Array.push({
                    typ: E,
                    label: i->Belt.Int.toString,
                    expr: hypExpr
                })
            })
        }
    }
    res
}

let getAllConsts = (ctx:mmContext):array<string> => {
    Belt_Option.getExn(ctx.contents.root).consts->Array.sliceToEnd(~start=1)
}

let getMandHyps = (
    ctx:mmContext, 
    expr:expr,
    ~skipEssentials:bool=false, 
    ~overrideHyps:option<array<expr>>=?
):array<hypothesis> => {
    let mandatoryVars = extractMandatoryVariables( ctx, expr, ~skipEssentials, ~overrideHyps? )
    extractMandatoryHypotheses(ctx, mandatoryVars, ~skipEssentials, ~overrideHyps?)
}

let getAllHyps = (ctx:mmContext):Belt_MapString.t<hypothesis> => {
    let hyps = []
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        hyps->Array.push(hyp)
        None
    })->ignore
    Belt_MapString.fromArray(hyps->Array.map(hyp => (hyp.label, hyp)))
}

let getAllFramesArr = (ctx:mmContext):array<frame> => {
    let frames = []
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        frames->Array.pushMany(ctx.frames->Belt_HashMapString.valuesToArray)
        None
    })->ignore
    frames
}

let getAllFrames = (ctx:mmContext):Belt_MapString.t<frame> => {
    let frames = []
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->Belt_HashMapString.forEach((k,v) => frames->Array.push((k,v)))
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

let findParentheses = (ctx:mmContext, ~onProgress:option<float=>unit>=?):array<int> => {

    let getAllExprs = ctx => {
        let allExpr = []
        ctx->forEachFrame(frame => {
            frame.hyps->Array.forEach(hyp => {
                if (hyp.typ == E) {
                    allExpr->Array.push(hyp.expr)
                }
            })
            allExpr->Array.push(frame.asrt)
            None
        })->ignore
        allExpr
    }

    let checkValidParens = (allExprs, openSym, closeSym):bool => {
        open MM_parenCounter_unoptimized
        let res = ref(true)
        let openUsed = ref(false)
        let closeUsed = ref(false)
        let parenCnt = parenCntMake([openSym, closeSym], ~checkParensOptimized=false)
        let parenState = ref(Balanced)
        let allExprsLen = allExprs->Array.length
        let e = ref(0)
        while (e.contents < allExprsLen && res.contents) {
            let expr = allExprs->Array.getUnsafe(e.contents)
            let exprLen = expr->Array.length
            let s = ref(0)
            while (s.contents < exprLen && res.contents) {
                let sym = expr->Array.getUnsafe(s.contents)
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
        ->Array.filter(isConst(ctx, _))
        ->ctxSymsToIntsExn(ctx, _)
        ->Array.concat(
            Belt_Array.range(
                1,
                (ctx.contents.root->Belt.Option.getExn).consts->Array.length - 1
            )->Array.map(i => -i)
        )

    let maxC = allConsts->Array.length - 2
    let maxCF = maxC->Belt_Int.toFloat
    let progressState = progressTrackerMake(~step=0.01, ~onProgress?)
    let foundParens = []
    for c in 0 to maxC {
        let openParen = allConsts->Array.getUnsafe(c)
        let closeParen = allConsts->Array.getUnsafe(c+1)
        if (!(foundParens->Array.includes(openParen))
            && !(foundParens->Array.includes(closeParen))
            && checkValidParens(allExprs, openParen, closeParen)
        ) {
            foundParens->Array.push(openParen)
            foundParens->Array.push(closeParen)
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

let createContext = (~parent:option<mmContext>=?, ~debug:bool=false):mmContext => {
    let pCtxContentsOpt = switch parent {
        | Some(pCtx) => {
            pCtx.contents.lastComment = None
            Some(pCtx.contents)
        }
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
                | Some(parent) => parent.varsBaseIdx + parent.vars->Array.length
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
    ctx.contents = createContext(~parent=ctx).contents
}

let closeChildContext = (ctx:mmContext):unit => {
    let childCtx = ctx.contents
    ctx.contents = switch childCtx.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            parent.lastComment = None
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
        | Some(parent) => {
            parent.lastComment = None
            parent
        }
    }
}

let addComment = (ctx:mmContext,str:string):unit => {
    ctx.contents.lastComment = Some(str)
}

let assertNameIsUnique = (ctx:mmContext,name:string,tokenType:string):unit => {
    if (name->String.trim == "") {
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
        ctx.symToInt->Belt_HashMapString.set(name, -(ctx.consts->Array.length))
        ctx.consts->Array.push(name)
    }
}

let addVar = (ctx:mmContext,name:string):unit => {
    assertNameIsUnique(ctx,name,"a variable")
    let ctx = ctx.contents
    ctx.symToInt->Belt_HashMapString.set(name, ctx.varsBaseIdx + ctx.vars->Array.length)
    ctx.vars->Array.push(name)
}

let addDisj = (ctx:mmContext, vars:array<string>):unit => {
    switch vars->Array.find(sym => !(ctx->isVar(sym))) {
        | Some(sym) => 
            raise(MmException({msg:`The symbol '${sym}' is not a variable but it is used in a disjoint statement.`}))
        | None => {
            let varInts = vars->Array.map(ctxSymToIntExn(ctx, _))
            let maxIdx = varInts->Array.length - 1
            for i in 0 to maxIdx {
                for j in i+1 to maxIdx {
                    ctx.contents.disj->disjAddPair(varInts->Array.getUnsafe(i),varInts->Array.getUnsafe(j))
                }
            }
        }
    }
}

let addFloating = (ctx:mmContext, ~label:string, ~exprStr:array<string>):unit => {
    if (exprStr->Array.length != 2) {
        raise(MmException({msg:`Length of a floating expression must be 2.`}))
    } else {
        assertNameIsUnique(ctx,label,"a floating label")
        let typName = exprStr->Array.getUnsafe(0)
        let varName = exprStr->Array.getUnsafe(1)
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
                ctx.hyps->Array.push(hyp)
                ctx.labelToHyp->Belt_HashMapString.set(label, hyp)
                ctx->exprToHypAdd(hyp)
                ctx.varTypes->Belt_HashMapInt.set(varInt, typInt)
            }
        }
    }
}

let addEssential = (ctx:mmContext, ~label:string, ~exprStr:array<string>):unit => {
    if (exprStr->Array.length < 1) {
        raise(MmException({msg:`Length of an essential expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr->Array.getUnsafe(0)))) {
        raise(MmException({msg:`The first symbol in an essential expression must be a constant.`}))
    } else {
        let expr = ctx->ctxSymsToIntsExn(exprStr)
        let hyp = {typ:E, label, expr}
        let ctx = ctx.contents
        ctx.hyps->Array.push(hyp)
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
    expr->Array.map(ctxIntToFrameInt(ctxToFrameRenum, _))
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
        ->Array.map(((n,ms)) => {
            (
                n,
                ms->Belt_HashSetInt.toArray->Belt_SetInt.fromArray
            )
        })
        ->Belt_MapInt.fromArray
}

let matchesOptRegex = (str:string, regex:option<RegExp.t>):bool => {
    regex->Belt_Option.map(regex => regex->RegExp.test(str))->Belt.Option.getWithDefault(false)
}

let isMatch = (
    ~descr:option<string>,
    ~label:string,
    ~descrRegexToMatch:option<RegExp.t>,
    ~labelRegexToMatch:option<RegExp.t>,
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
                            labels->Array.some(label => {
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
    asrtHyps->Array.forEachWithIndex((hyp,i) => {
        if (hyp.typ == F) {
            let asrtVarNum = hyp.expr->Array.getUnsafe(1)
            if (asrtVarNum >= numOfVars) {
                raise(MmException({msg:`Internal error: hyp.expr[1] >= numOfVars`}))
            } else {
                varHyps[asrtVarNum] = i
            }
        }
    })
    if (varHyps->Array.some(idx => idx < 0)) {
        raise(MmException({msg:`Internal error: varHyps->Array.some(idx => idx < 0)`}))
    } else {
        varHyps
    }
}

let normalizeDescrRegex = RegExp.fromStringWithFlags("\\s+", ~flags="g")
let normalizeDescr = (descr:string):string => {
    descr->String.trim->String.toLowerCase->String.replaceAllRegExp(normalizeDescrRegex, " ")
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
    ~descrRegexToDisc:option<RegExp.t>=?,
    ~labelRegexToDisc:option<RegExp.t>=?,
    ~descrRegexToDepr:option<RegExp.t>=?,
    ~labelRegexToDepr:option<RegExp.t>=?
):frame => {
    assertNameIsUnique(ctx,label,tokenType)
    if (exprStr->Array.length < 1) {
        raise(MmException({msg:`Length of an assertion expression must be at least 1.`}))
    } else if (!skipFirstSymCheck && !(ctx->isConst(exprStr->Array.getUnsafe(0)))) {
        raise(MmException({msg:`The first symbol in an assertion expression must be a constant.`}))
    } else {
        switch exprStr->Array.find(sym => ctx->ctxSymToInt(sym)->Belt_Option.isNone) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let asrt = exprStr->Array.map(ctxSymToIntExn(ctx, _))
                let mandatoryVarsSet = extractMandatoryVariables(ctx, asrt, ~skipEssentials, ~overrideHyps?)
                let mandatoryVarsArr = mandatoryVarsSet->Belt_HashSetInt.toArray
                let mandatoryDisj = if (skipDisj) {disjMake()} else {
                    extractMandatoryDisj(ctx, mandatoryVarsSet)
                }
                let mandatoryHypotheses = extractMandatoryHypotheses(
                    ctx, mandatoryVarsSet, ~skipEssentials, ~overrideHyps?
                )
                let ctxToFrameRenum = mandatoryVarsArr
                                        ->Array.mapWithIndex((cv,fv) => (cv,fv))
                                        ->Belt_HashMapInt.fromArray
                let descr = ctx.contents.lastComment
                let descrNorm = descr->Option.map(normalizeDescr)
                let hyps = mandatoryHypotheses->Array.map(renumberVarsInHypothesis(ctxToFrameRenum, _))
                let numOfVars = mandatoryVarsArr->Array.length
                let frame = {
                    ord,
                    isAxiom,
                    disj: ctxToFrameRenum->renumberVarsInDisj(mandatoryDisj),
                    hyps,
                    asrt: ctxToFrameRenum->renumberVarsInExpr(asrt),
                    label,
                    frameVarToSymb: mandatoryVarsArr->Array.map(ctxIntToSymExn(ctx, _)),
                    varTypes: mandatoryVarsArr->Array.map(getTypeOfVarExn(ctx, _)),
                    varHyps: getAsrtVarHyps(numOfVars, hyps),
                    numOfVars,
                    numOfArgs: mandatoryHypotheses->Array.length,
                    descr,
                    descrNorm,
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
                                disj: mandatoryDisj->disjToArr->Array.map(ctxIntsToStrExn(ctx, _)),
                                hyps: mandatoryHypotheses->Array.map(hyp => ctx->ctxIntsToStrExn(hyp.expr)),
                                asrt: ctx->ctxIntsToStrExn(asrt),
                            })
                        } else {
                            None
                        },
                    usageCnt: -1,
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
    ~descrRegexToDisc:option<RegExp.t>=?,
    ~labelRegexToDisc:option<RegExp.t>=?,
    ~descrRegexToDepr:option<RegExp.t>=?,
    ~labelRegexToDepr:option<RegExp.t>=?
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
        ~labelRegexToDepr?
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
    ~descrRegexToDisc:option<RegExp.t>=?,
    ~labelRegexToDisc:option<RegExp.t>=?,
    ~descrRegexToDepr:option<RegExp.t>=?,
    ~labelRegexToDepr:option<RegExp.t>=?
):unit => {
    let isComment = ref(false)
    switch stmt {
        | Comment({text}) => {
            addComment(ctx, text)
            isComment := true
        }
        | Const({symbols}) => symbols->Array.forEach(addConst(ctx, _))
        | Block(_) => raise(MmException({msg:`Block statements are not accepted by applySingleStmt().`}))
        | Var({symbols}) => symbols->Array.forEach(addVar(ctx, _))
        | Disj({vars}) => addDisj(ctx, vars)
        | Floating({label, expr}) => addFloating(ctx, ~label, ~exprStr=expr)
        | Essential({label, expr}) => addEssential(ctx, ~label, ~exprStr=expr)
        | Axiom({label, expr}) => {
            addAssertion(
                ctx, ~isAxiom=true, ~label, ~exprStr=expr, ~proof=None, 
                ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?
            )
        }
        | Provable({label, expr, proof}) => {
            addAssertion(
                ctx, ~isAxiom=false, ~label, ~exprStr=expr, ~proof, 
                ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?
            )
        }
    }
    if (!isComment.contents) {
        ctx.contents.lastComment = None
    }
}

let loadContext = (
    ast, 
    ~initialContext=?,
    ~stopBefore="",
    ~stopAfter="",
    ~onPreProcess: option<(mmContext,MM_parser.stmt)=>unit>=?,
    ~expectedNumOfAssertions=-1, 
    ~descrRegexToDisc:option<RegExp.t>=?,
    ~labelRegexToDisc:option<RegExp.t>=?,
    ~descrRegexToDepr:option<RegExp.t>=?,
    ~labelRegexToDepr:option<RegExp.t>=?,
    ~onProgress= _=>(), 
    ~debug:bool=false
) => {
    let expectedNumOfAssertionsF = expectedNumOfAssertions->Belt_Int.toFloat
    let assertionsProcessed = ref(0.)
    let progressTracker = progressTrackerMake(~step=0.1, ~onProgress)

    let onAsrtProcess = () => {
        if (expectedNumOfAssertions > 0) {
            assertionsProcessed.contents = assertionsProcessed.contents +. 1.
            progressTracker->progressTrackerSetCurrPct(assertionsProcessed.contents /. expectedNumOfAssertionsF)
        }
    }

    let (ctx, _) = traverseAst(
        switch initialContext {
            | Some(ctx) => ctx
            | None => createContext(~debug)
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
                        ctx, stmt, ~descrRegexToDisc?, ~labelRegexToDisc?, ~descrRegexToDepr?, ~labelRegexToDepr?
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
        }
    )
    ctx
}

let generateNewVarNames = (
    ~ctx:mmContext, 
    ~types:array<int>, 
    ~typeToPrefix:Belt_MapString.t<string>=Belt_MapString.empty,
    ~reservedNames:option<Belt_HashSetString.t>=?
): array<string> => {
    let prefixToCnt = Belt_HashMapString.make(~hintSize=typeToPrefix->Belt_MapString.size)

    let getCnt = prefix => prefixToCnt->Belt_HashMapString.get(prefix)->Belt.Option.getWithDefault(0)
    let incCnt = prefix => prefixToCnt->Belt_HashMapString.set(prefix,getCnt(prefix)+1)

    let maxI = types->Array.length - 1
    let res = []
    for i in 0 to maxI {
        let typeStr = ctx->ctxIntToSymExn(types->Array.getUnsafe(i))
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
        res->Array.push(newName.contents)
    }
    res
}

let generateNewLabels = (
    ~ctx:mmContext, 
    ~prefix:string, 
    ~amount:int,
    ~reservedLabels:option<Belt_HashSetString.t>=?,
    ~checkHypsOnly:bool=false
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
        res->Array.push(newName.contents)
    }
    res
}

let renumberConst = (constRenum:Belt_HashMapInt.t<int>, c:int):int => {
    constRenum->Belt_HashMapInt.get(c)->Belt_Option.getWithDefault(c)
}

let renumberConstsInExpr = (constRenum:Belt_HashMapInt.t<int>, expr:expr):unit => {
    let maxI = expr->Array.length-1
    for i in 0 to maxI {
        let sym = expr->Array.getUnsafe(i)
        if (sym < 0) {
            expr[i] = constRenum->renumberConst(sym)
        }
    }
}

let moveConstsToBegin = (ctx:mmContext, firstConsts:array<int>):unit => {
    let rootCtx = ctx.contents.root->Belt_Option.getExn
    let newConstOrder:array<int> = firstConsts->Array.copy
    for i in -1 downto -(rootCtx.consts->Array.length-1) {
        if (!(newConstOrder->Array.includes(i))) {
            newConstOrder->Array.push(i)
        }
    }
    let oldConstOrder:array<string> = rootCtx.consts->Array.copy
    let constRenum = Belt_HashMapInt.make(~hintSize=rootCtx.consts->Array.length)
    newConstOrder->Array.forEachWithIndex((symOldInt,i) => {
        let sym = oldConstOrder->Array.getUnsafe(-symOldInt)
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
        frame.hyps->Array.forEach(hyp => constRenum->renumberConstsInExpr(hyp.expr))
        constRenum->renumberConstsInExpr(frame.asrt)
        constRenum->renumberConstsInExpr(frame.varTypes)
        None
    })->ignore
    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
        let maxI = ctx.vars->Array.length-1
        for i in 0 to maxI {
            let var = ctx.varsBaseIdx + i
            switch ctx.varTypes->Belt_HashMapInt.get(var) {
                | None => ()
                | Some(oldTyp) => ctx.varTypes->Belt_HashMapInt.set(var, constRenum->renumberConst(oldTyp))
            }
        }
        ctx.exprToHyp->Belt_HashMapInt.clear
        ctx.hyps->Array.forEach(exprToHypAdd(ctx, _))
        None
    })->ignore
}

let frameUpdate = (
    frame:frame,
    ~removeAsrtDescr:bool,
    ~removeProofs:bool,
    ~frmUsageCounts:Belt_HashMapString.t<ref<int>>,
):frame => {
    let usageCnt = switch frmUsageCounts->Belt_HashMapString.get(frame.label) {
        | None => frame.usageCnt
        | Some(cnt) => cnt.contents
    }
    {
        ...frame,
        descr: if (removeAsrtDescr) {None} else {frame.descr},
        descrNorm: if (removeAsrtDescr) {None} else {frame.descrNorm},
        proof: if (removeProofs) {None} else {frame.proof},
        usageCnt, 
    }
}

let rec ctxUpdate = (
    ctx:mmContextContents,
    ~removeAsrtDescr:bool,
    ~removeProofs:bool,
    ~frmUsageCounts:Belt_HashMapString.t<ref<int>>,
):(mmContextContents, mmContextContents) => {
    let update = ctx => {
        {
            ...ctx,
            lastComment: if (removeAsrtDescr) {None} else {ctx.lastComment},
            frames: ctx.frames->Belt_HashMapString.toArray->Array.map(((label,frame)) => {
                (
                    label,
                    frame->frameUpdate(~removeAsrtDescr, ~removeProofs, ~frmUsageCounts)
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
            let res = update(ctx)
            res.root = Some(res)
            (res, res)
        }
        | Some(parent) => {
            let (newRoot, newParent) = ctxUpdate(parent, ~removeAsrtDescr, ~removeProofs, ~frmUsageCounts)
            let res = {
                ...update(ctx),
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
        let first = frame.asrt->Array.getUnsafe(1)
        if (first < 0) {
            canBeFirst->Belt_HashSetInt.add(first)
        }
        let last = frame.asrt->Array.getUnsafe(frame.asrt->Array.length-1)
        if (last < 0) {
            canBeLast->Belt_HashSetInt.add(last)
        }
        None
    })->ignore
    let canBeFirstAndLast = canBeFirst->Belt_HashSetInt.toArray
        ->Array.filter(Belt_HashSetInt.has(canBeLast, _))
        ->Belt_HashSetInt.fromArray
    canBeFirstAndLast->Belt_HashSetInt.forEach(canBeFirst->Belt_HashSetInt.remove)
    canBeFirstAndLast->Belt_HashSetInt.forEach(canBeLast->Belt_HashSetInt.remove)

    let parenInts = parens->getSpaceSeparatedValuesAsArray
        ->Array.map(ctxSymToInt(ctx, _))
        ->Array.filter(intOpt => intOpt->Belt_Option.mapWithDefault(false, i => i < 0))
        ->Array.map(Belt_Option.getExn(_))
    parenInts->Array.forEach(i => {
        canBeFirst->Belt_HashSetInt.remove(i)
        canBeFirstAndLast->Belt_HashSetInt.remove(i)
        canBeLast->Belt_HashSetInt.remove(i)
    })

    let remainingConsts = allConsts->Belt_HashSetInt.toArray->Array.filter(i => {
        !(parenInts->Array.includes(i))
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

    let parenMin = -(parenInts->Array.length)

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
    ~removeAsrtDescr:bool,
    ~removeProofs:bool,
    ~updateUsageCntForFrames:bool,
):mmContext => {
    let frmUsageCounts:Belt_HashMapString.t<ref<int>> = if !updateUsageCntForFrames {
        Belt_HashMapString.make(~hintSize=0)
    } else {
        let allFrms = ctx->getAllFramesArr
        let cnts:Belt_HashMapString.t<ref<int>> = Belt_HashMapString.fromArray(
            allFrms->Array.map(frm => (frm.label, ref(0)))
        )
        allFrms->Array.forEach(frm => {
            switch frm.proof {
                | Some(Compressed({labels})) | Some(Uncompressed({labels})) => {
                    labels->Belt_HashSetString.fromArray->Belt_HashSetString.forEach(label => {
                        switch cnts->Belt_HashMapString.get(label) {
                            | Some(cnt) => cnt := cnt.contents + 1
                            | None => ()
                        }
                    })
                }
                | None => ()
            }
        })
        cnts
    }
    let (_,ctx) = ctx.contents->ctxUpdate( ~removeAsrtDescr, ~removeProofs, ~frmUsageCounts, )
    let resCtx = ref(ctx)
    let {allConsts} = resCtx->ctxGetOptimizedConstsOrder(~parens)
    resCtx->moveConstsToBegin(resCtx->ctxSymsToIntsExn(allConsts))
    resCtx
}