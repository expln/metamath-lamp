open MM_parser
open MM_progress_tracker

// cdblk #types ===========================================================================================

type expr = array<int>

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

type frame = {
    disj: Belt.Map.Int.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    frameVarToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
}

type mutableMapStr<'v> = Belt.HashMap.String.t<'v>
type mutableMapInt<'v> = Belt.HashMap.Int.t<'v>
type mutableSetInt = Belt.HashSet.Int.t

type disjMutable = mutableMapInt<mutableSetInt>

type rec mmContextContents = {
    mutable root: option<mmContextContents>,
    parent: option<mmContextContents>,
    consts: array<string>,
    varsBaseIdx: int,
    vars: array<string>,
    symToInt: mutableMapStr<int>,
    disj: disjMutable,
    hyps: array<hypothesis>,
    symToHyp: mutableMapStr<hypothesis>,
    mutable lastComment: string,
    frames: mutableMapStr<frame>,
    frameDescr: mutableMapStr<string>,
}

type mmContext = ref<mmContextContents>

// cdblk #utils ===========================================================================================

let mutableMapStrMake = () => {
    Belt.HashMap.String.make(~hintSize=16)
}

let mutableMapStrPut = (map,k,v) => {
    map->Belt.HashMap.String.set(k,v)
}

let mutableMapStrHas = (map,k) => {
    map->Belt.HashMap.String.has(k)
}

let mutableMapStrGet = (map,k) => {
    map->Belt.HashMap.String.get(k)
}

let mutableMapStrForEach = (map,func) => {
    map->Belt.HashMap.String.forEach(func)
}

let mutableMapStrMakeFromArray = arr => {
    let map = mutableMapStrMake()
    arr->Js_array2.forEach(((k,v)) => map->mutableMapStrPut(k,v))
    map
}

let mutableMapStrClone = (orig:mutableMapStr<'v>, cloneValue:'v=>'v) => {
    let map = mutableMapStrMake()
    orig->mutableMapStrForEach((k,v) => map->mutableMapStrPut(k,cloneValue(v)))
    map
}

let mutableMapStrClear = map => {
    map->Belt.HashMap.String.clear
}

let mutableMapIntMake = () => {
    Belt.HashMap.Int.make(~hintSize=16)
}

let mutableMapIntSize = (map) => {
    map->Belt.HashMap.Int.size
}

let mutableMapIntPut = (map,k,v) => {
    map->Belt.HashMap.Int.set(k,v)
}

let mutableMapIntHas = (map,k) => {
    map->Belt.HashMap.Int.has(k)
}

let mutableMapIntGet = (map,k) => {
    map->Belt.HashMap.Int.get(k)
}

let mutableMapIntForEach = (map,func) => {
    map->Belt.HashMap.Int.forEach(func)
}

let mutableMapIntForEachI = (map:mutableMapInt<'v>,func:(int,'v,int)=>unit) => {
    let i = ref(0)
    map->Belt.HashMap.Int.forEach((k,v) => {
        func(k,v,i.contents)
        i.contents = i.contents + 1
    })
}

let mutableMapIntMakeFromArray = arr => {
    let map = mutableMapIntMake()
    arr->Js_array2.forEach(((k,v)) => map->mutableMapIntPut(k,v))
    map
}

let mutableMapIntToArr = (map, valueMapper) => {
    let res = []
    map->mutableMapIntForEach((k,v) => res->Js.Array2.push((k, valueMapper(v)))->ignore)
    res
}

let mutableMapIntClone = (orig:mutableMapInt<'v>, cloneValue:'v=>'v) => {
    let map = mutableMapIntMake()
    orig->mutableMapIntForEach((k,v) => map->mutableMapIntPut(k,cloneValue(v)))
    map
}

let mutableSetIntMake = () => {
    Belt.HashSet.Int.make(~hintSize=16)
}

let mutableSetIntSize = (set) => {
    set->Belt.HashSet.Int.size
}

let mutableSetIntAdd = (set,k) => {
    set->Belt.HashSet.Int.add(k)
}

let mutableSetIntHas = (set,k) => {
    set->Belt.HashSet.Int.has(k)
}

let mutableSetIntForEach = (set,func) => {
    set->Belt.HashSet.Int.forEach(func)
}

let mutableSetIntForEachI = (set,func) => {
    let i = ref(0)
    set->Belt.HashSet.Int.forEach(e => {
        func(e,i.contents)
        i.contents = i.contents + 1
    })
}

let mutableSetIntMakeFromArray = arr => {
    let set = mutableSetIntMake()
    arr->Js_array2.forEach(v => set->mutableSetIntAdd(v))
    set
}

let mutableSetIntToArray = set => {
    let arr = Expln_utils_common.createArray(set->mutableSetIntSize)
    let i = ref(0)
    set->mutableSetIntForEach(e => {
        arr[i.contents] = e
        i.contents = i.contents + 1
    })
    arr
}

let mutableSetIntClone = (orig:mutableSetInt) => {
    let set = mutableSetIntMake()
    orig->mutableSetIntForEach(set->mutableSetIntAdd)
    set
}

let disjAddPair = (disjMap:disjMutable, n, m) => {
    if (n != m) {
        let min = if (n <= m) {n} else {m}
        let max = if (n <= m) {m} else {n}

        switch disjMap->mutableMapIntGet(min) {
            | None => disjMap->mutableMapIntPut(min, mutableSetIntMakeFromArray([max]))
            | Some(set) => set->mutableSetIntAdd(max)
        }
    }
}

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

module ExprCmp = Belt.Id.MakeComparable({
    type t = expr
    let cmp = (e1,e2) => {
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
})

module ExprHash = Belt.Id.MakeHashable({
    type t = expr

    // https://stackoverflow.com/questions/194846/is-there-hash-code-function-accepting-any-object-type
    let hash = %raw(`
        expr => {
            let hash = 0;
            for (let i = 0; i < expr.length; i++) {
                hash = ( ( hash << 5 ) - hash ) + expr[i];
                hash = hash & hash;  // Convert to 32-bit integer
            }
            return hash;
        }
    `)
    let eq = exprEq
})

// cdblk #search ===========================================================================================

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

let rec isConstPriv: (mmContextContents,string) => bool = (ctx, sym) => {
    (ctx.root->Belt.Option.getExn).symToInt
        ->mutableMapStrGet(sym)
        ->Belt_Option.map(i => i < 0)
        ->Belt_Option.getWithDefault(false)
}

let isConst: (mmContext,string) => bool = (ctx, sym) => isConstPriv(ctx.contents, sym)

let isVarPriv: (mmContextContents,string) => bool = (ctx, sym) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->mutableMapStrGet(sym)
    })
        ->Belt_Option.map(i => 0 <= i)
        ->Belt_Option.getWithDefault(false)
}

let isVar: (mmContext,string) => bool = (ctx, sym) => isVarPriv(ctx.contents, sym)

let isHypPriv: (mmContextContents,string) => bool = (ctx, label) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToHyp->mutableMapStrGet(label)
    })->Belt_Option.isSome
}

let isHyp: (mmContext,string) => bool = (ctx, label) => isHypPriv(ctx.contents, label)

let isAsrtPriv: (mmContextContents,string) => bool = (ctx, label) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.frames->mutableMapStrGet(label)
    })->Belt_Option.isSome
}

let isAsrt: (mmContext,string) => bool = (ctx, label) => isAsrtPriv(ctx.contents, label)

let disjContains = (disj:disjMutable, n, m):bool => {
    let min = if (n <= m) {n} else {m}
    let max = if (n <= m) {m} else {n}
    switch disj->mutableMapIntGet(min) {
        | None => false
        | Some(ms) => ms->mutableSetIntHas(max)
    }
}

let disjNumOfGroups = disjMutable => disjMutable->mutableMapIntSize

let disjForEachArr = (disjMutable, consumer) => {
    disjMutable->mutableMapIntForEach((n,ms) => Belt_Array.concat([n], ms->mutableSetIntToArray)->consumer)
}

let disjForEach = (disjMutable, consumer) => {
    disjMutable->mutableMapIntForEach((n,ms) => {
        ms->mutableSetIntForEach(m => {
            consumer(n,m)
        })
    })
}

let disjIsEmpty = disjMutable => {
    disjMutable->mutableMapIntSize == 0
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

let getHypothesisPriv = (ctx:mmContextContents,label):option<hypothesis> => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToHyp->mutableMapStrGet(label)
    })
}

let getHypothesis: (mmContext, string) => option<hypothesis> = (ctx, sym) => getHypothesisPriv(ctx.contents, sym)

let getFramePriv = (ctx:mmContextContents,label):option<frame> => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.frames->mutableMapStrGet(label)
    })
}

let getFrame = (ctx:mmContext,label):option<frame> => getFramePriv(ctx.contents,label)

let getLocalVars: mmContext => array<string> = ctx => {
    ctx.contents.vars->Js_array2.copy
}

let getLocalHyps: mmContext => array<hypothesis> = ctx => {
    ctx.contents.hyps->Js_array2.copy
}

let getNumOfVars = ctx => {
    ctx.contents.varsBaseIdx + ctx.contents.vars->Js_array2.length
}

let forEachHypothesisInDeclarationOrderPriv: (mmContextContents, hypothesis => option<'a>) => option<'a> = (ctx, consumer) => {
    ctx->forEachCtxInDeclarationOrder(ctx => {
        Expln_utils_common.arrForEach(ctx.hyps, consumer)
    })
}

let forEachHypothesisInDeclarationOrder: (mmContext, hypothesis => option<'a>) => option<'a> = 
    (ctx, consumer) => forEachHypothesisInDeclarationOrderPriv(ctx.contents, consumer)

let ctxSymToIntPriv = (ctx:mmContextContents,sym) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->mutableMapStrGet(sym)
    })
}

let ctxSymToIntExnPriv = (ctx:mmContextContents,sym) => {
    switch ctxSymToIntPriv(ctx,sym) {
        | Some(i) => i
        | None => raise(MmException({msg:`The symbol '${sym}' is not declared.`}))
    }
}

let ctxSymsToIntsExnPriv: (mmContextContents,array<string>) => expr = (ctx, symbols) => {
    symbols->Js_array2.map(ctx->ctxSymToIntExnPriv)
}

let ctxSymsToIntsExn: (mmContext,array<string>) => expr = (ctx, symbols) => ctxSymsToIntsExnPriv(ctx.contents, symbols)

let ctxStrToIntsExn: (mmContext,string) => expr = (ctx, str) => ctxSymsToIntsExnPriv(ctx.contents, str->getSpaceSeparatedValuesAsArray)

let ctxSymToInt = (ctx:mmContext, sym:string):option<int> => {
    ctxSymToIntPriv(ctx.contents,sym)
}

let ctxSymToIntExn = (ctx:mmContext, sym:string):int => {
    ctx.contents->ctxSymToIntExnPriv(sym)
}

let ctxIntToSymPriv = (ctx:mmContextContents,i):option<string> => {
    if (i < 0) {
        (ctx.root->Belt.Option.getExn).consts->Belt_Array.get(-i)
    } else {
        ctx->forEachCtxInReverseOrder(ctx => {
            if (i < ctx.varsBaseIdx) {
                None
            } else {
                Some(ctx.vars[i-ctx.varsBaseIdx])
            }
        })
    }
}

let ctxIntToSymExnPriv = (ctx:mmContextContents,i) => {
    ctxIntToSymPriv(ctx,i)->Belt.Option.getExn
}

let ctxIntToSymExn = (ctx:mmContext,i) => ctxIntToSymExnPriv(ctx.contents,i)

let ctxIntsToSymsExn = (ctx,expr) => expr->Js_array2.map(ctxIntToSymExn(ctx, _))

let ctxIntToSym = (ctx:mmContext,i) => ctxIntToSymPriv(ctx.contents,i)

let ctxIntsToStrExnPriv: (mmContextContents, expr) => string = (ctx, expr) => {
    expr->Js_array2.map(ctxIntToSymExnPriv(ctx, _))->Js_array2.joinWith(" ")
}

let ctxIntsToStrExn: (mmContext, expr) => string = (ctx, expr) => ctxIntsToStrExnPriv(ctx.contents,expr)

let frmIntToSymExnPriv: (mmContextContents, frame, int) => string = (ctx, frame, i) => {
    if (i < 0) {ctx->ctxIntToSymExnPriv(i)} else {frame.frameVarToSymb->Belt_MapInt.getExn(i)}
}

let frmIntToSymExn: (mmContext, frame, int) => string = (ctx, frame, i) => frmIntToSymExnPriv(ctx.contents,frame,i)

let frmIntsToStrExnPriv: (mmContextContents, frame, expr) => string = (ctx, frame, expr) => {
    expr->Js_array2.map(frmIntToSymExnPriv(ctx, frame, _))->Js_array2.joinWith(" ")
}

let frmIntsToStrExn: (mmContext, frame, expr) => string = (ctx, frame, expr) => frmIntsToStrExnPriv(ctx.contents, frame, expr)

let getTypeOfVarPriv = (ctx, varInt) => {
    ctx->forEachHypothesisInDeclarationOrderPriv(hyp => {
        if (hyp.typ == F && hyp.expr[1] == varInt) {
            Some(hyp.expr[0])
        } else {
            None
        }
    })
}

let getTypeOfVar = (ctx, varInt) => getTypeOfVarPriv(ctx.contents, varInt)

let getTypeOfVarExn = (ctx, varInt) => {
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

let extractMandatoryVariables = (ctx:mmContextContents,asrt, ~skipHyps:bool=false, ()): mutableSetInt => {
    let res = mutableSetIntMake()
    if (!skipHyps) {
        ctx->forEachHypothesisInDeclarationOrderPriv(hyp => {
            if (hyp.typ == E) {
                hyp.expr->Js_array2.forEach(i => if i >= 0 {res->mutableSetIntAdd(i)})
            }
            None
        })->ignore
    }
    asrt->Js_array2.forEach(i => if i >= 0 {res->mutableSetIntAdd(i)})
    res
}

let extractMandatoryDisj = (ctx:mmContextContents, mandatoryVars:mutableSetInt): mutableMapInt<mutableSetInt> => {
    let mandatoryDisj = mutableMapIntMake()
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.disj->mutableMapIntForEach((n,ms) => {
            if (mandatoryVars->mutableSetIntHas(n)) {
                ms->mutableSetIntForEach(m => {
                    if (mandatoryVars->mutableSetIntHas(m)) {
                        disjAddPair(mandatoryDisj, n, m)
                    }
                })
            }
        })->ignore
        None
    })->ignore
    mandatoryDisj
}

let extractMandatoryHypotheses = (ctx:mmContextContents, mandatoryVars:mutableSetInt, ~skipHyps:bool=false, ()):array<hypothesis> => {
    let res = []
    ctx->forEachCtxInDeclarationOrder(ctx=>{
        ctx.hyps->Js.Array2.forEach(hyp => {
            if ((hyp.typ == E && !skipHyps) || hyp.typ == F && mandatoryVars->mutableSetIntHas(hyp.expr[1])) {
                res->Js.Array2.push(hyp)->ignore
            }
        })
        None
    })->ignore
    res
}

let getMandHypsPriv:(mmContextContents, expr) => array<hypothesis> = (ctx, expr) => {
    let mandatoryVars: mutableSetInt = extractMandatoryVariables(ctx, expr, ())
    extractMandatoryHypotheses(ctx, mandatoryVars, ())
}

let getMandHyps:(mmContext, expr) => array<hypothesis> = (ctx, expr) => getMandHypsPriv(ctx.contents, expr)

let getAllHyps = (ctx):Belt_MapString.t<hypothesis> => {
    let hyps = []
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        hyps->Js.Array2.push(hyp)->ignore
        None
    })->ignore
    Belt_MapString.fromArray(hyps->Js_array2.map(hyp => (hyp.label, hyp)))
}

let forEachFramePriv: (mmContextContents, frame => option<'a>) => option<'a> = (ctx, consumer) => {
    ctx->forEachCtxInDeclarationOrder(ctx => {
        let result = ref(None)
        ctx.frames->mutableMapStrForEach((_,frm) => {
            if (result.contents->Belt_Option.isNone) {
                result.contents = consumer(frm)
            }
        })
        result.contents
    })
}

let forEachFrame: (mmContext, frame => option<'a>) => option<'a> = (ctx, consumer) => forEachFramePriv(ctx.contents, consumer)

let rec getNestingLevelPriv: mmContextContents => int = ctx => {
    switch ctx.parent {
        | None => 0
        | Some(pCtx) => 1 + getNestingLevelPriv(pCtx)
    }
}

let getNestingLevel: mmContext => int = ctx => getNestingLevelPriv(ctx.contents)

let findParentheses: (mmContext, ~onProgress:float=>unit=?, unit) => array<int> = (ctx, ~onProgress=?, ()) => {
    let ctx = ctx.contents

    let getAllExprs = ctx => {
        let allExpr = []
        ctx->forEachFramePriv(frame => {
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
        open MM_parenCounter
        let res = ref(true)
        let openUsed = ref(false)
        let closeUsed = ref(false)
        let parenCnt = parenCntMake([openSym, closeSym])
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
        ->Js.Array2.filter(ctx->isConstPriv)
        ->ctxSymsToIntsExnPriv(ctx, _)
        ->Js_array2.concat(
            Belt_Array.range(
                1,
                (ctx.root->Belt.Option.getExn).consts->Js.Array2.length - 1
            )->Js_array2.map(i => -i)
        )

    let maxC = allConsts->Js.Array2.length - 2
    let maxCF = maxC->Belt_Int.toFloat
    let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))
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
        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
            c->Belt_Int.toFloat /. maxCF
        )
    }
    foundParens
}

// cdblk #update ===========================================================================================

let disjMutableMake = () => {
    mutableMapIntMake()
}

let getAllDisj = (ctx:mmContext):disjMutable => {
    let disj = disjMutableMake()
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.disj->mutableMapIntForEach((n,ms) => {
            ms->mutableSetIntForEach(m => {
                disj->disjAddPair(n,m)
            })
        })
        None
    })->ignore
    disj
}

let getLocalDisj = (ctx:mmContext):disjMutable => {
    let disj = disjMutableMake()
    ctx.contents.disj->mutableMapIntForEach((n,ms) => {
        ms->mutableSetIntForEach(m => {
            disj->disjAddPair(n,m)
        })
    })
    disj
}

let createContext: (~parent:mmContext=?, ()) => mmContext = (~parent=?, ()) => {
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
            symToInt: mutableMapStrMake(),
            disj: disjMutableMake(),
            hyps: [],
            symToHyp: mutableMapStrMake(),
            lastComment: "",
            frames: mutableMapStrMake(),
            frameDescr: mutableMapStrMake(),
        }
    )
    switch pCtxContentsOpt {
        | None => ctx.contents.root = Some(ctx.contents)
        | Some(pCtxContents) => ctx.contents.root = pCtxContents.root
    }
    ctx
}

let openChildContext: mmContext => unit = ctx => {
    ctx.contents = createContext(~parent=ctx, ()).contents
}

let resetToParentContext: mmContext => unit = ctx => {
    ctx.contents = switch ctx.contents.parent {
        | None => raise(MmException({msg:`Cannot reset the root context.`}))
        | Some(parent) => parent
    }
}

let addComment: (mmContext,string) => unit = (ctx,str) => {
    ctx.contents.lastComment = str
}

let addConstPriv: (mmContextContents,string) => unit = (ctx,cName) => {
    if (ctx.parent->Belt_Option.isSome) {
        raise(MmException({msg:`An attempt to declare a constant '${cName}' in an inner block.`}))
    } else if (isConstPriv(ctx, cName) || isVarPriv(ctx, cName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${cName}' as a constant.`}))
    } else {
        ctx.symToInt->mutableMapStrPut(cName, -(ctx.consts->Js_array2.length))
        ctx.consts->Js_array2.push(cName)->ignore
    }
}

let addConst: (mmContext,string) => unit = (ctx,cName) => {
    ctx.contents->addConstPriv(cName)
}

let closeChildContext: mmContext => unit = ctx => {
    ctx.contents = switch ctx.contents.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            ctx.contents.frames->mutableMapStrForEach((k,v) => parent.frames->mutableMapStrPut(k,v))
            ctx.contents.frameDescr->mutableMapStrForEach((k,v) => parent.frameDescr->mutableMapStrPut(k,v))
            parent
        }
    }
}

let addVar: (mmContext,string) => unit = (ctx,vName) => {
    let ctx = ctx.contents
    if (isConstPriv(ctx, vName) || isVarPriv(ctx, vName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${vName}' as a variable.`}))
    } else {
        ctx.symToInt->mutableMapStrPut(vName, ctx.varsBaseIdx + ctx.vars->Js_array2.length)
        ctx.vars->Js_array2.push(vName)->ignore
    }
}

let addDisjPair = (ctx:mmContextContents, n, m) => disjAddPair(ctx.disj,n,m)

let addDisj: (mmContext,array<string>) => unit = (ctx, vars) => {
    let ctx = ctx.contents
    switch vars->Js_array2.find(sym => !(ctx->isVarPriv(sym))) {
        | Some(sym) => raise(MmException({msg:`The symbol '${sym}' is not a variable but it is used in a disjoint statement.`}))
        | None => {
            let varInts = vars->Js_array2.map(ctx->ctxSymToIntExnPriv)
            let maxIdx = varInts->Js_array2.length - 1
            for i in 0 to maxIdx {
                for j in i+1 to maxIdx {
                    ctx->addDisjPair(varInts[i],varInts[j])
                }
            }
        }
    }
}

let addFloating: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    let ctx = ctx.contents
    if (exprStr->Js_array2.length != 2) {
        raise(MmException({msg:`Length of a floating expression must be 2.`}))
    } else if (!(ctx->isConstPriv(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in a floating expression must be a constant.`}))
    } else if (!(ctx->isVarPriv(exprStr[1]))) {
        raise(MmException({msg:`The second symbol in a floating expression must be a variable.`}))
    } else if (ctx->getHypothesisPriv(label)->Belt_Option.isSome || ctx->getFramePriv(label)->Belt_Option.isSome) {
        raise(MmException({msg:`Cannot reuse the label '${label}'`}))
    } else {
        let varName = exprStr[1]
        let varInt = ctx->ctxSymToIntExnPriv(varName)
        if (ctx->getTypeOfVarPriv(varInt)->Belt_Option.isSome) {
            raise(MmException({msg:`Cannot redefine typecode for the variable '${varName}'`}))
        } else {
            let expr = ctxSymsToIntsExnPriv(ctx, exprStr)
            let hyp = {typ:F, label, expr}
            ctx.hyps->Js_array2.push(hyp)->ignore
            ctx.symToHyp->mutableMapStrPut(label, hyp)
        }
    }
}

let addEssential: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    let ctx = ctx.contents
    if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an essential expression must be at least 1.`}))
    } else if (!(ctx->isConstPriv(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an essential expression must be a constant.`}))
    } else {
        let expr = ctxSymsToIntsExnPriv(ctx, exprStr)
        let hyp = {typ:E, label, expr}
        ctx.hyps->Js_array2.push(hyp)->ignore
        ctx.symToHyp->mutableMapStrPut(label, hyp)
   }
}

let renumberVarsInDisj = (disj: mutableMapInt<mutableSetInt>, renumbering: Belt_MapInt.t<int>): Belt_MapInt.t<Belt_SetInt.t> => {
    let disjArr = Expln_utils_common.createArray(disj->mutableMapIntSize)
    disj->mutableMapIntForEachI((n,ms,ni) => {
        let msArr = Expln_utils_common.createArray(ms->mutableSetIntSize)
        ms->mutableSetIntForEachI((m,mi) => {
            msArr[mi] = switch renumbering->Belt_MapInt.get(m) {
                | None => raise(MmException({msg:`[1] Cannot determine frame variable for the context variable ${m->Belt_Int.toString}`}))
                | Some(fv) => fv
            }
        })
        disjArr[ni] = (
            switch renumbering->Belt_MapInt.get(n) {
                | None => raise(MmException({msg:`[2] Cannot determine frame variable for the context variable ${n->Belt_Int.toString}`}))
                | Some(fv) => fv
            }, 
            Belt_Set.Int.fromArray(msArr)
        )
    })
    Belt_MapInt.fromArray(disjArr)
}

let renumberVarsInExpr = (expr: expr, renumbering: Belt_MapInt.t<int>): expr => {
    expr->Js_array2.map(i => if (i<0) {i} else {renumbering->Belt_MapInt.getExn(i)})
}

let renumberVarsInHypothesis = (hyp: hypothesis, renumbering: Belt_MapInt.t<int>): hypothesis => {
    ...hyp,
    expr: renumberVarsInExpr(hyp.expr, renumbering)
}

let createFrameVarToSymbMap = (ctx:mmContextContents, mandatoryHypotheses:array<hypothesis>, asrt, renumbering: Belt_MapInt.t<int>): Belt_MapInt.t<string> => {
    let allVars = mutableSetIntMake()
    mandatoryHypotheses->Js.Array2.forEach(hyp => {
        hyp.expr->Js_array2.forEach(i => {
            if (i >= 0) {
                allVars->mutableSetIntAdd(i)
            }
        })
    })
    asrt->Js_array2.forEach(i => {
        if (i >= 0) {
            allVars->mutableSetIntAdd(i)
        }
    })
    Belt_MapInt.fromArray(
        allVars->mutableSetIntToArray->Js_array2.map(v => (renumbering->Belt_MapInt.getExn(v), ctxIntToSymExnPriv(ctx,v)))
    )
}

let extractVarTypes = (mandatoryHypotheses:array<hypothesis>, renumbering: Belt_MapInt.t<int>): array<int> => {
    let varTypes = Expln_utils_common.createArray(renumbering->Belt_MapInt.size)
    mandatoryHypotheses->Js_array2.forEach(hyp => {
        if (hyp.typ == F) {
            varTypes[renumbering->Belt_MapInt.getExn(hyp.expr[1])] = hyp.expr[0]
        }
    })
    varTypes
}

let createFramePriv: (mmContextContents, string, array<string>, ~skipHyps:bool=?, ~skipFirstSymCheck:bool=?, ()) => (frame,string) = (
    ctx, label, exprStr, ~skipHyps:bool=false, ~skipFirstSymCheck:bool=false, ()
) => {
    if (label->Js_string2.trim == "") {
        raise(MmException({msg:`Cannot use empty string as a label.`}))
    } else if (ctx->getHypothesisPriv(label)->Belt_Option.isSome || ctx->getFramePriv(label)->Belt_Option.isSome) {
        raise(MmException({msg:`Cannot reuse the label '${label}'`}))
    } else if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an assertion expression must be at least 1.`}))
    } else if (!skipFirstSymCheck && !(ctx->isConstPriv(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an assertion expression must be a constant.`}))
    } else {
        switch exprStr->Js_array2.find(sym => ctx->ctxSymToIntPriv(sym)->Belt_Option.isNone) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let asrt: expr = exprStr->Js_array2.map(ctxSymToIntExnPriv(ctx, _))
                let mandatoryVars: mutableSetInt = extractMandatoryVariables(ctx, asrt, ~skipHyps, ())
                let mandatoryDisj: mutableMapInt<mutableSetInt> = extractMandatoryDisj(ctx, mandatoryVars)
                let mandatoryHypotheses: array<hypothesis> = extractMandatoryHypotheses(ctx, mandatoryVars, ~skipHyps, ())
                let varRenumbering: Belt_MapInt.t<int> = mandatoryVars
                                                            ->mutableSetIntToArray
                                                            ->Js_array2.mapi((cv,fv) => (cv,fv))
                                                            ->Belt_MapInt.fromArray
                let varTypes = extractVarTypes(mandatoryHypotheses, varRenumbering)
                let hyps = mandatoryHypotheses->Js_array2.map(renumberVarsInHypothesis(_, varRenumbering))
                let frame = {
                    disj: mandatoryDisj->renumberVarsInDisj(varRenumbering),
                    hyps,
                    asrt: asrt->renumberVarsInExpr(varRenumbering),
                    label,
                    frameVarToSymb: createFrameVarToSymbMap(ctx, mandatoryHypotheses, asrt, varRenumbering),
                    varTypes,
                    numOfVars: varTypes->Js_array2.length,
                    numOfArgs: hyps->Js_array2.length
                }
                (frame, ctx.lastComment)
            }
        }
    }
}

let createFrame = ( ctx, label, exprStr, ~skipHyps:bool=false, ~skipFirstSymCheck:bool=false, () ) => 
    createFramePriv(ctx.contents, label, exprStr, ~skipHyps, ~skipFirstSymCheck, ())

let ctxRemoveFrameDescriptions = ctx => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frameDescr->mutableMapStrClear
        None
    })->ignore
}

let addAssertion: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    let ctx = ctx.contents
    let (frame, descr) = createFramePriv(ctx, label, exprStr, ())
    ctx.frames->mutableMapStrPut(label, frame)
    ctx.frameDescr->mutableMapStrPut(label, descr)
}

let applySingleStmt = (ctx:mmContext, stmt:stmt):unit => {
    switch stmt {
        | Comment({text}) => addComment(ctx, text)
        | Const({symbols}) => symbols->Js_array2.forEach(addConst(ctx, _))
        | Block(_) => raise(MmException({msg:`Block statements are not accepted by applySingleStmt().`}))
        | Var({symbols}) => symbols->Js_array2.forEach(addVar(ctx, _))
        | Disj({vars}) => addDisj(ctx, vars)
        | Floating({label, expr}) => addFloating(ctx, ~label, ~exprStr=expr)
        | Essential({label, expr}) => addEssential(ctx, ~label, ~exprStr=expr)
        | Axiom({label, expr}) | Provable({label, expr}) => addAssertion(ctx, ~label, ~exprStr=expr)
    }
}

let loadContext = (
    ast, 
    ~initialContext=?,
    ~stopBefore="",
    ~stopAfter="",
    ~expectedNumOfAssertions=-1, 
    ~onProgress= _=>(), 
    ()
) => {
    let expectedNumOfAssertionsF = expectedNumOfAssertions->Belt_Int.toFloat
    let assertionsProcessed = ref(0.)
    let progressTracker = ref(progressTrackerMake(~step=0.01, ~onProgress, ()))

    let onAsrtProcess = () => {
        if (expectedNumOfAssertions > 0) {
            assertionsProcessed.contents = assertionsProcessed.contents +. 1.
            progressTracker.contents = progressTracker.contents->progressTrackerSetCurrPct(assertionsProcessed.contents /. expectedNumOfAssertionsF)
        }
    }

    let (ctx, _) = traverseAst(
        switch initialContext {
            | Some(ctx) => ctx
            | None => createContext(())
        },
        ast,
        ~preProcess = (ctx,node) => {
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        openChildContext(ctx)
                    }
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
                | {stmt} => applySingleStmt(ctx,stmt)
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

let generateNewVarNames = (ctx:mmContext, types:array<int>, typeToPrefix:Belt_MapString.t<string>): array<string> => {
    let prefixToCnt = Belt_MutableMapString.make()

    let getCnt = prefix => prefixToCnt->Belt_MutableMapString.getWithDefault(prefix,0)
    let incCnt = prefix => prefixToCnt->Belt_MutableMapString.set(prefix,getCnt(prefix)+1)

    let maxI = types->Js.Array2.length - 1
    let res = []
    for i in 0 to maxI {
        let typeStr = ctx->ctxIntToSymExn(types[i])
        let prefix = typeToPrefix->Belt_MapString.getWithDefault(typeStr, typeStr)
        incCnt(prefix)
        let newName = ref(prefix ++ getCnt(prefix)->Belt_Int.toString)
        while (ctx->isConst(newName.contents) || ctx->isVar(newName.contents)) {
            incCnt(prefix)
            newName.contents = prefix ++ getCnt(prefix)->Belt_Int.toString
        }
        res->Js.Array2.push(newName.contents)->ignore
    }
    res
}

let generateNewLabels = (ctx:mmContext, ~prefix:string, ~amount:int): array<string> => {
    let maxI = amount - 1
    let cnt = ref(0)
    let res = []
    for _ in 0 to maxI {
        cnt.contents = cnt.contents + 1
        let newName = ref(prefix ++ cnt.contents->Belt_Int.toString)
        while (ctx->isHyp(newName.contents) || ctx->isAsrt(newName.contents)) {
            cnt.contents = cnt.contents + 1
            newName.contents = prefix ++ cnt.contents->Belt_Int.toString
        }
        res->Js.Array2.push(newName.contents)->ignore
    }
    res
}

let renumberConstsInExpr = (expr:expr, constRenum:Belt_HashMapInt.t<int>):unit => {
    let maxI = expr->Js_array2.length-1
    for i in 0 to maxI {
        let sym = expr[i]
        if (sym < 0) {
            expr[i] = constRenum->Belt_HashMapInt.get(sym)->Belt_Option.getWithDefault(sym)
        }
    }
}

let moveConstsToBegin = (ctx:mmContext, constsStr:string):unit => {
    let rootCtx = ctx.contents.root->Belt_Option.getExn
    let constsToMove = constsStr->getSpaceSeparatedValuesAsArray
        ->Js_array2.map(ctxSymToInt(ctx,_))
        ->Js.Array2.filter(intOpt => intOpt->Belt_Option.mapWithDefault(false, i => i < 0))
        ->Js.Array2.map(Belt_Option.getExn)
        ->Belt_HashSetInt.fromArray
    let constsLen = constsToMove->Belt_HashSetInt.size
    let reservedIdxs = Belt_HashSetInt.make(~hintSize=constsLen)

    let getConstToMoveFar = () => {
        let res= ref(1)
        while (reservedIdxs->Belt_HashSetInt.has(res.contents) || constsToMove->Belt_HashSetInt.has(-res.contents)) {
            res.contents = res.contents + 1
        }
        reservedIdxs->Belt_HashSetInt.add(res.contents)
        -res.contents
    }
    
    let constRenum = Belt_HashMapInt.make(~hintSize=constsLen)
    constsToMove->Belt_HashSetInt.forEach(constToMoveClose => {
        if (constToMoveClose < -constsLen) {
            let constToMoveFar = getConstToMoveFar()
            constRenum->Belt_HashMapInt.set(constToMoveClose, constToMoveFar)
            constRenum->Belt_HashMapInt.set(constToMoveFar, constToMoveClose)
            let symTmp = rootCtx.consts[-constToMoveClose]
            rootCtx.consts[-constToMoveClose] = rootCtx.consts[-constToMoveFar]
            rootCtx.consts[-constToMoveFar] = symTmp
            rootCtx.symToInt->mutableMapStrPut(rootCtx.consts[-constToMoveClose], constToMoveClose)
            rootCtx.symToInt->mutableMapStrPut(rootCtx.consts[-constToMoveFar], constToMoveFar)
        }
    })

    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.hyps->Js_array2.forEach(hyp => renumberConstsInExpr(hyp.expr, constRenum))
        ctx.frames->mutableMapStrForEach((_,frame) => {
            frame.hyps->Js_array2.forEach(hyp => renumberConstsInExpr(hyp.expr, constRenum))
            renumberConstsInExpr(frame.asrt, constRenum)
            renumberConstsInExpr(frame.varTypes, constRenum)
        })
        None
    })->ignore

}