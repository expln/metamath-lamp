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

type frameDbg = {
    disj: array<string>,
    hyps: array<string>,
    asrt: string,
}

type frame = {
    disj: Belt_MapInt.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    frameVarToSymb: array<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
    descr:option<string>,
    proof:option<proof>,
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
    symToHyp: Belt_HashMapString.t<hypothesis>,
    exprToHyp: Belt_HashMap.t<expr,hypothesis,ExprHash.identity>,
    mutable lastComment: option<string>,
    frames: Belt_HashMapString.t<frame>,
    debug:bool,
}

type mmContext = ref<mmContextContents>

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
    let hash = Expln_utils_common.hashArrInt
    let eq = exprEq
})

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
                switch ctx.symToHyp->Belt_HashMapString.get(token) {
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
    })->Belt_Option.isSome
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
        ctx.symToHyp->Belt_HashMapString.get(label)
    })->Belt_Option.isSome
}

let isAsrt = (ctx:mmContext, label:string) => {
    ctx->forEachCtxInReverseOrder(ctx => {
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

let disjNumOfGroups = disjMutable => disjMutable->Belt_HashMapInt.size

let disjForEach = (disjMutable, consumer) => {
    disjMutable->Belt_HashMapInt.forEach((n,ms) => {
        ms->Belt_HashSetInt.forEach(m => {
            consumer(n,m)
        })
    })
}

let disjToArr = (disj:Belt_MapInt.t<Belt_SetInt.t>):array<array<int>> => {
    let res = []
    disj->Belt_MapInt.forEach((n,ms) => {
        ms->Belt_SetInt.forEach(m => {
            res->Js_array2.push([n,m])->ignore
        })
    })
    res
}

let disjForEachArr = (disjMutable, consumer) => {
    disjMutable->disjForEach((n,m) => consumer([n,m]))
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
        ctx.symToHyp->Belt_HashMapString.get(label)
    })
}

let getHypByExpr = (ctx:mmContext, expr:expr) => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.exprToHyp->Belt_HashMap.get(expr)
    })
}

let getFrame = (ctx:mmContext,label):option<frame> => {
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->Belt_HashMapString.get(label)
    })
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

let frmIntToSymExn = (ctx:mmContext, frameVarToSymb:array<string>, i:int) => {
    if (i < 0) {ctx->ctxIntToSymExn(i)} else {frameVarToSymb[i]}
}

let frmIntsToStrExn = (ctx:mmContext, frameVarToSymb:array<string>, expr:expr):string => {
    expr->Js_array2.map(frmIntToSymExn(ctx, frameVarToSymb, _))->Js_array2.joinWith(" ")
}

let getTypeOfVar = (ctx:mmContext, varInt:int):option<int> => {
    ctx.contents->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F && hyp.expr[1] == varInt) {
            Some(hyp.expr[0])
        } else {
            None
        }
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

let extractMandatoryVariables = (ctx:mmContext, asrt:expr, ~skipHyps:bool=false, ()):Belt_HashSetInt.t => {
    let res = Belt_HashSetInt.make(~hintSize=16)
    if (!skipHyps) {
        ctx->forEachHypothesisInDeclarationOrder(hyp => {
            if (hyp.typ == E) {
                hyp.expr->Js_array2.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
            }
            None
        })->ignore
    }
    asrt->Js_array2.forEach(i => if i >= 0 {res->Belt_HashSetInt.add(i)})
    res
}

let extractMandatoryDisj = (ctx:mmContext, mandatoryVars:Belt_HashSetInt.t): disjMutable => {
    let mandatoryDisj = Belt_HashMapInt.make(~hintSize=16)
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

let getAllFrames = (ctx):Belt_MapString.t<frame> => {
    let frames = []
    ctx.contents->forEachCtxInReverseOrder(ctx => {
        ctx.frames->mutableMapStrForEach((k,v) => frames->Js.Array2.push((k,v))->ignore)
        None
    })->ignore
    Belt_MapString.fromArray(frames)
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
    let progressState = progressTrackerMutableMake(~step=0.01, ~onProgress?, ())
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
        progressState->progressTrackerMutableSetCurrPct(
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
            symToInt: mutableMapStrMake(),
            disj: disjMutableMake(),
            hyps: [],
            symToHyp: mutableMapStrMake(),
            exprToHyp: Belt_HashMap.make(~hintSize=5, ~id=module(ExprHash)),
            lastComment: None,
            frames: mutableMapStrMake(),
            debug: pCtxContentsOpt->Belt_Option.map(pCtx => pCtx.debug)->Belt.Option.getWithDefault(debug),
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
    ctx.contents.lastComment = Some(str)
}

let addConstPriv: (mmContextContents,string) => unit = (ctx,cName) => {
    if (ctx.parent->Belt_Option.isSome) {
        raise(MmException({msg:`An attempt to declare a constant '${cName}' in an inner block.`}))
    } else if (isConstPriv(ctx, cName) || isVarPriv(ctx, cName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${cName}' as a constant.`}))
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
            ctx.exprToHyp->Belt_HashMap.set(expr, hyp)
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
        ctx.exprToHyp->Belt_HashMap.set(expr, hyp)
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

let createFrameVarToSymbMap = (ctx:mmContextContents, mandatoryHypotheses:array<hypothesis>, asrt, renumbering: Belt_MapInt.t<int>): array<string> => {
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
    let frameVarToSymb = Expln_utils_common.createArray(allVars->mutableSetIntSize)
    allVars->mutableSetIntForEach(ctxVar => {
        frameVarToSymb[renumbering->Belt_MapInt.getExn(ctxVar)] = ctxIntToSymExnPriv(ctx,ctxVar)
    })
    frameVarToSymb
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

let createFramePriv = (
    ~ctx:mmContextContents, 
    ~label:string, 
    ~exprStr:array<string>,
    ~proof:option<proof>,
    ~skipHyps:bool=false, 
    ~skipFirstSymCheck:bool=false, 
    ()
):frame => {
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
                let disj = mandatoryDisj->renumberVarsInDisj(varRenumbering)
                let frameVarToSymb = createFrameVarToSymbMap(ctx, mandatoryHypotheses, asrt, varRenumbering)
                let asrt = asrt->renumberVarsInExpr(varRenumbering)
                let dbg = if (ctx.debug) {
                    Some({
                        disj: disj->disjToArr->Js_array2.map(frmIntsToStrExnPriv(ctx, frameVarToSymb, _)),
                        hyps: hyps->Js_array2.map(hyp => frmIntsToStrExnPriv(ctx, frameVarToSymb, hyp.expr)),
                        asrt: frmIntsToStrExnPriv(ctx, frameVarToSymb, asrt),
                    })
                } else {
                    None
                }
                let frame = {
                    disj,
                    hyps,
                    asrt,
                    label,
                    frameVarToSymb,
                    varTypes,
                    numOfVars: varTypes->Js_array2.length,
                    numOfArgs: hyps->Js_array2.length,
                    descr: ctx.lastComment,
                    proof,
                    dbg
                }
                frame
            }
        }
    }
}

let createFrame = ( ~ctx:mmContext, ~label, ~exprStr, ~proof, ~skipHyps:bool=false, ~skipFirstSymCheck:bool=false, () ) => 
    createFramePriv(~ctx=ctx.contents, ~label, ~exprStr, ~proof, ~skipHyps, ~skipFirstSymCheck, ())

let addAssertion: (mmContext, ~label:string, ~exprStr:array<string>, ~proof:option<proof>) => unit = (
    ctx, ~label, ~exprStr, ~proof
) => {
    let ctx = ctx.contents
    ctx.frames->mutableMapStrPut(label, createFramePriv(~ctx, ~label, ~exprStr, ~proof, ()))
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
        | Axiom({label, expr}) => addAssertion(ctx, ~label, ~exprStr=expr, ~proof=None)
        | Provable({label, expr, proof}) => addAssertion(ctx, ~label, ~exprStr=expr, ~proof)
    }
}

let loadContext = (
    ast, 
    ~initialContext=?,
    ~stopBefore="",
    ~stopAfter="",
    ~expectedNumOfAssertions=-1, 
    ~onProgress= _=>(), 
    ~debug:bool=false, 
    ()
) => {
    let expectedNumOfAssertionsF = expectedNumOfAssertions->Belt_Int.toFloat
    let assertionsProcessed = ref(0.)
    let progressTracker = progressTrackerMutableMake(~step=0.1, ~onProgress, ())

    let onAsrtProcess = () => {
        if (expectedNumOfAssertions > 0) {
            assertionsProcessed.contents = assertionsProcessed.contents +. 1.
            progressTracker->progressTrackerMutableSetCurrPct(assertionsProcessed.contents /. expectedNumOfAssertionsF)
        }
    }

    let (ctx, _) = traverseAst(
        switch initialContext {
            | Some(ctx) => ctx
            | None => createContext(~debug, ())
        },
        ast,
        ~preProcess = (ctx,node) => {
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

let generateNewVarNames = (
    ~ctx:mmContext, 
    ~types:array<int>, 
    ~typeToPrefix:Belt_MapString.t<string>,
    ~usedNames:option<Belt_HashSetString.t>=?,
    ()
): array<string> => {
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
        while (ctx->isConst(newName.contents) || ctx->isVar(newName.contents)
                || usedNames
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
    ~usedLabels:option<Belt_HashSetString.t>=?,
    ()
): array<string> => {
    let maxI = amount - 1
    let cnt = ref(0)
    let res = []
    for _ in 0 to maxI {
        cnt.contents = cnt.contents + 1
        let newName = ref(prefix ++ cnt.contents->Belt_Int.toString)
        while (ctx->isHyp(newName.contents) || ctx->isAsrt(newName.contents)
                    || usedLabels
                        ->Belt.Option.map(Belt_HashSetString.has(_,newName.contents))
                        ->Belt_Option.getWithDefault(false)
        ) {
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

let ctxMakeExprToHyp = (ctx:mmContext):Belt_HashMap.t<expr,hypothesis,ExprHash.identity> => {
    let res = Belt_HashMap.make(
        ~id=module(ExprHash), 
        ~hintSize=ctx.contents.root->Belt_Option.map(root => root.hyps->Js_array.length)->Belt_Option.getWithDefault(0)
    )
    ctx.contents->forEachCtxInDeclarationOrder(ctx => {
        ctx.hyps->Js_array2.forEach(hyp => res->Belt_HashMap.set(hyp.expr, hyp))
    })
    res
}
