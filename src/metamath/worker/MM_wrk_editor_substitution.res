open MM_wrk_editor
open MM_context
open MM_parser
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings
open MM_parenCounter
open MM_substitution
open MM_provers
open Common

let verifyTypesForSubstitution = (~parenCnt, ~ctx, ~frms, ~frameRestrict, ~wrkSubs:wrkSubs):unit => {
    let varToExprArr = wrkSubs.subs->Belt_MapInt.toArray
    let typesToProve = varToExprArr->Array.map(((var,expr)) => 
        [ctx->getTypeOfVarExn(var)]->Array.concat(expr)
    )
    let proofTree = proveFloatings(
        ~wrkCtx=ctx,
        ~frms,
        ~frameRestrict,
        ~floatingsToProve=typesToProve,
        ~parenCnt,
    )
    varToExprArr->Array.forEachWithIndex(((var,expr), i) =>
        if (wrkSubs.err->Belt_Option.isNone) {
            let typeExpr = typesToProve->Array.getUnsafe(i)
            if (proofTree->ptGetNode(typeExpr)->pnGetProof->Belt_Option.isNone) {
                wrkSubs.err = Some(TypeMismatch({ var, subsExpr:expr, typeExpr, }))
            }
        }
    )
}

let verifyDisjoints = (~wrkSubs:wrkSubs, ~disj:disjMutable):unit => {
    let varToSubVars = Belt_HashMapInt.make(~hintSize=wrkSubs.subs->Belt_MapInt.size)

    let getSubVars = var => {
        switch varToSubVars->Belt_HashMapInt.get(var) {
            | None => {
                varToSubVars->Belt_HashMapInt.set(
                    var, 
                    switch wrkSubs.subs->Belt_MapInt.get(var) {
                        | None => []
                        | Some(expr) => expr->Array.filter(s => s >= 0)
                    }
                )
                varToSubVars->Belt_HashMapInt.get(var)->Belt.Option.getExn
            }
            | Some(arr) => arr
        }
    }

    disj->disjForEach((n,m) => {
        if (wrkSubs.err->Belt_Option.isNone) {
            getSubVars(n)->Array.forEach(nv => {
                if (wrkSubs.err->Belt_Option.isNone) {
                    getSubVars(m)->Array.forEach(mv => {
                        if (wrkSubs.err->Belt_Option.isNone) {
                            if (nv == mv) {
                                wrkSubs.err = Some(CommonVar({
                                    var1:n,
                                    var2:m,
                                    commonVar:nv
                                }))
                            }
                            if (wrkSubs.err->Belt_Option.isNone && !(disj->disjContains(nv,mv))) {
                                wrkSubs.newDisj->disjAddPair(nv,mv)
                            }
                        }
                    })
                }
            })
        }
    })
}

let applyWrkSubs = (expr, wrkSubs:wrkSubs): expr => {
    let resultSize = ref(0)
    expr->Array.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            switch wrkSubs.subs->Belt_MapInt.get(s) {
                | None => raise(MmException({msg:`Cannot find a substitution for ${s->Belt_Int.toString} in applyWrkSubs.`}))
                | Some(expr) => resultSize.contents = resultSize.contents + expr->Array.length
            }
        }
    })
    let res = Expln_utils_common.createArray(resultSize.contents)
    let e = ref(0)
    let r = ref(0)
    while (r.contents < resultSize.contents) {
        let s = expr->Array.getUnsafe(e.contents)
        if (s < 0) {
            res[r.contents] = s
            r.contents = r.contents + 1
        } else {
            let subExpr = wrkSubs.subs->Belt_MapInt.getExn(s)
            let len = subExpr->Array.length
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=0, ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let applySubstitutionForStmt = (st:editorState, ctx:mmContext, stmt:userStmt, wrkSubs:wrkSubs):userStmt => {
    let expr = ctx->ctxSymsToIntsExn(stmt.cont->contToArrStr)
    let newExpr = applyWrkSubs(expr, wrkSubs)
    {
        ...stmt,
        cont: ctx->ctxIntsToStrExn(newExpr)->strToCont(~preCtxColors=st.preCtxColors, ~wrkCtxColors=st.wrkCtxColors)
    }
}

let applySubstitutionForEditor = (st, wrkSubs:wrkSubs):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot apply substitution without wrkCtx.`}))
        | Some(wrkCtx) => {
            let st = createNewDisj(st, wrkSubs.newDisj)
            {
                ...st,
                stmts: st.stmts->Array.map(stmt => applySubstitutionForStmt(st, wrkCtx,stmt,wrkSubs))
            }
        }
    }
}

let convertSubsToWrkSubs = (~subs, ~tmpFrame, ~ctx):wrkSubs => {
    let frameVarToCtxVar = frameVar => {
        switch tmpFrame.frameVarToSymb->Belt_Array.get(frameVar) {
            | None => raise(MmException({msg:`Cannot convert frameVar to ctxVar.`}))
            | Some(ctxSym) => ctx->ctxSymToIntExn(ctxSym)
        }
    }
    let res = Belt_Array.range(0,tmpFrame.numOfVars-1)
        ->Array.map(v => {
            (
                frameVarToCtxVar(v),
                applySubs(
                    ~frmExpr=[v],
                    ~subs,
                    ~createWorkVar = 
                        _ => raise(MmException({msg:`Work variables are not supported in convertSubsToWrkSubs().`}))
                )
            )
        })
        ->Belt_HashMapInt.fromArray
    let maxVar = ctx->getNumOfVars-1
    for v in 0 to maxVar {
        if (!(res->Belt_HashMapInt.has(v))) {
            res->Belt_HashMapInt.set(v, [v])
        }
    }
    {
        subs: res->Belt_HashMapInt.toArray->Belt_MapInt.fromArray,
        newDisj: disjMake(),
        err: None,
    }
}

let findPossibleSubsByMatch = (
    ~wrkCtx:mmContext, 
    ~parenCnt: parenCnt,
    ~frmExpr:expr, 
    ~expr:expr
):result<array<wrkSubs>,string> => {
    try {
        let axLabel = generateNewLabels(~ctx=wrkCtx, ~prefix="temp-ax-", ~amount=1)->Array.getUnsafe(0)
        let tmpFrame = createFrame(
            ~ctx=wrkCtx, ~ord=0, ~isAxiom=false, ~label=axLabel, ~exprStr=wrkCtx->ctxIntsToSymsExn(frmExpr), ~proof=None,
            ~skipEssentials=true, ~skipFirstSymCheck=true
        )
        let frm = prepareFrmSubsDataForFrame(tmpFrame)
        let foundSubs = []
        iterateSubstitutions(
            ~frmExpr=tmpFrame.asrt,
            ~expr,
            ~frmConstParts = frm.frmConstParts->Array.getUnsafe(frm.numOfHypsE), 
            ~constParts = frm.constParts->Array.getUnsafe(frm.numOfHypsE), 
            ~varGroups = frm.varGroups->Array.getUnsafe(frm.numOfHypsE),
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = subs => {
                let wrkSubs = convertSubsToWrkSubs(~subs, ~tmpFrame, ~ctx=wrkCtx)
                foundSubs->Array.push(wrkSubs)
                Continue
            }
        )->ignore
        Ok(foundSubs)
    } catch {
        | MmException({msg}) => Error(msg)
        | Exn.Error(exn) => Error(exn->Exn.message->Belt_Option.getWithDefault("Unknown error."))
    }
}

let makeCannotBuildSyntaxTreeError = (expr:expr, msg:string, ctx:mmContext):string => {
    `Cannot build a syntax tree for the expression '${ctx->ctxIntsToStrExn(expr)}': ${msg}`
}

let buildSyntaxTreesOfSameType = (
    ~wrkCtx:mmContext, 
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~expr1:expr, 
    ~expr2:expr,
):result<(syntaxTreeNode,syntaxTreeNode),string> => {
    let syntaxTrees = textToSyntaxTree(
        ~wrkCtx,
        ~syms = [ wrkCtx->ctxIntsToSymsExn(expr1), wrkCtx->ctxIntsToSymsExn(expr2) ],
        ~syntaxTypes,
        ~frms,
        ~frameRestrict,
        ~parenCnt,
        ~lastSyntaxType=None,
        ~onLastSyntaxTypeChange = _ => (),
    )
    @warning("-8")
    switch syntaxTrees {
        | Error(msg) => Error(msg)
        | Ok([tree1,tree2]) => {
            switch tree1 {
                | Error(msg) => Error(makeCannotBuildSyntaxTreeError(expr1, msg, wrkCtx))
                | Ok(tree1) => {
                    switch tree2 {
                        | Error(msg) => Error(makeCannotBuildSyntaxTreeError(expr2, msg, wrkCtx))
                        | Ok(tree2) => {
                            if (tree1.typ == tree2.typ) {
                                Ok((tree1,tree2))
                            } else {
                                let syntaxTreeWithMatchingType = textToSyntaxTree(
                                    ~wrkCtx,
                                    ~syms = [ wrkCtx->ctxIntsToSymsExn(expr1) ],
                                    ~syntaxTypes=[tree2.typ],
                                    ~frms,
                                    ~frameRestrict,
                                    ~parenCnt,
                                    ~lastSyntaxType=None,
                                    ~onLastSyntaxTypeChange = _ => (),
                                )
                                @warning("-8")
                                switch syntaxTreeWithMatchingType {
                                    | Ok([Ok(tree1)]) => Ok((tree1,tree2))
                                    | _ => {
                                        let syntaxTreeWithMatchingType = textToSyntaxTree(
                                            ~wrkCtx,
                                            ~syms = [ wrkCtx->ctxIntsToSymsExn(expr2) ],
                                            ~syntaxTypes=[tree1.typ],
                                            ~frms,
                                            ~frameRestrict,
                                            ~parenCnt,
                                            ~lastSyntaxType=None,
                                            ~onLastSyntaxTypeChange = _ => (),
                                        )
                                        @warning("-8")
                                        switch syntaxTreeWithMatchingType {
                                            | Ok([Ok(tree2)]) => Ok((tree1,tree2))
                                            | _ => {
                                                Error(`Cannot prove the two expressions are of the same type.`)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let removeTypePrefix = (expr:expr, allTypes:array<int>):expr => {
    if (expr->Array.length > 0 && allTypes->Array.includes(expr->Array.getUnsafe(0))) {
        expr->Array.sliceToEnd(~start=1)
    } else {
        expr
    }
}

let findPossibleSubsByUnif = (
    ~wrkCtx:mmContext, 
    ~allTypes:array<int>,
    ~syntaxTypes:array<int>,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~parenCnt: parenCnt,
    ~expr1:expr, 
    ~expr2:expr,
    ~metavarPrefix:string,
):result<array<wrkSubs>,string> => {
    let syntaxTrees = buildSyntaxTreesOfSameType( 
        ~wrkCtx, ~syntaxTypes, ~frms, ~frameRestrict, ~parenCnt, 
        ~expr1=removeTypePrefix(expr1, allTypes), 
        ~expr2=removeTypePrefix(expr2, allTypes), 
    )
    switch syntaxTrees {
        | Error(msg) => Error(msg)
        | Ok((tree1,tree2)) => {
            let continue = ref(true)
            let foundSubs = Belt_HashMapString.make(~hintSize = expr1->Array.length + expr2->Array.length)
            unify(tree1, tree2, ~foundSubs, ~continue, ~isMetavar=String.startsWith(_,metavarPrefix))
            if (!continue.contents) {
                Error(`Cannot unify these expressions.`)
            } else {
                let res = foundSubs->Belt_HashMapString.toArray
                    ->Array.map(((var,expr)) => (wrkCtx->ctxSymToIntExn(var), wrkCtx->ctxSymsToIntsExn(expr)))
                    ->Belt_HashMapInt.fromArray
                let maxVar = wrkCtx->getNumOfVars-1
                for v in 0 to maxVar {
                    if (!(res->Belt_HashMapInt.has(v))) {
                        res->Belt_HashMapInt.set(v, [v])
                    }
                }
                Ok([{
                    subs: res->Belt_HashMapInt.toArray->Belt_MapInt.fromArray,
                    newDisj: disjMake(),
                    err: None,
                }])
            }
        }
    }
}

let findPossibleSubs = (st:editorState, frmExpr:expr, expr:expr, useMatching:bool):result<array<wrkSubs>,string> => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot search for substitutions without wrkCtx.`}))
        | Some(wrkCtx) => {
            let foundSubs = if (useMatching) {
                findPossibleSubsByMatch(~wrkCtx, ~parenCnt=st.parenCnt, ~frmExpr, ~expr)
            } else {
                findPossibleSubsByUnif(
                    ~wrkCtx, 
                    ~allTypes=st.allTypes,
                    ~syntaxTypes=st.syntaxTypes,
                    ~frms=st.frms,
                    ~frameRestrict=st.settings.allowedFrms.inSyntax,
                    ~parenCnt=st.parenCnt,
                    ~expr1=frmExpr, 
                    ~expr2=expr,
                    ~metavarPrefix=st.settings.unifMetavarPrefix,
                )
            }
            switch foundSubs {
                | Error(msg) => Error(msg)
                | Ok(foundSubs) => {
                    let disj = wrkCtx->getAllDisj
                    foundSubs->Array.forEach(wrkSubs => {
                        verifyDisjoints(~wrkSubs, ~disj)
                        if (wrkSubs.err->Belt_Option.isNone) {
                            verifyTypesForSubstitution(
                                ~parenCnt=st.parenCnt, 
                                ~ctx=wrkCtx, 
                                ~frms=st.frms, 
                                ~frameRestrict=st.settings.allowedFrms.inSyntax,
                                ~wrkSubs
                            )
                        }
                    })
                    Ok(foundSubs)
                }
            }
        }
    }
}

let substitute = (st:editorState, ~what:string, ~with_:string):result<editorState,string> => {
    switch st.wrkCtx {
        | None => Error("Cannot apply a substitution because of errors in the editor.")
        | Some(wrkCtx) => {
            let findIncorrectSymbol = syms => syms->Array.find(sym => {
                !(wrkCtx->isConst(sym) || wrkCtx->MM_context.isVar(sym))
            })
            let syms1 = what->getSpaceSeparatedValuesAsArray
            switch findIncorrectSymbol(syms1) {
                | Some(sym) => Error(`Unknown symbol - '${sym}'`)
                | None => {
                    let syms2 = with_->getSpaceSeparatedValuesAsArray
                    switch findIncorrectSymbol(syms2) {
                        | Some(sym) => Error(`Unknown symbol - '${sym}'`)
                        | None => {
                            let foundSubs = findPossibleSubs(
                                st, 
                                wrkCtx->ctxSymsToIntsExn(syms1),
                                wrkCtx->ctxSymsToIntsExn(syms2),
                                true,
                            )
                            switch foundSubs {
                                | Error(msg) => Error(msg)
                                | Ok(foundSubs) => {
                                    let validSubs = foundSubs->Array.filter(subs => subs.err->Belt_Option.isNone)
                                    if (validSubs->Array.length == 0) {
                                        Error(`No substitutions found.`)
                                    } else if (validSubs->Array.length > 1) {
                                        Error(`More than 1 substitution found.`)
                                    } else {
                                        Ok(st->applySubstitutionForEditor(validSubs->Array.getUnsafe(0)))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}