open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree
open MM_unification_debug

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    maxSearchDepth: int,
    lengthRestriction: lengthRestrict,
    allowNewVars: bool,
    useRootStmtsAsArgs: bool,
}

let findAsrtParentsWithoutNewVars = ( 
    ~tree, 
    ~expr, 
    ~restrictExprLen:lengthRestrict, 
):array<exprSource> => {
    let exprLen = expr->Js_array2.length
    let foundParents = []
    tree->ptGetFrms->Belt_MapString.forEach((_,frm) => {
        let frmExpr = frm.frame.asrt
        if (frmExpr[0] == expr[0]) {
            iterateSubstitutions(
                ~frmExpr,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE],
                ~constParts = frm.constParts[frm.numOfHypsE],
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt=tree->ptGetParenCnt,
                ~consumer = subs => {
                    if (subs.isDefined->Js_array2.every(b=>b)
                        && verifyDisjoints(
                                ~frmDisj=frm.frame.disj, 
                                ~subs, 
                                ~isDisjInCtx=tree->ptIsDisj, 
                                ~debugLevel=0
                            )->Belt.Option.isNone
                    ) {
                        let hyps = frm.frame.hyps
                        let numOfArgs = hyps->Js_array2.length
                        let args = Expln_utils_common.createArray(numOfArgs)
                        let argsAreCorrect = ref(true)
                        let argIdx = ref(0)
                        let maxArgIdx = numOfArgs - 1
                        while (argIdx.contents <= maxArgIdx && argsAreCorrect.contents) {
                            let newExpr = applySubs(
                                ~frmExpr = hyps[argIdx.contents].expr, 
                                ~subs,
                                ~createWorkVar = _ => raise(MmException({
                                    msg:`Work variables are not supported in addAsrtParentsWithoutNewVars().`
                                }))
                            )
                            argsAreCorrect.contents = switch restrictExprLen {
                                | No => true
                                | LessEq => newExpr->Js_array2.length <= exprLen
                                | Less => newExpr->Js_array2.length < exprLen
                            }
                            if (argsAreCorrect.contents) {
                                args[argIdx.contents] = tree->ptGetOrCreateNode(newExpr)
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        if (argsAreCorrect.contents) {
                            foundParents->Js_array2.push( Assertion({ args, frame:frm.frame, err:None }) )->ignore
                        }
                    }
                    Continue
                }
            )->ignore
        }
    })
    foundParents
}

let proveFloating = (
    ~tree:proofTree, 
    ~node:proofNode, 
) => {
    /*
    If a node has a proof, no need to prove it again.
    If a node has Some(parents), it could appear as a side effect of proving other nodes and it was left unproved 
        because the root node became proved first.
    It follows from the statement above, that all unproved nodes have to have most complete collection of parents, 
        otherwise there is a risk to miss existing proof for this node. So we must not interrupt the process of adding 
        parents even if the root node becomes proved.
    */
    if (node->pnGetProof->Belt.Option.isNone && !(node->pnIsInvalidFloating)) {
        let nodesToCreateParentsFor = Belt_MutableQueue.make()
        let savedNodes = Belt_HashSet.make( ~id=module(ExprHash), ~hintSize = 16 )

        let saveNodeToCreateParentsFor = node => {
            switch node->pnGetProof {
                | Some(_) => ()
                | None => {
                    if (!(savedNodes->Belt_HashSet.has(node->pnGetExpr))) {
                        savedNodes->Belt_HashSet.add(node->pnGetExpr)
                        nodesToCreateParentsFor->Belt_MutableQueue.add(node)
                    }
                }
            }
        }

        let rootNode = node
        saveNodeToCreateParentsFor(rootNode)
        while (rootNode->pnGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableQueue.isEmpty)) {
            let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
            if (curNode->pnGetProof->Belt.Option.isNone) {
                switch curNode->pnGetParents {
                    | Some(parents) => {
                        parents->Js.Array2.forEach(parent => {
                            switch parent {
                                | Assertion({args}) => args->Js_array2.forEach(saveNodeToCreateParentsFor)
                                | _ => ()
                            }
                        })
                    }
                    | None => {
                        let curExpr = curNode->pnGetExpr
                        if (tree->ptIsNewVarDef(curExpr)) {
                            curNode->pnAddParent(VarType)
                        } else {
                            switch tree->ptGetHypByExpr(curExpr) {
                                | Some(hyp) => curNode->pnAddParent(Hypothesis({label:hyp.label}))
                                | None => {
                                    findAsrtParentsWithoutNewVars(
                                        ~tree, ~expr=curExpr, ~restrictExprLen=LessEq
                                    )->Js.Array2.forEach(parent => {
                                        curNode->pnAddParent(parent)
                                        switch parent {
                                            | Assertion({args}) => args->Js_array2.forEach(saveNodeToCreateParentsFor)
                                            | _ => ()
                                        }
                                    })
                                }
                            }
                        }
                    }
                }
            }
        }
        if (rootNode->pnGetProof->Belt.Option.isNone) {
            rootNode->pnSetInvalidFloating(true)
        }
    }
}

let findAsrtParentsWithNewVars = (
    ~tree,
    ~expr:expr,
    ~stmts:array<expr>,
    ~exactOrderOfStmts:bool,
    ~asrtLabel:option<string>=?,
    ~allowEmptyArgs:bool,
    ~allowNewVars:bool,
    ~debugLevel:int=0,
    ()
):array<exprSource> => {
    let applResults = []
    applyAssertions(
        ~maxVar = tree->ptGetMaxVar,
        ~frms = tree->ptGetFrms,
        ~isDisjInCtx = tree->ptIsDisj,
        ~parenCnt=tree->ptGetParenCnt,
        ~statements = stmts,
        ~exactOrderOfStmts,
        ~allowEmptyArgs,
        ~result = expr,
        ~frameFilter = frame => asrtLabel
                                    ->Belt_Option.map(asrtLabel => frame.label == asrtLabel)
                                    ->Belt_Option.getWithDefault(true),
        ~debugLevel,
        ~onMatchFound = res => {
            if (allowNewVars || res.newVars->Js.Array2.length == 0) {
                applResults->Js_array2.push(res)->ignore
            }
            if (applResults->Js.Array2.length > 1000) {
                Stop
            } else {
                Continue
            }
        },
        ()
    )
    let foundParents = []
    applResults->Js_array2.forEach(applResult => {
        let applNewVarToTreeNewVar = Belt_MutableMapInt.make()
        applResult.newVars->Js.Array2.forEachi((applResNewVar,i) => {
            let newVarType = applResult.newVarTypes[i]
            let treeNewVar = tree->ptAddNewVar(newVarType)
            applNewVarToTreeNewVar->Belt_MutableMapInt.set(applResNewVar,treeNewVar)
        })
        applResult.newDisj->disjForEach((n,m) => {
            tree->ptAddDisjPair(
                applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(n, n),
                applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(m, m),
            )
        })
        let frame = switch tree->ptGetFrms->Belt_MapString.get(applResult.asrtLabel) {
            | None => 
                raise(MmException({msg:`Cannot find an assertion with label ${applResult.asrtLabel} in findParentsWithNewVars.`}))
            | Some(frm) => frm.frame
        }
        let numOfArgs = frame.hyps->Js_array2.length
        let args = Expln_utils_common.createArray(numOfArgs)
        let unprovedFloating = ref(None)
        let argIdx = ref(0)
        let maxArgIdx = numOfArgs - 1

        while (argIdx.contents <= maxArgIdx && (unprovedFloating.contents->Belt_Option.isNone || debugLevel != 0)) {
            let argExpr = applySubs(
                ~frmExpr = frame.hyps[argIdx.contents].expr, 
                ~subs=applResult.subs,
                ~createWorkVar = _ => raise(MmException({msg:`New work variables are not expected here [findParentsWithNewVars].`}))
            )
            let maxI = argExpr->Js_array2.length-1
            for i in 0 to maxI {
                argExpr[i] = applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(argExpr[i], argExpr[i])
            }
            let argNode = tree->ptGetOrCreateNode(argExpr)
            args[argIdx.contents] = argNode
            if (frame.hyps[argIdx.contents].typ == F
                    && (
                            debugLevel == 0 
                            || applResult.err->Belt_Option.isNone && unprovedFloating.contents->Belt_Option.isNone
                       )
            ) {
                proveFloating(~tree, ~node=argNode)
                if (argNode->pnGetProof->Belt.Option.isNone) {
                    unprovedFloating.contents = Some(argNode->pnGetExpr)
                }
            }
            argIdx.contents = argIdx.contents + 1
        }
        if (debugLevel == 0) {
            if (unprovedFloating.contents->Belt_Option.isNone) {
                foundParents->Js.Array2.push( Assertion({ 
                    args, 
                    frame,
                    err: None
                }) )->ignore
            }
        } else {
            foundParents->Js.Array2.push( Assertion({ 
                args, 
                frame,
                err: switch applResult.err {
                    | Some(_) => applResult.err
                    | None => unprovedFloating.contents->Belt_Option.map(expr => UnprovedFloating({expr:expr}))
                }
            }) )->ignore
        }
    })
    foundParents
}

let proveWithoutJustification = (~tree:proofTree, ~expr:expr):proofNode => {
    let node = tree->ptGetOrCreateNode(expr)
    if (node->pnGetProof->Belt.Option.isNone) {
        let parents = findAsrtParentsWithNewVars( 
            ~tree, 
            ~expr, 
            ~stmts=tree->ptGetRootStmts->Js.Array2.map(stmt => stmt.expr),
            ~exactOrderOfStmts=false,
            ~allowEmptyArgs=false,
            ~allowNewVars=false,
            () 
        )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent)
            node->pnGetProof
        })->ignore
    }
    node
}

let getStatementsFromJustification = (
    ~tree:proofTree,
    ~jstf: justification,
):option<array<expr>> => {
    let getStmtByLabel = label => {
        switch tree->ptGetRootStmts->Js.Array2.find(stmt => stmt.label == label) {
            | Some(stmt) => Some(stmt.expr)
            | None => {
                switch tree->ptGetHypByLabel(label) {
                    | Some(hyp) => Some(hyp.expr)
                    | None => None
                }
            }
        }
    }
    let foundStmts = jstf.args
        ->Js_array2.map(getStmtByLabel)
        ->Js_array2.filter(Belt_Option.isSome)
        ->Js_array2.map(Belt_Option.getExn)
    if (foundStmts->Js_array2.length == jstf.args->Js.Array2.length) {
        Some(foundStmts)
    } else {
        None
    }
}

let proveWithJustification = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~jstf: justification, 
):proofNode => {
    let node = tree->ptGetOrCreateNode(expr)
    if (node->pnGetProof->Belt.Option.isNone) {
        switch getStatementsFromJustification( ~tree, ~jstf, ) {
            | None => ()
            | Some(args) => {
                let parents = findAsrtParentsWithNewVars( 
                    ~tree,
                    ~expr,
                    ~stmts=args,
                    ~exactOrderOfStmts=true,
                    ~allowEmptyArgs=false,
                    ~allowNewVars=false,
                    ~asrtLabel=jstf.asrt,
                    ()
                )
                parents->Expln_utils_common.arrForEach(parent => {
                    node->pnAddParent(parent)
                    node->pnGetProof
                })->ignore
            }
        }
    }
    node
}

let srcIsBackRef = (expr:expr, src:exprSource):bool => {
    switch src {
        | Assertion({args}) => args->Js_array2.some(arg => expr->exprEq(arg->pnGetExpr))
        | _ => false
    }
}

let proveBottomUp = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~getParents:(expr,int)=>array<exprSource>,
    ~maxSearchDepth:int,
    ~onProgress:option<string=>unit>,
) => {
    onProgress->Belt.Option.forEach(onProgress => onProgress("Proving bottom-up: initialization"))

    let nodesToCreateParentsFor = Belt_MutableQueue.make()

    let maxSearchDepthStr = maxSearchDepth->Belt.Int.toString
    let progressState = ref(progressTrackerMutableMake( ~step=0.01, ~onProgress = _ => (), () ))

    tree->ptClearDists
    let rootNode = tree->ptGetOrCreateNode(expr)
    rootNode->pnSetDist(0)
    nodesToCreateParentsFor->Belt_MutableQueue.add(rootNode)
    let lastDist = ref(0)
    let maxCnt = ref(1)
    let cnt = ref(0)
    while (rootNode->pnGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableQueue.isEmpty)) {
        let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
        if (curNode->pnGetProof->Belt.Option.isNone) {
            let curDist = switch curNode->pnGetDist {
                | None => raise(MmException({msg:`Encountered a node without dist.`}))
                | Some(dist) => dist
            }
            switch onProgress {
                | Some(onProgress) => {
                    if (lastDist.contents != curDist) {
                        lastDist.contents = curDist
                        let curDistStr = curDist->Belt.Int.toString
                        progressState.contents = progressTrackerMutableMake(
                            ~step=0.01,
                            ~onProgress= pct => {
                                let pctStr = (pct  *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString
                                onProgress(`Proving bottom-up: ${curDistStr}/${maxSearchDepthStr} ${pctStr}%`)
                            }, 
                            ()
                        )
                        maxCnt.contents = nodesToCreateParentsFor->Belt_MutableQueue.size + 1
                        cnt.contents = 0
                    }
                    progressState.contents->progressTrackerMutableSetCurrPct(
                        cnt.contents->Belt_Int.toFloat /. maxCnt.contents->Belt_Int.toFloat
                    )
                    cnt.contents = cnt.contents + 1
                }
                | None => ()
            }

            let curExpr = curNode->pnGetExpr
            if (tree->ptIsNewVarDef(curExpr)) {
                curNode->pnAddParent(VarType)
            } else {
                switch tree->ptGetHypByExpr(curExpr) {
                    | Some(hyp) => curNode->pnAddParent(Hypothesis({label:hyp.label}))
                    | None => {
                        let newDist = curDist + 1
                        if (newDist <= maxSearchDepth) {
                            getParents(curExpr, curDist)->Js.Array2.forEach(src => {
                                if (!srcIsBackRef(curExpr, src)) {
                                    curNode->pnAddParent(src)
                                    switch src {
                                        | Assertion({args,err:None}) => 
                                            args->Js_array2.forEach(arg => {
                                                if (arg->pnGetProof->Belt.Option.isNone
                                                        && arg->pnGetDist->Belt.Option.isNone) {
                                                    arg->pnSetDist(newDist)
                                                    nodesToCreateParentsFor->Belt_MutableQueue.add(arg)
                                                }
                                            })
                                        | _ => ()
                                    }
                                }
                            })
                        }
                    }
                }
            }
        }
    }
}

let proveStmtBottomUp = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~params:bottomUpProverParams,
    ~debugLevel:int,
    ~onProgress:option<string=>unit>,
):proofNode => {
    let rootExprs = tree->ptGetRootStmts->Js.Array2.map(stmt => stmt.expr)

    let getParents = (expr:expr, dist:int):array<exprSource> => {
        let parents = findAsrtParentsWithNewVars(
            ~tree,
            ~expr,
            ~stmts = if (params.useRootStmtsAsArgs || dist == 0) {rootExprs} else {[]},
            ~exactOrderOfStmts=false,
            ~asrtLabel = ?(if (dist == 0) {params.asrtLabel} else {None}),
            ~allowEmptyArgs = true,
            ~allowNewVars = dist == 0 && params.allowNewVars,
            ~debugLevel,
            ()
        )
        if (dist == 0) {
            parents
        } else {
            let exprLen = expr->Js_array2.length
            parents->Js.Array2.filter(parent => {
                switch parent {
                    | Assertion({args, frame}) => {
                        let argsAreCorrect = ref(true)
                        let numOfArgs = frame.hyps->Js_array2.length
                        let maxArgIdx = numOfArgs - 1
                        let argIdx = ref(0)
                        while (argIdx.contents <= maxArgIdx && argsAreCorrect.contents) {
                            let arg = args[argIdx.contents]
                            if (frame.hyps[argIdx.contents].typ == E) {
                                argsAreCorrect.contents = switch params.lengthRestriction {
                                    | No => true
                                    | LessEq => arg->pnGetExpr->Js_array2.length <= exprLen
                                    | Less => arg->pnGetExpr->Js_array2.length < exprLen
                                }
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        argsAreCorrect.contents
                    }
                    | _ => true
                }
            })
        }
    }

    proveBottomUp(
        ~tree, 
        ~expr, 
        ~getParents,
        ~maxSearchDepth=params.maxSearchDepth,
        ~onProgress,
    )
    tree->ptGetOrCreateNode(expr)
}

let proveStmt = (
    ~tree, 
    ~expr:expr, 
    ~jstf:option<justification>, 
    ~bottomUpProverParams:option<bottomUpProverParams>,
    ~debugLevel:int,
    ~onProgress:option<string=>unit>,
) => {
    switch bottomUpProverParams {
        | Some(params) => proveStmtBottomUp( ~tree, ~expr, ~params, ~debugLevel, ~onProgress, )->ignore
        | None => {
            switch jstf {
                | None => proveWithoutJustification( ~tree, ~expr, )->ignore
                | Some(jstf) => proveWithJustification( ~tree, ~expr, ~jstf, )->ignore
            }
        }
    }
}

let makeExprToStr = (ctx, ctxMaxVar) => {
    if (ctx->isDebug) {
        let intToSym = i => {
            if (i <= ctxMaxVar) {
                ctx->ctxIntToSymExn(i)
            } else {
                "&" ++ i->Belt.Int.toString
            }
        }
        Some(expr => expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" "))
    } else {
        None
    }
}

let createProofTree = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~addEssentials: bool,
) => {
    let ctxMaxVar = ctx->getNumOfVars - 1
    let hyps = ctx->getAllHyps
    let tree = ptMake(
        ~frms, 
        ~hyps, 
        ~ctxMaxVar, 
        ~disj=ctx->getAllDisj, 
        ~parenCnt, 
        ~exprToStr=makeExprToStr(ctx, ctxMaxVar),
    )
    if (addEssentials) {
        hyps->Belt_MapString.forEach((label,hyp) => {
            if (hyp.typ == E) {
                let node = tree->ptGetOrCreateNode(hyp.expr)
                node->pnAddParent(Hypothesis({label:label}))
                tree->ptAddRootStmt({
                    label,
                    expr: hyp.expr,
                    justification: None,
                })->ignore
            }
        })
    }
    tree
}

let proveFloatings = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<expr>,
    ~parenCnt: parenCnt,
    ()
) => {
    let tree = createProofTree(
        ~ctx,
        ~frms,
        ~parenCnt,
        ~addEssentials=false,
    )

    stmts->Js.Array2.forEach(stmt => {
        proveFloating( ~tree, ~node=tree->ptGetOrCreateNode(stmt) )
    })
    tree
}

let unifyAll = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:option<bottomUpProverParams>=?,
    ~debugLevel:int=1,
    ~onProgress:option<string=>unit>=?,
    ()
):proofTree => {
    let progressState = progressTrackerMutableMake(
        ~step=0.01, 
        ~onProgress=?onProgress->Belt.Option.map(onProgress => {
            pct => onProgress(`Unifying all: ${(pct  *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`)
        }), 
        ()
    )

    let tree = createProofTree(
        ~ctx,
        ~frms,
        ~parenCnt,
        ~addEssentials=true,
    )

    let numOfStmts = stmts->Js_array2.length
    let maxStmtIdx = numOfStmts - 1
    stmts->Js.Array2.forEachi((stmt,stmtIdx) => {
        proveStmt(
            ~tree, 
            ~expr=stmt.expr, 
            ~jstf=stmt.justification,
            ~bottomUpProverParams = if (stmtIdx == maxStmtIdx) {bottomUpProverParams} else {None},
            ~debugLevel,
            ~onProgress =
                if (stmtIdx != maxStmtIdx || bottomUpProverParams->Belt.Option.isNone) {
                    None
                } else {
                    onProgress
                },
        )
        tree->ptAddRootStmt(stmt)

        progressState->progressTrackerMutableSetCurrPct( 
            (stmtIdx+1)->Belt_Int.toFloat /. numOfStmts->Belt_Int.toFloat
        )
    })

    if (ctx->isDebug) {
        tree->ptGetStats
        let nodes = stmts->Js.Array2.map(stmt => tree->ptGetOrCreateNode(stmt.expr))
        //to doto: too many children
        // Js.Console.log2("nodes.length", nodes->Js_array2.length)
    }

    tree
}