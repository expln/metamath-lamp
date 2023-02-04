open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    maxSearchDepth: int,
    lengthRestriction: lengthRestrict,
}

let findAsrtParentsWithoutNewVars = ( 
    ~tree, 
    ~expr, 
    ~restrictExprLen:lengthRestrict, 
    ~framesToSkip: array<string>,
):array<exprSource> => {
    let exprLen = expr->Js_array2.length
    let foundParents = []
    tree->ptGetFrms->Belt_MapString.forEach((_,frm) => {
        let frmExpr = frm.frame.asrt
        if (frmExpr[0] == expr[0] && !(framesToSkip->Js.Array2.includes(frm.frame.label))) {
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
                        && verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx=tree->ptIsDisj)) {
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
                            argsAreCorrect.contents = 
                                restrictExprLen == No
                                || restrictExprLen == LessEq && newExpr->Js_array2.length <= exprLen
                                || newExpr->Js_array2.length < exprLen
                            if (argsAreCorrect.contents) {
                                args[argIdx.contents] = tree->ptGetOrCreateNode(newExpr)
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        if (argsAreCorrect.contents) {
                            foundParents->Js_array2.push( Assertion({ args, label:frm.frame.label, frame:frm.frame }) )->ignore
                        }
                    }
                    Continue
                }
            )->ignore
        }
    })
    foundParents
}

let proveBottomUp = (
    ~tree:proofTree, 
    ~node:proofNode, 
    ~getParents:expr=>array<exprSource>,
    ~maxSearchDepth:option<int>,
    ~onDistProgress:int=>unit,
) => {
    /*
    If a node has a proof, no need to prove it again.
    If a node has Some(parents), it could appear as a side effect of proving other nodes and it was left unproved 
        because the root node became proved first.
    It follows from the statement above, that all unproved nodes have to have most complete collection of parents, 
        otherwise there is a risk to miss existing proof for this node. So we must not interrupt the process of adding 
        parents even if the root node becomes proved.
    */
    if (node->pnGetProof->Belt.Option.isNone) {
        if (maxSearchDepth->Belt.Option.isSome) {
            tree->ptEraseDists
        }

        let nodesToCreateParentsFor = Belt_MutableQueue.make()
        let savedNodes = Belt_HashSet.make( ~id=module(ExprHash), ~hintSize = 16 )

        let saveNodeToCreateParentsFor = (node,dist) => {
            switch node->pnGetProof {
                | Some(_) => ()
                | None => {
                    if (!(savedNodes->Belt_HashSet.has(node->pnGetExpr))) {
                        let notTooFarFromRoot = switch maxSearchDepth {
                            | None => true
                            | Some(maxSearchDepth) => {
                                switch dist {
                                    | None => 
                                        raise(MmException({
                                            msg: `proveBottomUp: dist must not be None when maxSearchDepth is Some.`
                                        }))
                                    | Some(dist) => dist < maxSearchDepth
                                }
                            }
                        }
                        if (notTooFarFromRoot) {
                            switch dist {
                                | None => ()
                                | Some(dist) => {
                                    switch node->pnGetDist {
                                        | Some(curDist) if curDist <= dist => ()
                                        | _ => node->pnSetDist(Some(dist))
                                    }
                                }
                            }
                            savedNodes->Belt_HashSet.add(node->pnGetExpr)
                            nodesToCreateParentsFor->Belt_MutableQueue.add(node)
                        }
                    }
                }
            }
        }

        let rootNode = node
        saveNodeToCreateParentsFor(rootNode, maxSearchDepth->Belt_Option.map(_ => 0))
        let lastDist = ref(0)
        while (rootNode->pnGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableQueue.isEmpty)) {
            let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
            if (curNode->pnGetProof->Belt.Option.isNone) {
                let newDist = switch curNode->pnGetDist {
                    | Some(curDist) => {
                        if (lastDist.contents != curDist) {
                            lastDist.contents = curDist
                            onDistProgress(curDist)
                        }
                        Some(curDist + 1)
                    }
                    | None => None
                }
                switch curNode->pnGetParents {
                    | Some(parents) => {
                        parents->Js.Array2.forEach(parent => {
                            switch parent {
                                | Assertion({args}) => args->Js_array2.forEach(saveNodeToCreateParentsFor(_,newDist))
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
                                    getParents(curNode->pnGetExpr)->Js.Array2.forEach(parent => {
                                        curNode->pnAddParent(parent)
                                        switch parent {
                                            | Assertion({args}) => 
                                                args->Js_array2.forEach(saveNodeToCreateParentsFor(_,newDist))
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
        // if (maxSearchDepth->Belt_Option.isSome) {
        //     Js.Console.log2("savedNodes.size", savedNodes->Belt_HashSet.size)
        // }
    }
}

let dummyOnDistProgress = _ => ()

let proveFloating = (tree, node, ~framesToSkip: array<string>) => {
    if (!(node->pnIsInvalidFloating) && node->pnGetProof->Belt.Option.isNone) {
        proveBottomUp(
            ~tree, 
            ~node, 
            ~getParents = expr => findAsrtParentsWithoutNewVars(~tree, ~expr, ~restrictExprLen=LessEq, ~framesToSkip),
            ~maxSearchDepth = None,
            ~onDistProgress = dummyOnDistProgress,
        )
        if (node->pnGetProof->Belt.Option.isNone) {
            node->pnSetInvalidFloating(true)
        }
    }
}

let findAsrtParentsWithNewVars = (
    ~tree,
    ~expr:expr,
    ~stmts:array<expr>,
    ~framesToSkip: array<string>,
    ~exactOrderOfStmts:bool=false,
    ~asrtLabel:option<string>=?,
    ~allowEmptyArgs:bool=false,
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
        ~frameFilter = frame => !(framesToSkip->Js.Array2.includes(frame.label))
                                && asrtLabel
                                    ->Belt_Option.map(asrtLabel => frame.label == asrtLabel)
                                    ->Belt_Option.getWithDefault(true),
        ~onMatchFound = res => {
            applResults->Js_array2.push(res)->ignore
            Continue
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
        let typesAreCorrect = ref(true)
        let argIdx = ref(0)
        let maxArgIdx = numOfArgs - 1
        while (argIdx.contents <= maxArgIdx && typesAreCorrect.contents) {
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
            if (frame.hyps[argIdx.contents].typ == F) {
                proveFloating(tree, argNode, ~framesToSkip)
                typesAreCorrect.contents = argNode->pnGetProof->Belt.Option.isSome
            }
            argIdx.contents = argIdx.contents + 1
        }
        if (typesAreCorrect.contents) {
            foundParents->Js.Array2.push( Assertion({ args, label:applResult.asrtLabel, frame}) )->ignore
        }
    })
    foundParents
}

let proveWithoutJustification = (~tree, ~prevStmts:array<expr>, ~stmt:expr, ~framesToSkip: array<string>):proofNode => {
    let node = tree->ptGetOrCreateNode(stmt)
    if (node->pnGetProof->Belt.Option.isNone && node->pnGetParents->Belt.Option.isNone) {
        let parents = findAsrtParentsWithNewVars( ~tree, ~expr=stmt, ~framesToSkip, ~stmts=prevStmts, () )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent)
            node->pnGetProof
        })->ignore
    }
    node
}

let proveWithJustification = (
    ~tree, ~args:array<expr>, ~asrtLabel:string, ~stmt:expr, ~framesToSkip: array<string>
):proofNode => {
    let node = tree->ptGetOrCreateNode(stmt)
    if (node->pnGetProof->Belt.Option.isNone && node->pnGetParents->Belt.Option.isNone) {
        let parents = findAsrtParentsWithNewVars( 
            ~tree, 
            ~expr=stmt, 
            ~stmts=args, 
            ~asrtLabel,
            ~framesToSkip,
            ~exactOrderOfStmts=true,
            () 
        )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent)
            node->pnGetProof
        })->ignore
    }
    node
}

let proveStmtBottomUp = (
    ~tree, ~prevStmts:array<expr>, ~stmt:expr, ~params:bottomUpProverParams, ~framesToSkip: array<string>,
    ~onProgress:option<float=>unit>,
):proofNode => {
    let ctxMaxVar = tree->ptGetCtxMaxVar
    let exprHasNewVars = expr => expr->Js_array2.some(s => ctxMaxVar < s)

    // let numOfGetParentsCalls = ref(0)
    // let numOfParentsProcessed = ref(0)
    // let numOfParentsReturned = ref(0)
    // let numOfProveFloatingCalls = ref(0)

    let getParents = expr => {
        // numOfGetParentsCalls.contents = numOfGetParentsCalls.contents + 1
        let parents = findAsrtParentsWithoutNewVars( 
            ~tree, ~expr, ~restrictExprLen=params.lengthRestriction, ~framesToSkip
        )
        // numOfParentsProcessed.contents = numOfParentsProcessed.contents + parents->Js.Array2.length
        let parents = parents->Js.Array2.filter(parent => {
                switch parent {
                    | Assertion({args, frame}) => {
                        let numOfArgs = frame.hyps->Js_array2.length
                        let argsAreCorrect = ref(true)
                        let argIdx = ref(0)
                        let maxArgIdx = numOfArgs - 1
                        while (argIdx.contents <= maxArgIdx && argsAreCorrect.contents) {
                            let arg = args[argIdx.contents]
                            if (frame.hyps[argIdx.contents].typ == F) {
                                if (!(arg->pnIsInvalidFloating)) {
                                    // numOfProveFloatingCalls.contents = numOfProveFloatingCalls.contents + 1
                                    proveFloating(tree, arg, ~framesToSkip)
                                }
                                argsAreCorrect.contents = arg->pnGetProof->Belt.Option.isSome
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        argsAreCorrect.contents
                    }
                    | _ => true
                }
            })
        // numOfParentsReturned.contents = numOfParentsReturned.contents + parents->Js.Array2.length
        parents
    }

    let node = tree->ptGetOrCreateNode(stmt)
    if (node->pnGetProof->Belt.Option.isNone) {
        let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))
        let firstLevelParents = findAsrtParentsWithNewVars( 
            ~tree, 
            ~expr=stmt, 
            ~stmts=prevStmts, 
            ~allowEmptyArgs=true, 
            ~framesToSkip, 
            ~asrtLabel=?params.asrtLabel,
            ()
        )
        firstLevelParents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent)
            node->pnGetProof
        })->ignore
        let flParentsLen = firstLevelParents->Js.Array2.length
        if (flParentsLen > 0) {
            let pctPerFlParent = 1. /. flParentsLen->Belt.Int.toFloat
            let maxSearchDepth = params.maxSearchDepth-1
            if (maxSearchDepth > 0) {
                firstLevelParents->Js.Array2.forEachi((firstLevelParent, pi) => {
                    if (node->pnGetProof->Belt.Option.isNone) {
                        switch firstLevelParent {
                            | Assertion({args}) => {
                                let argsLen = args->Js.Array2.length
                                if (argsLen > 0) {
                                    let pctPerArg = pctPerFlParent /. argsLen->Belt.Int.toFloat
                                    args->Js.Array2.forEachi((arg,ai) => {
                                        if (node->pnGetProof->Belt.Option.isNone && arg->pnGetProof->Belt.Option.isNone
                                                && !exprHasNewVars(arg->pnGetExpr)) {
                                            // Js.Console.log2("(pi,ai)", (pi,ai))
                                            // numOfGetParentsCalls.contents = 0
                                            // numOfParentsProcessed.contents = 0
                                            // numOfParentsReturned.contents = 0
                                            // numOfProveFloatingCalls.contents = 0
                                            let pctPerDepth = pctPerArg /. maxSearchDepth->Belt.Int.toFloat
                                            proveBottomUp(
                                                ~tree, 
                                                ~node=arg, 
                                                ~getParents,
                                                ~maxSearchDepth = Some(maxSearchDepth),
                                                ~onDistProgress = curDist => {
                                                    let depthDone = curDist - 1
                                                    if (depthDone > 0) {
                                                        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
                                                            pi->Belt_Int.toFloat *. pctPerFlParent
                                                                +. ai->Belt_Int.toFloat *. pctPerArg
                                                                +. depthDone->Belt_Int.toFloat *. pctPerDepth
                                                        )
                                                    }
                                                },
                                            )
                                            // Js.Console.log2("numOfGetParentsCalls", numOfGetParentsCalls.contents)
                                            // Js.Console.log2("numOfParentsProcessed", numOfParentsProcessed.contents)
                                            // Js.Console.log2("numOfParentsReturned", numOfParentsReturned.contents)
                                            // Js.Console.log2("numOfProveFloatingCalls", numOfProveFloatingCalls.contents)
                                        }
                                        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
                                            pi->Belt_Int.toFloat *. pctPerFlParent
                                                +. (ai+1)->Belt_Int.toFloat *. pctPerArg
                                        )
                                    })
                                }
                            }
                            | _ => ()
                        }
                    }
                    progressState.contents = progressState.contents->progressTrackerSetCurrPct(
                        (pi+1)->Belt_Int.toFloat *. pctPerFlParent
                    )
                })
            }
        }
    }
    node
}

let getStatementsFromJustification = (
    ~tree:proofTree,
    ~stmts:array<rootStmt>,
    ~justification: justification,
):array<expr> => {
    let getStmtByLabel = label => {
        switch stmts->Js.Array2.find(stmt => stmt.label == label) {
            | Some(stmt) => Some(stmt.expr)
            | None => {
                switch tree->ptGetHypByLabel(label) {
                    | Some(hyp) => Some(hyp.expr)
                    | None => None
                }
            }
        }
    }
    let foundStmts = justification.args
        ->Js_array2.map(getStmtByLabel)
        ->Js_array2.filter(Belt_Option.isSome)
        ->Js_array2.map(Belt_Option.getExn)
    if (foundStmts->Js_array2.length == justification.args->Js.Array2.length) {
        foundStmts
    } else {
        []
    }
}

let proveStmt = (
    ~tree, 
    ~prevStmts:array<rootStmt>, 
    ~stmt:rootStmt, 
    ~framesToSkip: array<string>,
    ~jstf:option<justification>, 
    ~bottomUpProverParams:option<bottomUpProverParams>,
    ~onProgress:option<float=>unit>,
) => {
    switch bottomUpProverParams {
        | Some(params) => {
            proveStmtBottomUp( 
                ~tree, 
                ~prevStmts=prevStmts->Js_array2.map(stmt => stmt.expr),
                ~stmt=stmt.expr,
                ~params,
                ~framesToSkip,
                ~onProgress,
            )->ignore
        }
        | None => {
            switch jstf {
                | None => {
                    proveWithoutJustification(
                        ~tree, 
                        ~prevStmts=prevStmts->Js_array2.map(stmt => stmt.expr),
                        ~stmt=stmt.expr,
                        ~framesToSkip,
                    )->ignore
                }
                | Some(jstf) => {
                    proveWithJustification(
                        ~tree, 
                        ~args=getStatementsFromJustification(
                            ~tree,
                            ~stmts=prevStmts,
                            ~justification=jstf,
                        ), 
                        ~asrtLabel=jstf.asrt, 
                        ~stmt=stmt.expr,
                        ~framesToSkip,
                    )->ignore
                }
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
            }
        })
    }
    tree
}

let proveFloatings = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<expr>,
    ~framesToSkip: array<string>=[],
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
        proveFloating( tree, tree->ptGetOrCreateNode(stmt), ~framesToSkip )
    })
    tree
}

let unifyAll = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~framesToSkip: array<string>=[],
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:option<bottomUpProverParams>=?,
    ~onProgress:option<float=>unit>=?,
    ()
) => {
    let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))

    let tree = createProofTree(
        ~ctx,
        ~frms,
        ~parenCnt,
        ~addEssentials=true,
    )

    let prevStmts = []
    ctx->getAllHyps->Belt_MapString.forEach((label,hyp) => {
        if (hyp.typ == E) {
            prevStmts->Js_array2.push({
                label,
                expr: hyp.expr,
                justification: None,
            })->ignore
        }
    })
    let numOfStmts = stmts->Js_array2.length
    if (numOfStmts > 0) {
        let pctPerStmt = (1. /. numOfStmts->Belt.Int.toFloat)
            *. if (bottomUpProverParams->Belt.Option.isNone) { 1. } else { 0.1 }
        let maxStmtIdx = numOfStmts - 1
        stmts->Js.Array2.forEachi((stmt,stmtIdx) => {
            proveStmt(
                ~tree, 
                ~prevStmts,
                ~stmt, 
                ~jstf=stmt.justification,
                ~bottomUpProverParams = if (stmtIdx == maxStmtIdx) {bottomUpProverParams} else {None},
                ~framesToSkip,
                ~onProgress = 
                    if (stmtIdx != maxStmtIdx) {
                        None
                    } else {
                        Some(pct => {
                            progressState.contents = progressState.contents->progressTrackerSetCurrPct( pct *. 0.9 )
                        })
                    },
            )
            prevStmts->Js_array2.push(stmt)->ignore

            if (stmtIdx == maxStmtIdx) {
                progressState.contents = progressState.contents->progressTrackerSetCurrPct( 1. )
            } else {
                progressState.contents = progressState.contents->progressTrackerSetCurrPct( 
                    stmtIdx->Belt_Int.toFloat *. pctPerStmt
                )
            }
        })
    }

    if (ctx->isDebug) {
        tree->ptGetStats
        let nodes = stmts->Js.Array2.map(stmt => tree->ptGetOrCreateNode(stmt.expr))
        //to doto: too many children
        Js.Console.log2("nodes.length", nodes->Js_array2.length)
    }

    tree
}