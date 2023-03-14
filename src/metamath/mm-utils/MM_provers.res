open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree
open MM_unification_debug
open MM_statements_dto

type lengthRestrict = No | LessEq | Less

type bottomUpProverParams = {
    asrtLabel: option<string>,
    maxSearchDepth: int,
    lengthRestriction: lengthRestrict,
    allowNewStmts: bool,
    allowNewVars: bool,
    args0: array<expr>,
    args1: array<expr>,
    maxNumberOfBranches: option<int>,
}

let findNonAsrtParent = ( ~tree, ~expr, ):option<exprSrc> => {
    if (tree->ptIsNewVarDef(expr)) {
        Some(VarType)
    } else {
        switch tree->ptGetHypByExpr(expr) {
            | Some(hyp) => Some(Hypothesis({label:hyp.label}))
            | None => None
        }
    }
}

let findAsrtParentsWithoutNewVars = ( 
    ~tree, 
    ~expr, 
    ~restrictExprLen:lengthRestrict, 
):array<exprSrc> => {
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
                                    msg:`Work variables are not supported in findAsrtParentsWithoutNewVars().`
                                }))
                            )
                            argsAreCorrect.contents = switch restrictExprLen {
                                | No => true
                                | LessEq => newExpr->Js_array2.length <= exprLen
                                | Less => newExpr->Js_array2.length < exprLen
                            }
                            if (argsAreCorrect.contents) {
                                args[argIdx.contents] = tree->ptGetNode(newExpr)
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        if (argsAreCorrect.contents) {
                            foundParents->Js_array2.push( 
                                Assertion({ args, frame:frm.frame, missingDisj:None })
                            )->ignore
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

        let saveArgs = (src:exprSrc) => {
            switch src {
                | Assertion({args}) => args->Js_array2.forEach(saveNodeToCreateParentsFor)
                | _ => ()
            }
        }

        let rootNode = node
        saveNodeToCreateParentsFor(rootNode)
        while (rootNode->pnGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableQueue.isEmpty)) {
            let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
            if (curNode->pnGetProof->Belt.Option.isNone) {
                switch curNode->pnGetFParents {
                    | Some(fParents) => fParents->Js_array2.forEach(saveArgs)
                    | None => {
                        let curExpr = curNode->pnGetExpr
                        switch findNonAsrtParent(~tree, ~expr=curExpr) {
                            | Some(parent) => curNode->pnAddParent(parent, false)
                            | None => {
                                findAsrtParentsWithoutNewVars(
                                    ~tree, ~expr=curExpr, ~restrictExprLen=LessEq
                                )->Js.Array2.forEach(parent => {
                                    curNode->pnAddParent(parent, false)
                                    saveArgs(parent)
                                })
                            }
                        }
                    }
                }
            }
        }
        if (rootNode->pnGetProof->Belt.Option.isNone) {
            rootNode->pnSetInvalidFloating
        }
    }
}

let findAsrtParentsWithNewVars = (
    ~tree,
    ~expr:expr,
    ~args:array<expr>,
    ~exactOrderOfArgs:bool,
    ~asrtLabel:option<string>=?,
    ~allowEmptyArgs:bool,
    ~allowNewVars:bool,
    ~strictDisj:bool,
    ~debugLevel:int=0,
    ~maxNumberOfResults: option<int>=?,
    ~onProgress:option<int=>unit>=?,
    ()
):array<exprSrc> => {
    let applResults = []
    let restrictFoundCnt = maxNumberOfResults->Belt_Option.isSome
    let maxFoundCnt = maxNumberOfResults->Belt_Option.getWithDefault(0)
    let frameFilter = switch asrtLabel {
        | None => _ => true
        | Some(asrtLabel) => (frame:frame) => frame.label == asrtLabel
    }

    let maxVarBeforeSearch = tree->ptGetMaxVar
    applyAssertions(
        ~maxVar = maxVarBeforeSearch,
        ~frms = tree->ptGetFrms,
        ~isDisjInCtx = tree->ptIsDisj,
        ~parenCnt=tree->ptGetParenCnt,
        ~statements = args,
        ~exactOrderOfStmts = exactOrderOfArgs,
        ~allowEmptyArgs,
        ~allowNewVars,
        ~result = expr,
        ~frameFilter,
        ~strictDisj,
        ~debugLevel,
        ~onMatchFound = res => {
            applResults->Js_array2.push(res)->ignore

            let foundCnt = applResults->Js.Array2.length
            switch onProgress {
                | Some(onProgress) => {
                    if (mod(foundCnt, 100) == 0) {
                        onProgress(foundCnt)
                    }
                }
                | _ => ()
            }
            if (restrictFoundCnt && foundCnt >= maxFoundCnt) {
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
                raise(MmException({
                    msg:`Cannot find an assertion with label ${applResult.asrtLabel} in findAsrtParentsWithNewVars.`
                }))
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
                ~createWorkVar = _ => raise(MmException({
                    msg:`New work variables are not expected here [findAsrtParentsWithNewVars].`
                }))
            )
            let maxI = argExpr->Js_array2.length-1
            for i in 0 to maxI {
                let sym = argExpr[i]
                if (sym > maxVarBeforeSearch) {
                    argExpr[i] = switch applNewVarToTreeNewVar->Belt_MutableMapInt.get(sym) {
                        | None => raise(MmException({msg:`Could not replace an applVar with a treeVar.`}))
                        | Some(treeVar) => treeVar
                    }
                }
            }
            let argNode = tree->ptGetNode(argExpr)
            args[argIdx.contents] = argNode
            if (frame.hyps[argIdx.contents].typ == F
                    && applResult.err->Belt_Option.isNone && unprovedFloating.contents->Belt_Option.isNone
            ) {
                proveFloating(~tree, ~node=argNode)
                if (argNode->pnGetProof->Belt.Option.isNone) {
                    unprovedFloating.contents = Some(argNode->pnGetExpr)
                }
            }
            argIdx.contents = argIdx.contents + 1
        }
        let err = switch applResult.err {
            | Some(_) => applResult.err
            | None => unprovedFloating.contents->Belt_Option.map(expr => UnprovedFloating({expr:expr}))
        }
        switch err {
            | None => {
                let missingDisj = applResult.missingDisj->Belt_Option.map(applMissingDisj => {
                    let treeMissingDisj = disjMutableMake()
                    applMissingDisj->disjForEach((n,m) => {
                        treeMissingDisj->disjAddPair(
                            applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(n, n),
                            applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(m, m),
                        )
                    })
                    treeMissingDisj
                })
                foundParents->Js.Array2.push( Assertion({ 
                    args, 
                    frame,
                    missingDisj,
                }) )->ignore
            }
            | Some(err) => {
                if (debugLevel != 0) {
                    foundParents->Js.Array2.push( AssertionWithErr({ 
                        args, 
                        frame,
                        err
                    }) )->ignore
                }
            }
        }
    })
    foundParents
}

let proveWithoutJustification = (~tree:proofTree, ~expr:expr):proofNode => {
    let node = tree->ptGetNode(expr)
    if (node->pnGetProof->Belt.Option.isNone) {
        let parents = findAsrtParentsWithNewVars(
            ~tree, 
            ~expr, 
            ~args=tree->ptGetRootStmts->Js.Array2.map(stmt => stmt.expr),
            ~exactOrderOfArgs=false,
            ~allowEmptyArgs=false,
            ~allowNewVars=false,
            ~strictDisj=true,
            () 
        )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent, true)
            node->pnGetProof
        })->ignore
    }
    node
}

let getStatementsFromJustification = (
    ~tree:proofTree,
    ~jstf: jstf,
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
    ~jstf: jstf,
):proofNode => {
    let node = tree->ptGetNode(expr)
    if (node->pnGetProof->Belt.Option.isNone) {
        switch getStatementsFromJustification( ~tree, ~jstf, ) {
            | None => ()
            | Some(args) => {
                let parents = findAsrtParentsWithNewVars(
                    ~tree,
                    ~expr,
                    ~args,
                    ~exactOrderOfArgs=true,
                    ~allowEmptyArgs=false,
                    ~allowNewVars=false,
                    ~asrtLabel=jstf.label,
                    ~strictDisj=true,
                    ()
                )
                parents->Expln_utils_common.arrForEach(parent => {
                    node->pnAddParent(parent, true)
                    node->pnGetProof
                })->ignore
            }
        }
    }
    node
}

let srcIsBackRef = (expr:expr, src:exprSrc):bool => {
    switch src {
        | Assertion({args}) => args->Js_array2.some(arg => expr->exprEq(arg->pnGetExpr))
        | _ => false
    }
}

let proveBottomUp = (
    ~tree:proofTree,
    ~expr:expr,
    ~getParents:(expr,int,option<int=>unit>)=>array<exprSrc>,
    ~maxSearchDepth:int,
    ~onProgress:option<string=>unit>,
) => {
    let nodesToCreateParentsFor = Belt_MutableQueue.make()

    let maxSearchDepthStr = maxSearchDepth->Belt.Int.toString
    let progressState = ref(progressTrackerMutableMake( ~step=0.01, ~onProgress = _ => (), () ))

    tree->ptClearDists
    let rootNode = tree->ptGetNode(expr)
    rootNode->pnSetDist(0)
    nodesToCreateParentsFor->Belt_MutableQueue.add(rootNode)
    let lastDist = ref(0)
    let maxCnt = ref(1)
    let cnt = ref(0)
    let onProgressP:ref<option<int=>unit>> = ref(onProgress->Belt_Option.map(onProgress => numOfParents => {
        onProgress(`Proving bottom-up: 0/${maxSearchDepthStr} ${numOfParents->Belt_Int.toString}`)
    }))
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
                        onProgressP.contents = None
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
            switch findNonAsrtParent(~tree, ~expr=curExpr) {
                | Some(parent) => curNode->pnAddParent(parent, true)
                | None => {
                    let newDist = curDist + 1
                    if (newDist <= maxSearchDepth) {
                        getParents(curExpr, curDist, onProgressP.contents)->Js.Array2.forEach(src => {
                            if (!srcIsBackRef(curExpr, src)) {
                                curNode->pnAddParent(src, true)
                                switch src {
                                    | Assertion({args}) => 
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

let proveStmtBottomUp = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~params:bottomUpProverParams,
    ~debugLevel:int,
    ~onProgress:option<string=>unit>,
):proofNode => {

    let getParents = (expr:expr, dist:int, onProgress:option<int=>unit>):array<exprSrc> => {
        let parents = findAsrtParentsWithNewVars(
            ~tree,
            ~expr,
            ~args = if (dist == 0) {params.args0} else {params.args1},
            ~exactOrderOfArgs=false,
            ~asrtLabel = ?(if (dist == 0) {params.asrtLabel} else {None}),
            ~allowEmptyArgs = params.allowNewStmts,
            ~allowNewVars = dist == 0 && params.allowNewVars,
            ~strictDisj=true,
            ~debugLevel,
            ~maxNumberOfResults=?params.maxNumberOfBranches,
            ~onProgress?,
            ()
        )
        if (dist == 0) {
            parents
        } else {
            let exprLen = expr->Js_array2.length
            parents->Js.Array2.filter(parent => {
                switch parent {
                    | VarType | Hypothesis(_) | AssertionWithErr(_) => true
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
    tree->ptGetNode(expr)
}

let proveStmt = (
    ~tree, 
    ~expr:expr, 
    ~jstf:option<jstf>,
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
                let node = tree->ptGetNode(hyp.expr)
                node->pnAddParent(Hypothesis({label:label}), true)
                tree->ptAddRootStmt({
                    isHyp: true,
                    label,
                    expr: hyp.expr,
                    jstf: None,
                })->ignore
            }
        })
    }
    tree
}

let proveFloatings = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~floatingsToProve: array<expr>,
    ~parenCnt: parenCnt,
    ()
) => {
    let tree = createProofTree(
        ~ctx,
        ~frms,
        ~parenCnt,
        ~addEssentials=false,
    )

    floatingsToProve->Js.Array2.forEach(expr => {
        proveFloating( ~tree, ~node=tree->ptGetNode(expr) )
    })
    tree
}

//todo: review this function
let unifyAll = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~rootProvables: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:option<bottomUpProverParams>=?,
    ~debugLevel:int=0,
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

    let numOfStmts = rootProvables->Js_array2.length
    let maxStmtIdx = numOfStmts - 1
    rootProvables->Js.Array2.forEachi((stmt,stmtIdx) => {
        proveStmt(
            ~tree, 
            ~expr=stmt.expr, 
            ~jstf=stmt.jstf,
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

    tree
}