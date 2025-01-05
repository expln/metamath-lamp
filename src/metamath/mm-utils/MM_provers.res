open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree
open MM_unification_debug
open MM_statements_dto
open Common
open MM_wrk_settings
open MM_apply_asrt_matcher_type
open MM_apply_asrt_matcher
open MM_bottom_up_prover_params

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
    ~frameRestrict:frameRestrict,
    ~onResult: exprSrc => unit,
):unit => {
    let exprLen = expr->Array.length
    tree->ptGetFrms->frmsForEach(~typ=expr->Array.getUnsafe(0), frm => {
        if (frm.frame->frameIsAllowed(frameRestrict)) {
            let frmExpr = frm.frame.asrt
            iterateSubstitutions(
                ~frmExpr,
                ~expr,
                ~frmConstParts = frm.frmConstParts->Array.getUnsafe(frm.numOfHypsE),
                ~constParts = frm.constParts->Array.getUnsafe(frm.numOfHypsE),
                ~varGroups = frm.varGroups->Array.getUnsafe(frm.numOfHypsE),
                ~subs = frm.subs,
                ~parenCnt=tree->ptGetParenCnt,
                ~consumer = subs => {
                    if (subs.isDefined->Array.every(b=>b)
                        && verifyDisjoints(
                                ~frmDisj=frm.frame.disj, 
                                ~subs, 
                                ~isDisjInCtx=ptIsDisjInCtx(tree, ...),
                                ~debugLevel=0
                            )->Belt.Option.isNone
                    ) {
                        let hyps = frm.frame.hyps
                        let numOfArgs = hyps->Array.length
                        let args = Expln_utils_common.createArray(numOfArgs)
                        let argsAreCorrect = ref(true)
                        let argIdx = ref(0)
                        let maxArgIdx = numOfArgs - 1
                        while (argIdx.contents <= maxArgIdx && argsAreCorrect.contents) {
                            let newExpr = applySubs(
                                ~frmExpr = (hyps->Array.getUnsafe(argIdx.contents)).expr,
                                ~subs,
                                ~createWorkVar = _ => raise(MmException({
                                    msg:`Work variables are not supported in findAsrtParentsWithoutNewVars().`
                                }))
                            )
                            argsAreCorrect.contents = switch restrictExprLen {
                                | No => true
                                | LessEq => newExpr->Array.length <= exprLen
                                | Less => newExpr->Array.length < exprLen
                            }
                            if (argsAreCorrect.contents) {
                                let node = tree->ptGetNode(newExpr)
                                args[argIdx.contents] = node
                                argsAreCorrect := !(node->pnIsInvalidFloating)
                            }
                            argIdx.contents = argIdx.contents + 1
                        }
                        if (argsAreCorrect.contents) {
                            onResult( Assertion({ args, frame:frm.frame }) )
                        }
                    }
                    Continue
                }
            )->ignore
        }
    })
}

let proveFloating = (
    ~tree:proofTree, 
    ~node:proofNode,
    ~frameRestrict:frameRestrict,
    ~nodesToCreateParentsFor:arrayQueue<proofNode>,
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
        nodesToCreateParentsFor->arrayQueueReset
        let savedNodes = Belt_HashSetInt.make( ~hintSize = 128 )

        let saveNodeToCreateParentsFor = node => {
            switch node->pnGetProof {
                | Some(_) => ()
                | None => {
                    let nodeId = node->pnGetId
                    if (!(savedNodes->Belt_HashSetInt.has(nodeId))) {
                        savedNodes->Belt_HashSetInt.add(nodeId)
                        nodesToCreateParentsFor->arrayQueueAdd(node)
                    }
                }
            }
        }

        let saveArgs = (src:exprSrc) => {
            switch src {
                | Assertion({args}) => args->Array.forEach(saveNodeToCreateParentsFor)
                | _ => ()
            }
        }

        let getNextNodeToProve = ():option<proofNode> => {
            let nextNodeRef = ref(nodesToCreateParentsFor->arrayQueuePop)
            while (
                nextNodeRef.contents->Belt_Option.mapWithDefault(false, nextNode => {
                    nextNode->pnGetProof->Belt.Option.isSome || nextNode->pnIsInvalidFloating
                })
            ) {
                nextNodeRef := nodesToCreateParentsFor->arrayQueuePop
            }
            nextNodeRef.contents
        }

        let rootNode = node
        saveNodeToCreateParentsFor(rootNode)->ignore

        let parentFound = ref(false)
        let currNodeRef = ref(getNextNodeToProve())
        while (rootNode->pnGetProof->Belt_Option.isNone && currNodeRef.contents->Belt_Option.isSome) {
            let curNode = currNodeRef.contents->Belt_Option.getExn
            switch curNode->pnGetFParents {
                | Some(fParents) => fParents->Array.forEach(saveArgs)
                | None => {
                    let curExpr = curNode->pnGetExpr
                    switch findNonAsrtParent(~tree, ~expr=curExpr) {
                        | Some(parent) => curNode->pnAddParent(parent, false, false)
                        | None => {
                            parentFound := false
                            findAsrtParentsWithoutNewVars(
                                ~tree, ~expr=curExpr, ~restrictExprLen=LessEq, ~frameRestrict,
                                ~onResult = parent => {
                                    curNode->pnAddParent(parent, false, false)
                                    saveArgs(parent)
                                    parentFound := true
                                }
                            )
                            if (!parentFound.contents) {
                                curNode->pnSetInvalidFloating
                            }
                        }
                    }
                }
            }
            currNodeRef := getNextNodeToProve()
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
    ~allowEmptyArgs:bool,
    ~allowNewVars:bool,
    ~allowNewDisjForExistingVars:bool,
    ~frmsToUse:option<array<string>>=?,
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
    ~debugLevel:int=0,
    ~maxNumberOfResults: option<int>=?,
    ~onProgress:option<int=>unit>=?
):array<exprSrc> => {
    let floatingNodesToCreateParentsFor = arrayQueueMake(1000)
    let applResults = []
    let restrictFoundCnt = maxNumberOfResults->Belt_Option.isSome
    let maxFoundCnt = maxNumberOfResults->Belt_Option.getWithDefault(0)
    let isFrameAllowed = (frame:frame) => frame->frameIsAllowed(allowedFrms.inEssen)

    let maxVarBeforeSearch = tree->ptGetMaxVar
    applyAssertions(
        ~maxVar = maxVarBeforeSearch,
        ~frms = tree->ptGetFrms,
        ~frmsToUse?,
        ~isFrameAllowed,
        ~isDisjInCtx = ptIsDisjInCtx(tree, ...),
        ~parenCnt=tree->ptGetParenCnt,
        ~statements = args,
        ~exactOrderOfStmts = exactOrderOfArgs,
        ~allowEmptyArgs,
        ~allowNewVars,
        ~result = expr,
        ~allowNewDisjForExistingVars,
        ~combCntMax,
        ~debugLevel,
        ~onMatchFound = res => {
            applResults->Array.push(res)

            let foundCnt = applResults->Array.length
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
        }
    )
    let foundParents = []
    applResults->Array.forEach(applResult => {
        let frame = applResult.frame
        switch applResult.err {
            | Some(NoUnifForAsrt(_)) | Some(NoUnifForArg(_)) | Some(NewVarsAreDisabled(_)) => {
                if (debugLevel != 0) {
                    foundParents->Array.push( 
                        AssertionWithErr({ args:[], frame, err:applResult.err->Belt.Option.getExn })
                    )
                }
            }
            | Some(TooManyCombinations(_)) => {
                foundParents->Array.push( 
                    AssertionWithErr({ args:[], frame, err:applResult.err->Belt.Option.getExn })
                )
            }
            | None | Some(UnifErr) | Some(DisjCommonVar(_)) | Some(Disj(_)) | Some(UnprovedFloating(_)) => {
                let applNewVarToTreeNewVar = Belt_MutableMapInt.make()
                applResult.newVars->Array.forEachWithIndex((applResNewVar,i) => {
                    let newVarType = applResult.newVarTypes->Array.getUnsafe(i)
                    let treeNewVar = tree->ptAddNewVar(newVarType)
                    applNewVarToTreeNewVar->Belt_MutableMapInt.set(applResNewVar,treeNewVar)
                })
                let numOfArgs = frame.hyps->Array.length
                let args = Expln_utils_common.createArray(numOfArgs)
                let unprovedFloating = ref(None)
                let argIdx = ref(0)
                let maxArgIdx = numOfArgs - 1

                while (argIdx.contents <= maxArgIdx && (unprovedFloating.contents->Belt_Option.isNone || debugLevel != 0)) {
                    let argExpr = applySubs(
                        ~frmExpr = (frame.hyps->Array.getUnsafe(argIdx.contents)).expr,
                        ~subs=applResult.subs,
                        ~createWorkVar = _ => raise(MmException({
                            msg:`New work variables are not expected here [findAsrtParentsWithNewVars].`
                        }))
                    )
                    let maxI = argExpr->Array.length-1
                    for i in 0 to maxI {
                        let sym = argExpr->Array.getUnsafe(i)
                        if (sym > maxVarBeforeSearch) {
                            argExpr[i] = switch applNewVarToTreeNewVar->Belt_MutableMapInt.get(sym) {
                                | None => raise(MmException({msg:`Could not replace an applVar with a treeVar.`}))
                                | Some(treeVar) => treeVar
                            }
                        }
                    }
                    let argNode = tree->ptGetNode(argExpr)
                    args[argIdx.contents] = argNode
                    if ((frame.hyps->Array.getUnsafe(argIdx.contents)).typ == F
                            && applResult.err->Belt_Option.isNone && unprovedFloating.contents->Belt_Option.isNone
                    ) {
                        proveFloating(
                            ~tree, 
                            ~node=argNode,
                            ~frameRestrict=allowedFrms.inSyntax,
                            ~nodesToCreateParentsFor=floatingNodesToCreateParentsFor,
                        )
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
                        foundParents->Array.push( Assertion({ args, frame, }) )
                    }
                    | Some(err) => {
                        if (debugLevel != 0) {
                            foundParents->Array.push( AssertionWithErr({ args, frame, err }) )
                        }
                    }
                }
            }
        }
    })
    foundParents
}

let proveWithoutJustification = (~tree:proofTree, ~expr:expr, ~allowedFrms:allowedFrms, ~combCntMax:int,):proofNode => {
    let node = tree->ptGetNode(expr)
    if (node->pnGetProof->Belt.Option.isNone) {
        let parents = findAsrtParentsWithNewVars(
            ~tree, 
            ~expr, 
            ~args=tree->ptGetRootStmts->Array.map(stmt => stmt.expr),
            ~exactOrderOfArgs=false,
            ~allowEmptyArgs=false,
            ~allowNewVars=false,
            ~allowNewDisjForExistingVars=false,
            ~allowedFrms,
            ~combCntMax 
        )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent, true, false)
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
        switch tree->ptGetRootStmts->Array.find(stmt => stmt.label == label) {
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
        ->Array.map(getStmtByLabel)
        ->Array.filter(Belt_Option.isSome(_))
        ->Array.map(Belt_Option.getExn(_))
    if (foundStmts->Array.length == jstf.args->Array.length) {
        Some(foundStmts)
    } else {
        None
    }
}

let proveWithJustification = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~jstf: jstf,
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
):proofNode => {
    let findAsrtParents = (args,debugLevel) => {
        findAsrtParentsWithNewVars(
            ~tree,
            ~expr,
            ~args,
            ~exactOrderOfArgs=true,
            ~allowEmptyArgs=false,
            ~allowNewVars=false,
            ~frmsToUse=[jstf.label],
            ~allowNewDisjForExistingVars=false,
            ~allowedFrms,
            ~combCntMax,
            ~debugLevel
        )
    }

    let node = tree->ptGetNode(expr)
    switch getStatementsFromJustification( ~tree, ~jstf, ) {
        | None => ()
        | Some(args) => {
            if (
                node->pnGetProof
                    ->Belt.Option.map(src => !jstfEqSrc(args,jstf.label,src))
                    ->Belt_Option.getWithDefault(true)
            ) {
                findAsrtParents(args,0)->Expln_utils_common.arrForEach(parent => {
                    let requiredJstfIsFound = jstfEqSrc(args,jstf.label,parent)
                    node->pnAddParent(parent, true, requiredJstfIsFound)
                    if (requiredJstfIsFound) {Some(())} else {None}
                })->ignore
                if (node->pnGetProof->Belt.Option.isNone) {
                    findAsrtParents(args,2)->Expln_utils_common.arrForEach(parent => {
                        let requiredJstfIsFound = jstfEqSrc(args,jstf.label,parent)
                        node->pnAddParent(parent, true, requiredJstfIsFound)
                        if (requiredJstfIsFound) {Some(())} else {None}
                    })->ignore
                }
            }
        }
    }
    node
}

let srcIsBackRef = (expr:expr, src:exprSrc):bool => {
    switch src {
        | Assertion({args}) => args->Array.some(arg => expr->exprEq(arg->pnGetExpr))
        | _ => false
    }
}

let updateProverParams = (
    ~node:proofNode, 
    ~dist:int,
    ~proofCtxIntToSymOpt:int=>option<string>,
    ~symToProofCtxIntOpt:string=>option<int>,
):unit => {
    switch node->pnGetProverParams {
        | None => raise(MmException({msg:`Internal error: node.proverParams is None.`}))
        | Some(proverParams) => {
            switch proverParams.bottomUpProverParams {
                | None => ()
                | Some(bottomUpProverParams) => {
                    switch bottomUpProverParams.updateParams {
                        | None => ()
                        | Some(updateParams) => {
                            switch updateParams(
                                proverParams, 
                                node->pnGetExpr,
                                dist,
                                proofCtxIntToSymOpt,
                                symToProofCtxIntOpt
                            ) {
                                | None => ()
                                | Some(newProverParams) => node->pnSetProverParams(Some(newProverParams))
                            }
                        }
                    }
                }
            }
        }
    }
}

let proveBottomUp = (
    ~initProverParams:proverParams,
    ~tree:proofTree,
    ~expr:expr,
    ~getParents:(bottomUpProverParams,expr,int,option<int=>unit>)=>array<exprSrc>,
    ~maxSearchDepth:int,
    ~onProgress:option<string=>unit>,
) => {
    let proofCtxIntToSymOpt:int=>option<string> = i => tree->ptGetProofCtx->ctxIntToSym(i)
    let symToProofCtxIntOpt:string=>option<int> = s => tree->ptGetProofCtx->ctxSymToInt(s)

    let nodesToCreateParentsFor = Belt_MutableQueue.make()

    let maxSearchDepthStr = maxSearchDepth->Belt.Int.toString
    let progressState = ref(progressTrackerMake( ~step=0.01, ~onProgress = _ => () ))

    tree->ptClearDists
    let rootNode = tree->ptGetNode(expr)
    rootNode->pnSetDist(0)
    rootNode->pnSetProverParams(Some(initProverParams))
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
                        progressState.contents = progressTrackerMake(
                            ~step=0.01,
                            ~onProgress= pct => {
                                let pctStr = (pct  *. 100.)->Math.round->Belt.Float.toInt->Belt_Int.toString
                                onProgress(`Proving bottom-up: ${curDistStr}/${maxSearchDepthStr} ${pctStr}%`)
                            }
                        )
                        maxCnt.contents = nodesToCreateParentsFor->Belt_MutableQueue.size + 1
                        cnt.contents = 0
                    }
                    progressState.contents->progressTrackerSetCurrPct(
                        cnt.contents->Belt_Int.toFloat /. maxCnt.contents->Belt_Int.toFloat
                    )
                    cnt.contents = cnt.contents + 1
                }
                | None => ()
            }

            let curExpr = curNode->pnGetExpr
            switch findNonAsrtParent(~tree, ~expr=curExpr) {
                | Some(parent) => curNode->pnAddParent(parent, true, false)
                | None => {
                    let newDist = curDist + 1
                    if (newDist <= maxSearchDepth) {
                        updateProverParams(~node=curNode, ~dist=curDist, ~proofCtxIntToSymOpt, ~symToProofCtxIntOpt, )
                        switch curNode->pnGetBottomUpProverParams {
                            | None => ()
                            | Some(bottomUpProverParams) => {
                                getParents(bottomUpProverParams, curExpr, curDist, onProgressP.contents)->Array.forEach(src => {
                                    if (!srcIsBackRef(curExpr, src)) {
                                        curNode->pnAddParent(src, true, false)
                                        switch src {
                                            | Assertion({args}) => 
                                                args->Array.forEach(arg => {
                                                    if (arg->pnGetDist->Belt.Option.isNone) {
                                                        arg->pnSetDist(newDist)
                                                        arg->pnSetProverParams(curNode->pnGetProverParams)
                                                        if (arg->pnGetProof->Belt.Option.isNone) {
                                                            nodesToCreateParentsFor->Belt_MutableQueue.add(arg)
                                                        }
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
}

let isInCorrectOrder = (min:option<int>, i:int, max:option<int>):bool => {
    switch min {
        | Some(min) => {
            if (min <= i) {
                switch max {
                    | Some(max) => i <= max
                    | None => true
                }
            } else {
                false
            }
        }
        | None => {
            switch max {
                | Some(max) => i <= max
                | None => true
            }
        }
    }
}

let exprMatchesAsrtMatcher = (
    ~expr:expr,
    ~matcher:applyAsrtResultMatcher,
    ~parenCnt:parenCnt,
):bool => {
    if (!matcher.matchAsrt) {
        true
    } else {
        let frm = matcher.frm
        if (expr->Array.getUnsafe(0) != frm.frame.asrt->Array.getUnsafe(0)) {
            false
        } else {
            let res = ref(false)
            iterateSubstitutions(
                ~frmExpr = frm.frame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts->Array.getUnsafe(frm.numOfHypsE), 
                ~constParts = frm.constParts->Array.getUnsafe(frm.numOfHypsE), 
                ~varGroups = frm.varGroups->Array.getUnsafe(frm.numOfHypsE),
                ~subs = frm.subs,
                ~parenCnt,
                ~consumer = _ => {
                    res.contents = true
                    Stop
                }
            )->ignore
            res.contents
        }
    }
}

let exprMatchesAsrtMatchers = (
    ~expr:expr, 
    ~matchers:option<array<applyAsrtResultMatcher>>,
    ~parenCnt:parenCnt,
):bool => {
    switch matchers {
        | None => true
        | Some(matchers) => matchers->Array.some(matcher => exprMatchesAsrtMatcher(~expr, ~matcher, ~parenCnt))
    }
}

let proveStmtBottomUp = (
    ~tree:proofTree, 
    ~expr:expr, 
    ~params:bottomUpProverParams,
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
    ~debugLevel:int,
    ~onProgress:option<string=>unit>,
):proofNode => {
    let getParents = (
        params:bottomUpProverParams, 
        expr:expr, 
        dist:int, 
        onProgress:option<int=>unit>
    ):array<exprSrc> => {
        let res = []
        for i in 0 to params.frameParams->Array.length-1 {
            let paramsI = params.frameParams->Array.getUnsafe(i)
            if (
                isInCorrectOrder(paramsI.minDist, dist, paramsI.maxDist) 
                && exprMatchesAsrtMatchers(~expr, ~matchers=paramsI.matches, ~parenCnt=tree->ptGetParenCnt)
            ) {
                let parents:array<exprSrc> = findAsrtParentsWithNewVars(
                    ~tree,
                    ~expr,
                    ~args=paramsI.deriveFrom,
                    ~exactOrderOfArgs=false,
                    ~frmsToUse=?paramsI.frmsToUse,
                    ~allowEmptyArgs=paramsI.allowNewStmts,
                    ~allowNewVars=paramsI.allowNewVars,
                    ~allowNewDisjForExistingVars=paramsI.allowNewDisjForExistingVars,
                    ~allowedFrms,
                    ~combCntMax,
                    ~debugLevel,
                    ~maxNumberOfResults=?paramsI.maxNumberOfBranches,
                    ~onProgress?
                )
                let parents = switch paramsI.lengthRestrict {
                    | No => parents
                    | LessEq | Less => {
                        let exprLen = expr->Array.length
                        parents->Array.filter(parent => {
                            switch parent {
                                | VarType | Hypothesis(_) | AssertionWithErr(_) => true
                                | Assertion({args, frame}) => {
                                    let argsAreCorrect = ref(true)
                                    let numOfArgs = frame.hyps->Array.length
                                    let maxArgIdx = numOfArgs - 1
                                    let argIdx = ref(0)
                                    while (argIdx.contents <= maxArgIdx && argsAreCorrect.contents) {
                                        let arg = args->Array.getUnsafe(argIdx.contents)
                                        if ((frame.hyps->Array.getUnsafe(argIdx.contents)).typ == E) {
                                            argsAreCorrect.contents = switch paramsI.lengthRestrict {
                                                | No => true
                                                | LessEq => arg->pnGetExpr->Array.length <= exprLen
                                                | Less => arg->pnGetExpr->Array.length < exprLen
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
                let parents = switch paramsI.matches {
                    | None => parents
                    | Some(matchers) => {
                        parents->Array.filter(parent => {
                            matchers->Array.some(matcher => {
                                exprSrcMatches(
                                    ~expr,
                                    ~src=parent,
                                    ~matcher,
                                    ~parenCnt=tree->ptGetParenCnt,
                                )
                            })
                        })
                    }
                }
                res->Array.push(parents)
            }
        }
        res->Belt.Array.concatMany
    }

    proveBottomUp(
        ~initProverParams={customParams:None,bottomUpProverParams:Some(params)},
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
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
    ~debugLevel:int,
    ~onProgress:option<string=>unit>,
) => {
    switch bottomUpProverParams {
        | Some(params) => {
            proveStmtBottomUp( 
                ~tree, ~expr, ~params, ~allowedFrms, ~combCntMax, ~debugLevel, ~onProgress, 
            )->ignore
        }
        | None => {
            switch jstf {
                | None => proveWithoutJustification( ~tree, ~expr, ~allowedFrms, ~combCntMax )->ignore
                | Some(jstf) => proveWithJustification( ~tree, ~expr, ~jstf, ~allowedFrms, ~combCntMax )->ignore
            }
        }
    }
}

let createProofTree = (
    ~proofCtx: mmContext,
    ~frms: frms,
    ~parenCnt: parenCnt,
) => {
    let tree = ptMake( ~proofCtx, ~frms, ~parenCnt, )
    proofCtx->getAllHyps->Belt_MapString.forEach((label,hyp) => {
        if (hyp.typ == E) {
            let node = tree->ptGetNode(hyp.expr)
            node->pnAddParent(Hypothesis({label:label}), true, false)
            tree->ptAddRootStmt({
                isHyp: true,
                label,
                expr: hyp.expr,
                jstf: None,
            })->ignore
        }
    })
    tree
}

let proveFloatings = (
    ~wrkCtx: mmContext,
    ~frms: frms,
    ~frameRestrict:frameRestrict,
    ~floatingsToProve: array<expr>,
    ~parenCnt: parenCnt,
):proofTree => {
    let tree = createProofTree(
        ~proofCtx=wrkCtx,
        ~frms,
        ~parenCnt,
    )
    let floatingNodesToCreateParentsFor = arrayQueueMake(1000)

    floatingsToProve->Array.forEach(expr => {
        proveFloating( 
            ~tree, 
            ~node=tree->ptGetNode(expr), 
            ~frameRestrict, 
            ~nodesToCreateParentsFor=floatingNodesToCreateParentsFor
        )
    })
    tree
}

let proveSyntaxTypes = (
    ~proofTree:option<proofTree>=?,
    ~wrkCtx: option<mmContext>=?,
    ~frms: option<frms>=?,
    ~frameRestrict:frameRestrict,
    ~parenCnt: option<parenCnt>=?,
    ~exprs: array<expr>,
    ~syntaxTypes: array<int>,
    ~onProgress:option<float=>unit>=?
):proofTree => {
    if (
        proofTree->Belt_Option.isNone
        && (wrkCtx->Belt_Option.isNone || frms->Belt_Option.isNone || parenCnt->Belt_Option.isNone)
    ) {
        raise(MmException({msg:`Either proofTree or (wrkCtx and frms and parenCnt) should be passed.`}))
    }

    let progressState = progressTrackerMake( ~step=0.01, ~onProgress? )

    let tree = switch proofTree {
        | Some(tree) => tree
        | None => {
            createProofTree(
                ~proofCtx=wrkCtx->Belt_Option.getExn,
                ~frms=frms->Belt_Option.getExn,
                ~parenCnt=parenCnt->Belt_Option.getExn,
            )
        }
    }
    if (syntaxTypes->Array.length == 0) {
        tree
    } else {
        let floatingNodesToCreateParentsFor = arrayQueueMake(1000)
        let lastType = ref(syntaxTypes->Array.getUnsafe(0))
        for ei in 0 to exprs->Array.length-1 {
            let expr = exprs->Array.getUnsafe(ei)
            let node = ref(tree->ptGetNode([lastType.contents]->Array.concat(expr)))
            proveFloating( 
                ~tree, 
                ~node=node.contents, 
                ~frameRestrict,
                ~nodesToCreateParentsFor=floatingNodesToCreateParentsFor,
            )
            let ti = ref(0)
            while (node.contents->pnGetProof->Belt.Option.isNone && ti.contents < syntaxTypes->Array.length ) {
                let typ = syntaxTypes->Array.getUnsafe(ti.contents)
                ti := ti.contents + 1
                if (typ != lastType.contents) {
                    node := tree->ptGetNode([typ]->Array.concat(expr))
                    proveFloating( 
                        ~tree, 
                        ~node=node.contents, 
                        ~frameRestrict,
                        ~nodesToCreateParentsFor=floatingNodesToCreateParentsFor,
                    )
                }
            }
            switch node.contents->pnGetProof {
                | None => ()
                | Some(_) => {
                    tree->ptAddSyntaxProof(expr, node.contents)
                    lastType := (node.contents->pnGetExpr)->Array.getUnsafe(0)
                }
            }
            progressState->progressTrackerSetCurrPct( 
                (ei+1)->Belt_Int.toFloat /. exprs->Array.length->Belt_Int.toFloat
            )
        }

        tree
    }
}

let createProofCtx = (wrkCtx:mmContext, rootStmts:array<rootStmt>):mmContext => {
    let proofCtx = createContext(~parent=wrkCtx)
    rootStmts->Array.forEach(stmt => {
        if (stmt.isHyp) {
            proofCtx->applySingleStmt(Essential({label:stmt.label, expr:wrkCtx->ctxIntsToSymsExn(stmt.expr)}))
        }
    })
    proofCtx
}

let unifyAll = (
    ~wrkCtx: mmContext,
    ~frms: frms,
    ~rootStmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~bottomUpProverParams:option<bottomUpProverParams>=?,
    ~allowedFrms:allowedFrms,
    ~combCntMax:int,
    ~syntaxTypes:option<array<int>>=?,
    ~exprsToSyntaxCheck:option<array<expr>>=?,
    ~debugLevel:int=0,
    ~onProgress:option<string=>unit>=?
):proofTree => {
    let progressState = progressTrackerMake(
        ~step=0.01, 
        ~onProgress=?onProgress->Belt.Option.map(onProgress => {
            pct => onProgress(`Unifying all: ${pct->floatToPctStr}`)
        })
    )

    let proofCtx = createProofCtx(wrkCtx, rootStmts)

    let tree = createProofTree(
        ~proofCtx,
        ~frms,
        ~parenCnt,
    )

    switch syntaxTypes {
        | None => ()
        | Some(syntaxTypes) => {
            if (syntaxTypes->Array.length > 0) {
                switch exprsToSyntaxCheck {
                    | None => ()
                    | Some(exprsToSyntaxCheck) => {
                        proveSyntaxTypes(
                            ~proofTree=tree,
                            ~syntaxTypes,
                            ~exprs=exprsToSyntaxCheck,
                            ~frameRestrict = allowedFrms.inSyntax,
                            ~onProgress = ?onProgress->Belt.Option.map(onProgress => {
                                pct => onProgress(`Checking syntax: ${pct->floatToPctStr}`)
                            })
                        )->ignore
                    }
                }
            }
        }
    }

    let rootProvables = rootStmts->Array.filter(stmt => !stmt.isHyp)
    let numOfStmts = rootProvables->Array.length
    let maxStmtIdx = numOfStmts - 1
    rootProvables->Array.forEachWithIndex((stmt,stmtIdx) => {
        proveStmt(
            ~tree, 
            ~expr=stmt.expr, 
            ~jstf=stmt.jstf,
            ~bottomUpProverParams = if (stmtIdx == maxStmtIdx) {bottomUpProverParams} else {None},
            ~allowedFrms,
            ~combCntMax,
            ~debugLevel,
            ~onProgress =
                if (stmtIdx != maxStmtIdx || bottomUpProverParams->Belt.Option.isNone) {
                    None
                } else {
                    onProgress
                },
        )
        tree->ptAddRootStmt(stmt)

        progressState->progressTrackerSetCurrPct( 
            (stmtIdx+1)->Belt_Int.toFloat /. numOfStmts->Belt_Int.toFloat
        )
    })

    tree
}

let getOrderedSubarray = (arr:array<string>, start:int, end:int):array<string> => {
    arr->Array.slice(~start, ~end=end)->Js.Array2.sortInPlace
}

let compareUnorderedSubArrays = (arr1:array<string>, arr2:array<string>, start:int, end:int):bool => {
    arr1->getOrderedSubarray(start, end) == arr2->getOrderedSubarray(start, end)
}

let makeParenCnt = (
    ~ctx:mmContext,
    ~parens:string,
):parenCnt => {
    let {
        allConsts,
        parenMin,
        canBeFirstMin,
        canBeFirstMax,
        canBeLastMin,
        canBeLastMax,
    } = ctx->ctxGetOptimizedConstsOrder(~parens)
    let allConstsActual = ctx->getAllConsts
    if (allConsts->Array.length != allConstsActual->Array.length) {
        raise(MmException({msg:`allConsts.length != allConstsActual.length`}))
    }
    if (parenMin < 0) {
        // let allConstsStr = allConsts->Array.slice(~start=0, ~end=-parenMin)->Array.joinUnsafe(" ")
        // Console.log2(`allConsts:`, allConstsStr)
        if (
            allConsts->Array.slice(~start=0, ~end=-parenMin)
            != 
            allConstsActual->Array.slice(~start=0, ~end=-parenMin)
        ) {
            raise(MmException({msg:`allConsts.parens != allConstsActual.parens`}))
        }
    }
    if (canBeFirstMin <= canBeFirstMax) {
        // let canBeFirstStr = allConsts->Array.slice(~start=(-canBeFirstMax-1), ~end=-canBeFirstMin)->Array.joinUnsafe(" ")
        // Console.log2(`canBeFirst:`, canBeFirstStr)
        if (!compareUnorderedSubArrays(allConsts, allConstsActual, (-canBeFirstMax-1), -canBeFirstMin)) {
            raise(MmException({msg:`allConsts.canBeFirst != allConstsActual.canBeFirst`}))
        }
    }
    if (canBeLastMin <= canBeLastMax) {
        // let canBeLastStr = allConsts->Array.slice(~start=(-canBeLastMax-1), ~end=-canBeLastMin)->Array.joinUnsafe(" ")
        // Console.log2(`canBeLast:`, canBeLastStr)
        if (!compareUnorderedSubArrays(allConsts, allConstsActual, (-canBeLastMax-1), -canBeLastMin)) {
            raise(MmException({msg:`allConsts.canBeLast != allConstsActual.canBeLast`}))
        }
    }
    if (!compareUnorderedSubArrays(allConsts, allConstsActual, 0, allConsts->Array.length)) {
        raise(MmException({msg:`allConsts.remaining != allConstsActual.remaining`}))
    }

    parenCntMake(~parenMin, ~canBeFirstMin, ~canBeFirstMax, ~canBeLastMin, ~canBeLastMax, )
}