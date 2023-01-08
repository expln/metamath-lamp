open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_proof_table
open MM_progress_tracker

type justification = {
    args: array<string>,
    asrt: string
}

type rootStmt = {
    label: option<string>,
    expr: expr,
    justification: option<justification>,
}

type rec proofTreeNode = {
    expr:expr,
    exprStr:option<string>,//debug
    label: option<string>,
    mutable parents: option<array<exprSource>>,
    mutable children: array<proofTreeNode>,
    mutable proof: option<exprSource>,
}
and exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({
        args:array<proofTreeNode>, 
        label:string, 
        subs: option<array<array<string>>>,//debug
        comb: array<int>,//debug
    })

type proofTree = {
    frms: Belt_MapString.t<frmSubsData>,
    hypsByExpr: Belt_Map.t<expr,hypothesis,ExprCmp.identity>,
    hypsByLabel: Belt_MapString.t<hypothesis>,
    ctxMaxVar:int,
    mutable maxVar:int,
    newVars: Belt_MutableSet.t<expr,ExprCmp.identity>,
    disj: disjMutable,
    parenCnt:parenCnt,
    rootNodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
    nodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofTreeNode>,
}

type stackOfNodesToCreateParentsFor = {
    stackF: Belt_MutableStack.t<proofTreeNode>,
    stackE: Belt_MutableStack.t<Belt_MutableStack.t<proofTreeNode>>,
    set: Belt_MutableSet.t<expr,ExprCmp.identity>,
}

let stackOfNodesToCreateParentsForHas = (stack:stackOfNodesToCreateParentsFor,node:proofTreeNode) => {
    stack.set->Belt_MutableSet.has(node.expr)
}

let stackOfNodesToCreateParentsForMake = () => {
    {
        stackF: Belt_MutableStack.make(),
        stackE: Belt_MutableStack.make(),
        set: Belt_MutableSet.make(~id=module(ExprCmp))
    }
}

let stackOfNodesToCreateParentsForAddAll = (
    stack:stackOfNodesToCreateParentsFor,
    ~fArgs:array<proofTreeNode>,
    ~eArgs:array<proofTreeNode>,
) => {
    fArgs->Js_array2.forEach(arg => {
        if (!(stack.set->Belt_MutableSet.has(arg.expr))) {
            stack.set->Belt_MutableSet.add(arg.expr)
            stack.stackF->Belt_MutableStack.push(arg)
        }
    })
    let essentials = Belt_MutableStack.make()
    eArgs->Js_array2.forEach(arg => {
        if (!(stack.set->Belt_MutableSet.has(arg.expr))) {
            stack.set->Belt_MutableSet.add(arg.expr)
            essentials->Belt_MutableStack.push(arg)
        }
    })
    if (!(essentials->Belt_MutableStack.isEmpty)) {
        stack.stackE->Belt_MutableStack.push(essentials)
    }
}

let stackOfNodesToCreateParentsForAddE = (stack:stackOfNodesToCreateParentsFor, node:proofTreeNode) => {
    if (!(stack.set->Belt_MutableSet.has(node.expr))) {
        stack.set->Belt_MutableSet.add(node.expr)
        let essentials = Belt_MutableStack.make()
        essentials->Belt_MutableStack.push(node)
        stack.stackE->Belt_MutableStack.push(essentials)
    }
}

let stackOfNodesToCreateParentsForDepth = (stack:stackOfNodesToCreateParentsFor) => {
    stack.stackE->Belt_MutableStack.size
}

let stackOfNodesToCreateParentsForIsEmty = (stack:stackOfNodesToCreateParentsFor) => {
    stack.stackF->Belt_MutableStack.isEmpty && stack.stackE->Belt_MutableStack.isEmpty
}

let stackOfNodesToCreateParentsForGetNextNode = (stack:stackOfNodesToCreateParentsFor) => {
    let isTopEmpty = stack => {
        switch stack->Belt_MutableStack.top {
            | None => false
            | Some(top) => top->Belt_MutableStack.isEmpty
        }
    }
    if (stack.stackF->Belt_MutableStack.isEmpty) {
        while (stack.stackE->isTopEmpty) {
            stack.stackE->Belt_MutableStack.pop->ignore
        }
        switch stack.stackE->Belt_MutableStack.top {
            | None => None
            | Some(top) => {
                let res = top->Belt_MutableStack.pop
                while (stack.stackE->isTopEmpty) {
                    stack.stackE->Belt_MutableStack.pop->ignore
                }
                res
            }
        }
    } else {
        stack.stackF->Belt_MutableStack.pop
    }
}

let createEmptyProofTree = (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
    ~parenCnt:parenCnt,
) => {
    {
        frms,
        hypsByLabel: hyps,
        hypsByExpr: hyps->Belt_MapString.toArray->Js_array2.map(((_,hyp)) => (hyp.expr, hyp))->Belt_Map.fromArray(~id=module(ExprCmp)),
        ctxMaxVar:maxVar,
        maxVar,
        newVars: Belt_MutableSet.make(~id=module(ExprCmp)),
        disj,
        parenCnt,
        rootNodes: Belt_MutableMap.make(~id=module(ExprCmp)),
        nodes: Belt_MutableMap.make(~id=module(ExprCmp)),
    }
}

let exprSourceEq = (s1,s2) => {
    switch s1 {
        | VarType => {
            switch s2 {
                | VarType => true
                | _ => false
            }
        }
        | Hypothesis({label:label1}) => {
            switch s2 {
                | Hypothesis({label:label2}) => label1 == label2
                | _ => false
            }
        }
        | Assertion({ args:args1, label:label1, }) => {
            switch s2 {
                | Assertion({ args:args2, label:label2, }) => {
                    label1 == label2
                    && args1->Js.Array2.length == args2->Js.Array2.length
                    && args1->Js.Array2.everyi((arg1,idx) => exprEq(arg1.expr, args2[idx].expr))
                }
                | _ => false
            }
        }
    }
}

let createOrUpdateNode = (
    ~tree:proofTree, 
    ~label:option<string>, 
    ~expr:expr, 
    ~child:option<proofTreeNode>,
    ~exprToStr:option<expr=>string>,
):proofTreeNode => {
    let result = switch tree.nodes->Belt_MutableMap.get(expr) {
        | Some(node) => node
        | None => {
            let node = {
                label,
                expr,
                exprStr:
                    switch exprToStr {
                        | None => None
                        | Some(exprToStr) => Some(exprToStr(expr))
                    },
                parents: None,
                proof: None,
                children: [],
            }
            tree.nodes->Belt_MutableMap.set(expr, node)->ignore
            node
        }
    }
    switch child {
        | Some(child) => {
            if (!exprEq(expr,child.expr)) {
                switch result.children->Js.Array2.find(existingChild => exprEq(existingChild.expr,child.expr)) {
                    | None => result.children->Js_array2.push(child)->ignore
                    | Some(_) => ()
                }
            }
        }
        | None => ()
    }
    result
}

let markProved = ( node:proofTreeNode ) => {
    let getProofFromParents = (node):option<exprSource> => {
        switch node.parents {
            | None => None
            | Some(parents) => {
                parents->Expln_utils_common.arrForEach(parent => {
                    switch parent {
                        | VarType | Hypothesis(_) => Some(parent)
                        | Assertion({args}) => {
                            if (args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)) {
                                Some(parent)
                            } else {
                                None
                            }
                        }
                    }
                })
            }
        }
    }

    switch getProofFromParents(node) {
        | None => ()
        | Some(nodeProof) => {
            node.proof = Some(nodeProof)
            let nodesToMarkProved = node.children->Belt_MutableQueue.fromArray
            while (nodesToMarkProved->Belt_MutableQueue.size > 0) {
                let curNode = nodesToMarkProved->Belt_MutableQueue.pop->Belt_Option.getExn
                if (curNode.proof->Belt_Option.isNone) {
                    switch getProofFromParents(curNode) {
                        | None => ()
                        | Some(curNodeProof) => {
                            curNode.proof = Some(curNodeProof)
                            curNode.children->Js_array2.forEach( nodesToMarkProved->Belt_MutableQueue.add )
                        }
                    }
                }
            }
        }
    }
}

let getStatementsFromJustification = (
    ~tree:proofTree,
    ~stmts:array<rootStmt>,
    ~justification: justification,
):array<expr> => {
    let byLabel = label => {
        (stmt:rootStmt):bool => {
            switch stmt.label {
                | None => false
                | Some(stmtLabel) => stmtLabel == label
            }
        }
    }

    let getStmtByLabel = label => {
        switch stmts->Js.Array2.find(byLabel(label)) {
            | Some(stmt) => Some(stmt.expr)
            | None => {
                switch tree.hypsByLabel->Belt_MapString.get(label) {
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

let printSubs = (~frame,~subs,~exprToStr):array<array<string>> => {
    let getSubExprForFrmVar = frmVar => {
        let subExpr = subs.exprs[frmVar]
        let minIdx = subs.begins[frmVar]
        let maxIdx = subs.ends[frmVar]
        subExpr->Js_array2.filteri((_,idx) => minIdx <= idx && idx <= maxIdx)
    }
    let lines = []
    for frmVar in 0 to subs.size-1 {
        lines->Js_array2.push([
            frame.frameVarToSymb->Belt_MapInt.get(frmVar)->Belt_Option.getWithDefault("?????"),
            getSubExprForFrmVar(frmVar)->exprToStr
        ])->ignore
    }
    lines
}

let addParentsWithoutNewVars = (
    ~tree, 
    ~node, 
    ~stackOfNodesToCreateParentsFor:stackOfNodesToCreateParentsFor,
    ~exprToStr
):unit => {
    if (stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForDepth < 5) {
        let expr = node.expr
        let exprLen = expr->Js_array2.length
        tree.frms->Belt_MapString.forEach((_,frm) => {
            if (node.proof->Belt.Option.isNone) {
                let frmExpr = frm.frame.asrt
                if (frmExpr[0] == expr[0]) {
                    iterateSubstitutions(
                        ~frmExpr,
                        ~expr,
                        ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                        ~constParts = frm.constParts[frm.numOfHypsE], 
                        ~varGroups = frm.varGroups[frm.numOfHypsE],
                        ~subs = frm.subs,
                        ~parenCnt=tree.parenCnt,
                        ~consumer = subs => {
                            if (subs.isDefined->Js_array2.every(b=>b)
                                && verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx=tree.disj->disjContains)) {
                                let args = frm.frame.hyps->Js_array2.map(hyp => {
                                    let newExprToProve = applySubs(
                                        ~frmExpr = hyp.expr, 
                                        ~subs,
                                        ~createWorkVar = 
                                            _ => raise(MmException({msg:`Work variables are not supported in addParentsWithoutNewVars().`}))
                                    )
                                    let arg = switch tree.rootNodes->Belt_MutableMap.get(newExprToProve) {
                                        | Some(existingNode) => existingNode
                                        | None => {
                                            createOrUpdateNode(
                                                ~tree,
                                                ~label=None,
                                                ~expr = newExprToProve,
                                                ~child=Some(node),
                                                ~exprToStr,
                                            )
                                        }
                                    }
                                    arg
                                })
                                stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForAddAll(
                                    ~fArgs=args->Js_array2.filteri((arg,i) => {
                                        frm.frame.hyps[i].typ == F
                                            && arg.parents->Belt.Option.isNone
                                            && arg.expr->Js_array2.length <= exprLen
                                    }),
                                    ~eArgs=args->Js_array2.filteri((arg,i) => {
                                        frm.frame.hyps[i].typ == E
                                            && arg.parents->Belt.Option.isNone
                                            && arg.expr->Js_array2.length <= exprLen
                                    })
                                )
                                let parent = Assertion({ args, label:frm.frame.label, comb: [], subs: None })
                                node.parents->Belt.Option.getExn->Js_array2.push(parent)->ignore
                                markProved(node)
                            }
                            if (node.proof->Belt.Option.isNone) {
                                Continue
                            } else {
                                Stop
                            }
                        }
                    )->ignore
                }
            }
        })
    }
}

let collectParentNodesWithoutNewVars = (
    ~tree,
    ~parents:array<exprSource>,
    ~parentNodesWithoutNewVars:Belt_MutableQueue.t<proofTreeNode>
) => {
    parents->Js.Array2.forEach(parent => {
        switch parent {
            | Assertion({args, label}) => {
                args->Js_array2.forEach(arg => {
                    if (arg.expr->Js_array2.every(s => s <= tree.ctxMaxVar)) {
                        parentNodesWithoutNewVars->Belt_MutableQueue.add(arg)
                    }
                })
            }
            | _ => ()
        }
    })
}

let getUnprovedParentNodesWithoutNewVars = (
    ~tree,
    ~parents:array<exprSource>,
):array<proofTreeNode> => {
    let res = Belt_MutableMap.make(~id=module(ExprCmp))
    parents->Js.Array2.forEach(parent => {
        switch parent {
            | Assertion({args, label}) => {
                args->Js_array2.forEach(arg => {
                    if (
                            arg.proof->Belt.Option.isNone
                            && !(res->Belt_MutableMap.has(arg.expr))
                            && arg.expr->Js_array2.every(s => s <= tree.ctxMaxVar) 
                    ) {
                        res->Belt_MutableMap.set(arg.expr, arg)
                    }
                })
            }
            | _ => ()
        }
    })
    res->Belt_MutableMap.valuesToArray
}

let rec addParentsWithNewVars = (~tree, ~stmts:array<rootStmt>, ~node, ~justification, ~bottomUp, ~exprToStr):unit => {
    let applResults = []
    switch justification {
        | None => {
            let prevRootExprs = stmts->Js.Array2.map(({expr}) => expr)
            applyAssertions(
                ~maxVar = tree.maxVar,
                ~frms = tree.frms,
                ~isDisjInCtx = tree.disj->disjContains,
                ~statements = prevRootExprs,
                ~allowEmptyArgs=bottomUp,
                ~result = node.expr,
                ~parenCnt=tree.parenCnt,
                ~onMatchFound = res => {
                    applResults->Js_array2.push(res)->ignore
                    Continue
                },
                ()
            )
        }
        | Some(justification) => {
            applyAssertions(
                ~maxVar = tree.maxVar,
                ~frms = tree.frms,
                ~isDisjInCtx = tree.disj->disjContains,
                ~statements = getStatementsFromJustification( ~tree, ~stmts, ~justification ),
                ~exactOrderOfStmts=true,
                ~allowEmptyArgs=false,
                ~result = node.expr,
                ~parenCnt=tree.parenCnt,
                ~frameFilter = frame => frame.label == justification.asrt,
                ~onMatchFound = res => {
                    applResults->Js_array2.push(res)->ignore
                    Continue
                },
                ()
            )
        }
    }
    applResults->Expln_utils_common.arrForEach(applResult => {
        let applNewVarToTreeNewVar = Belt_MutableMapInt.make()
        applResult.newVars->Js.Array2.forEachi((v,i) => {
            tree.maxVar = tree.maxVar + 1
            let newVar = tree.maxVar
            applNewVarToTreeNewVar->Belt_MutableMapInt.set(v,newVar)
            let newVarType = applResult.newVarTypes[i]
            tree.newVars->Belt_MutableSet.add([newVarType, newVar])
        })
        applResult.newDisj->disjForEach((n,m) => {
            tree.disj->disjAddPair(
                applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(n, n),
                applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(m, m),
            )
        })
        let frame = switch tree.frms->Belt_MapString.get(applResult.asrtLabel) {
            | None => 
                raise(MmException({msg:`Cannot find an assertion with label ${applResult.asrtLabel} in addParentsWithNewVars.`}))
            | Some(frm) => frm.frame
        }
        let args = frame.hyps->Js_array2.map(hyp => {
            let argExpr = applySubs(
                ~frmExpr = hyp.expr, 
                ~subs=applResult.subs,
                ~createWorkVar = _ => raise(MmException({msg:`New work variables are not expected here.`}))
            )
            switch tree.rootNodes->Belt_MutableMap.get(argExpr) {
                | Some(existingNode) => existingNode
                | None => {
                    createOrUpdateNode(
                        ~tree,
                        ~label=None,
                        ~expr = argExpr,
                        ~child=Some(node),
                        ~exprToStr,
                    )
                }
            }
        })
        let newParent = Assertion({
            args,
            label: applResult.asrtLabel,
            comb: applResult.comb,
            subs:
                switch exprToStr {
                    | None => None
                    | Some(exprToStr) => Some(printSubs(~frame,~subs=applResult.subs,~exprToStr))
                }
        })
        if (node.parents->Belt.Option.getExn->Js_array2.find(par => exprSourceEq(par, newParent))->Belt_Option.isNone) {
            if (checkTypes(~tree, ~frame, ~args, ~exprToStr)) {
                node.parents->Belt_Option.getExn->Js_array2.push(newParent)->ignore
                markProved(node)
            }
        }
        if (node.proof->Belt.Option.isSome) {
            Some(true)
        } else {
            None
        }
    })->ignore
}

and let proveNode = (
    ~tree:proofTree,
    ~stmts:array<rootStmt>,
    ~node:proofTreeNode,
    ~justification: option<justification>,
    ~syntaxProof:bool,
    ~bottomUp:bool,
    ~exprToStr:option<expr=>string>,
) => {
    if (syntaxProof) {
        let rootNode = node
        let stackOfNodesToCreateParentsFor = stackOfNodesToCreateParentsForMake()
        stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForAddE(node)
        let maxStackSize = ref(stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForDepth)
        while (rootNode.proof->Belt_Option.isNone && !(stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForIsEmty)) {
            let curNode = stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForGetNextNode->Belt_Option.getExn
            switch curNode.parents {
                | Some(_) => ()
                | None => {
                    if (tree.newVars->Belt_MutableSet.has(curNode.expr)) {
                        curNode.parents = Some([VarType])
                        markProved(curNode)
                    } else {
                        switch tree.hypsByExpr->Belt_Map.get(curNode.expr) {
                            | Some(hyp) => {
                                curNode.parents = Some([Hypothesis({label:hyp.label})])
                                markProved(curNode)
                            }
                            | None => {
                                curNode.parents = Some([])
                                addParentsWithoutNewVars(~tree, ~node=curNode, ~stackOfNodesToCreateParentsFor, ~exprToStr)
                            }
                        }
                    }
                }
            }
            if (bottomUp) {
                let size = stackOfNodesToCreateParentsFor->stackOfNodesToCreateParentsForDepth
                if (maxStackSize.contents < size) {
                    maxStackSize.contents = size
                    Js.Console.log2("maxStackSize", maxStackSize.contents)
                }
                if (size > 10) {
                    Js.Console.log2("size2", size)
                }
            }
        }
    } else if (!bottomUp) {
        node.parents = Some([])
        addParentsWithNewVars(~tree, ~stmts, ~node, ~justification, ~bottomUp=false, ~exprToStr)
    } else {
        node.parents = Some([])
        let rootNode = node
        addParentsWithNewVars(~tree, ~stmts, ~node=rootNode, ~justification, ~bottomUp=true, ~exprToStr)
        if (rootNode.proof->Belt.Option.isNone) {
            let cnt = ref(0)
            getUnprovedParentNodesWithoutNewVars(~tree, ~parents=rootNode.parents->Belt.Option.getExn)
                ->Expln_utils_common.arrForEach(parentNode => {
                    if (parentNode.proof->Belt.Option.isNone) {
                        cnt.contents = cnt.contents + 1
                        Js.Console.log2("cnt", cnt.contents)
                    }
                    // if (cnt.contents > 5) {
                    //     Some(())
                    // } else {
                        proveNode(
                            ~tree,
                            ~stmts,
                            ~node=parentNode,
                            ~justification=None,
                            ~syntaxProof=true,
                            ~bottomUp=true,
                            ~exprToStr,
                        )
                        rootNode.proof
                    //     None
                    // }
                })->ignore
            // let parentNodesWithoutNewVars = Belt_MutableQueue.make()
            // collectParentNodesWithoutNewVars( 
            //     ~tree, 
            //     ~parents=rootNode.parents->Belt.Option.getExn, 
            //     ~parentNodesWithoutNewVars
            // )
            // while (rootNode.proof->Belt.Option.isNone && parentNodesWithoutNewVars->Belt_MutableQueue.size > 0) {
            //     Js.Console.log2("Queue.size", parentNodesWithoutNewVars->Belt_MutableQueue.size)
            //     let currNode = parentNodesWithoutNewVars->Belt_MutableQueue.pop->Belt.Option.getExn
            //     currNode.parents = Some([])
            //     addParentsWithNewVars(~tree, ~stmts, ~node=currNode, ~justification=None, ~bottomUp=false, ~exprToStr)
            //     if (rootNode.proof->Belt.Option.isNone && currNode.proof->Belt.Option.isNone) {
            //         addParentsWithNewVars(~tree, ~stmts, ~node=currNode, ~justification=None, ~bottomUp=true, ~exprToStr)
            //         collectParentNodesWithoutNewVars( 
            //             ~tree, 
            //             ~parents=currNode.parents->Belt.Option.getExn,
            //             ~parentNodesWithoutNewVars 
            //         )
            //     }
            // }
        }
    }
}
and let checkTypes = (
    ~tree:proofTree,
    ~frame:frame,
    ~args:array<proofTreeNode>,
    ~exprToStr:option<expr=>string>,
):bool => {
    let nodesToTypecheck = []
    frame.hyps->Js.Array2.forEachi((hyp,hypIdx) => {
        if (hyp.typ == F) {
            nodesToTypecheck->Js.Array2.push(args[hypIdx])->ignore
        }
    })
    nodesToTypecheck->Js.Array2.every(node => {
        proveNode(
            ~tree,
            ~stmts = [],
            ~node,
            ~justification=None,
            ~syntaxProof=true,
            ~bottomUp=false,
            ~exprToStr,
        )
        node.proof->Belt_Option.isSome
    })
}

let proofTreeProve = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~bottomUp: bool=false,
    ~syntaxProof:bool,
    ~parenCnt: parenCnt,
    ~onProgress:option<float=>unit>=?,
    ~debug: bool=false,
    ()
):proofTree => {
    let stmtsProcessed = ref(0.)
    let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))

    let maxVar = ctx->getNumOfVars - 1
    let disj = ctx->getAllDisj
    let hyps = ctx->getAllHyps
    let tree = createEmptyProofTree(~frms, ~maxVar, ~disj, ~hyps, ~parenCnt, )
    let exprToStr = if (debug) {
        let maxCtxVar = ctx->getNumOfVars - 1
        let intToSym = i => {
            if (i <= maxCtxVar) {
                ctx->ctxIntToSymExn(i)
            } else {
                "&" ++ i->Belt.Int.toString
            }
        }
        Some(expr => expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" "))
    } else {
        None
    }
    ctx->getLocalHyps->Js_array2.forEach(hyp => {
        let rootNode = createOrUpdateNode(
            ~tree, 
            ~label=Some(hyp.label), 
            ~expr=hyp.expr, 
            ~child=None,
            ~exprToStr,
        )
        rootNode.proof = Some(Hypothesis({label:hyp.label}))
        tree.rootNodes->Belt_MutableMap.set(rootNode.expr, rootNode)
    })
    let numOfStmts = stmts->Js_array2.length
    let maxStmtIdx = numOfStmts - 1
    for stmtIdx in 0 to maxStmtIdx {
        tree.nodes->Belt_MutableMap.clear
        let rootNode = createOrUpdateNode(
            ~tree, 
            ~label=stmts[stmtIdx].label, 
            ~expr=stmts[stmtIdx].expr, 
            ~child=None,
            ~exprToStr,
        )
        tree.rootNodes->Belt_MutableMap.set(rootNode.expr, rootNode)
        proveNode(
            ~tree,
            ~stmts=tree.rootNodes->Belt_MutableMap.valuesToArray->Js_array2.map(node => {
                {
                    label: node.label,
                    expr: node.expr,
                    justification: None
                }
            }),
            ~node=rootNode,
            ~justification=stmts[stmtIdx].justification,
            ~syntaxProof,
            ~bottomUp = if (stmtIdx < maxStmtIdx) {false} else {bottomUp},
            ~exprToStr
        )

        stmtsProcessed.contents = stmtsProcessed.contents +. 1.
        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
            stmtsProcessed.contents /. numOfStmts->Belt_Int.toFloat
        )
    }
    tree
}

let proofTreeCreateProofTable = (node:proofTreeNode):proofTable => {
    let processedExprs = Belt_MutableSet.make(~id = module(ExprCmp))
    let exprToIdx = Belt_MutableMap.make(~id = module(ExprCmp))
    let tbl = []
    Expln_utils_data.traverseTree(
        (),
        node,
        (_,n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [1].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [1].`}))
                | Some(Hypothesis(_)) => None
                | Some(Assertion({args})) => {
                    if (processedExprs->Belt_MutableSet.has(n.expr)) {
                        None
                    } else {
                        Some(args)
                    }
                }
            }
        },
        ~process = (_, n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [2].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [2].`}))
                | Some(Hypothesis({label})) => {
                    if (exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({proof:Hypothesis({label:label}), expr:n.expr})-1
                        exprToIdx->Belt_MutableMap.set(n.expr,idx)
                    }
                }
                | Some(Assertion(_)) => ()
            }
            None
        },
        ~postProcess = (_, n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [3].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [3].`}))
                | Some(Hypothesis(_)) => ()
                | Some(Assertion({args,label})) => {
                    if (exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({
                            proof:Assertion({
                                label:label,
                                args: args->Js_array2.map(n => {
                                    exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.getWithDefault(-1)
                                })
                            }),
                            expr:n.expr
                        })-1
                        exprToIdx->Belt_MutableMap.set(n.expr,idx)
                    }
                }
            }
            None
        },
        ()
    )->ignore
    tbl
}