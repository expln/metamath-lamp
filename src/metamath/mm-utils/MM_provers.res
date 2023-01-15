open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree2

let findParentsWithoutNewVars = ( ~tree, ~expr, ):array<exprSource> => {
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
                        && verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx=tree->ptIsDisj)) {
                        let args = frm.frame.hyps->Js_array2.map(hyp => {
                            let newExpr = applySubs(
                                ~frmExpr = hyp.expr, 
                                ~subs,
                                ~createWorkVar = _ => raise(MmException({
                                    msg:`Work variables are not supported in addParentsWithoutNewVars().`
                                }))
                            )
                            tree->ptGetOrCreateNode(newExpr)
                        })
                        foundParents->Js_array2.push( Assertion({ args, label:frm.frame.label }) )->ignore
                    }
                    Continue
                }
            )->ignore
        }
    })
    foundParents
}

let proveFloating = (tree, node) => {
    /*
    If a node has a proof, no need to prove it again.
    If a node has Some(parents), it could appear as a side effect of proving other nodes and it was left unproved 
        because the root node became proved first.
    It follows from the statement above, that all unproved nodes have to have most complete collection of parents, 
        otherwise there is a risk to miss existing proof for this node. So we must not interrupt the process of adding 
        parents even if the root node becomes proved.
    */
    if (node->pnGetProof->Belt.Option.isNone) {
        let nodesToCreateParentsFor = Belt_MutableStack.make()
        let savedNodes = Belt_MutableSet.make(~id=module(ExprCmp))

        let saveNodeToCreateParentsFor = node => {
            switch node->pnGetProof {
                | Some(_) => ()
                | None => {
                    if (!(savedNodes->Belt_MutableSet.has(node->pnGetExpr))) {
                        savedNodes->Belt_MutableSet.add(node->pnGetExpr)
                        nodesToCreateParentsFor->Belt_MutableStack.push(node)
                    }
                }
            }
        }

        let rootNode = node
        saveNodeToCreateParentsFor(rootNode)
        while (rootNode->pnGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableStack.isEmpty)) {
            let curNode = nodesToCreateParentsFor->Belt_MutableStack.pop->Belt_Option.getExn
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
                                    findParentsWithoutNewVars(~tree, ~expr=curNode->pnGetExpr)
                                        ->Js.Array2.forEach(asrtParent => {
                                            curNode->pnAddParent(asrtParent)
                                            switch asrtParent {
                                                | Assertion({args}) => 
                                                    args->Js_array2.forEach(saveNodeToCreateParentsFor)
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
    }
}

let findParentsWithNewVars = (
    ~tree,
    ~expr:expr,
    ~stmts:array<expr>,
    ~exactOrderOfStmts:bool=false,
    ~asrtLabel:option<string>=?,
    ~bottomUp:bool=false,
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
        ~allowEmptyArgs=bottomUp,
        ~result = expr,
        ~frameFilter = 
            asrtLabel
                ->Belt_Option.map(asrtLabel => (frame:frame) => frame.label == asrtLabel)
                ->Belt_Option.getWithDefault(_=>true),
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
            let argNode = tree->ptGetOrCreateNode(argExpr)
            args[argIdx.contents] = argNode
            proveFloating(tree, argNode)
            typesAreCorrect.contents = argNode->pnGetProof->Belt.Option.isNone
            argIdx.contents = argIdx.contents + 1
        }
        if (typesAreCorrect.contents) {
            foundParents->Js.Array2.push( Assertion({ args, label: applResult.asrtLabel, }) )->ignore
        }
    })
    foundParents
}

let proveWithoutJustification = (~tree, ~prevStmts:array<expr>, ~stmt:expr):proofNode => {
    let node = tree->ptGetOrCreateNode(stmt)
    if (node->pnGetProof->Belt.Option.isNone && node->pnGetParents->Belt.Option.isNone) {
        let parents = findParentsWithNewVars( ~tree, ~expr=stmt, ~stmts=prevStmts, () )
        parents->Expln_utils_common.arrForEach(parent => {
            node->pnAddParent(parent)
            node->pnGetProof
        })->ignore
    }
    node
}

let proveWithJustification = (~tree, ~args:array<expr>, ~asrtLabel:string, ~stmt:expr):proofNode => {
    let node = tree->ptGetOrCreateNode(stmt)
    if (node->pnGetProof->Belt.Option.isNone && node->pnGetParents->Belt.Option.isNone) {
        let parents = findParentsWithNewVars( 
            ~tree, 
            ~expr=stmt, 
            ~stmts=args, 
            ~asrtLabel,
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

let proveStmt = (~tree, ~prevStmts:array<rootStmt>, ~stmt:rootStmt, ~jstf:option<justification>) => {
    switch jstf {
        | None => {
            proveWithoutJustification(
                ~tree, 
                ~prevStmts=prevStmts->Js_array2.map(stmt => stmt.expr),
                ~stmt=stmt.expr,
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
            )->ignore
        }
    }
}