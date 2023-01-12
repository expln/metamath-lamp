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
                            switch tree->ptGetNodeByExpr(newExpr) {
                                | Some(existingNode) => existingNode
                                | None => tree->ptMakeNode( ~label=None, ~expr = newExpr, )
                            }
                        })
                        foundParents->Js_array2.push( Assertion({ args, frame:frm.frame }) )->ignore
                    }
                    Continue
                }
            )->ignore
        }
    })
    foundParents
}

let proveFloating = (~tree, ~node) => {
    /*
    If a node has a proof, no need to prove it again.
    If a node has Some parents, this means there was an attempt to prove this node earlier,
        so no need in trying to prove it again.
    It follows from the statement above, that all unproved nodes have to have most complete collection of parents, 
        otherwise there is a risk to miss existing proof for this node. So we must not interrupt the process of adding 
        parents even if the root node becomes proved.

    On the other hand:
    We should not skip unproved nodes with Some parents because such nodes could appear as a side effect of proving 
        other nodes and were left unproved because the root node became proved first.
    */
    if (node->pnGetProof->Belt.Option.isNone /* && node->pnGetParents->Belt.Option.isNone */) {
        let tmpTree = ptMake(
            ~parentTree=tree,
            ~allowParentsWithUnprovedFloatings=true,
            ()
        )

        let nodesToCreateParentsFor = Belt_MutableStack.make()
        let savedNodes = Belt_MutableSet.make(~id=module(ExprCmp))

        let saveNodeToCreateParentsFor = node => {
            if (!(savedNodes->Belt_MutableSet.has(node->pnGetExpr))) {
                savedNodes->Belt_MutableSet.add(node->pnGetExpr)
                switch node->pnGetProof {
                    | Some(_) => ()
                    | None => {
                        switch node->pnGetParents {
                            | Some(parents) => {
                                parents->Js_array2.forEach(parent => {
                                    switch parent {
                                        | Assertion({args:})
                                    }
                                })
                            }
                            | None => {
                                savedNodes->Belt_MutableSet.add(node->pnGetExpr)
                                nodesToCreateParentsFor->Belt_MutableStack.push(node)
                            }
                        }
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
                    | Some(_) => ()
                    | None => {
                        let curExpr = curNode->pnGetExpr
                        switch tree->ptGetProvedNodeByExpr(curExpr) {
                            | Some(_) => curNode->pnAddParent(VarType, tmpTree)
                        }
                        if (tmpTree->ptIsNewVarDef(curExpr)) {
                            curNode->pnAddParent(VarType, tmpTree)
                        } else {
                            switch tree->ptGetHypByExpr(curExpr) {
                                | Some(hyp) => curNode->pnAddParent(Hypothesis({label:hyp.label}))
                                | None => {
                                    findParentsWithoutNewVars(~tree, ~expr=curNode->pnGetExpr)
                                        ->Js.Array2.forEach(asrtParent => {
                                            curNode->pnAddParent(asrtParent)//this will fail because of unproved floatings
                                            switch asrtParent {
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
    }

}