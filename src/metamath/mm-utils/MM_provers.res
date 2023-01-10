open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_progress_tracker
open MM_proof_tree2

let findParentsWithoutNewVars = ( ~tree, ~node, ):array<exprSource> => {
    let foundParents = []
    tree->proofTreeGetFrms->Belt_MapString.forEach((_,frm) => {
        let expr = node.expr
        let frmExpr = frm.frame.asrt
        if (frmExpr[0] == expr[0]) {
            iterateSubstitutions(
                ~frmExpr,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE],
                ~constParts = frm.constParts[frm.numOfHypsE],
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt=tree->proofTreeGetParenCnt,
                ~consumer = subs => {
                    if (subs.isDefined->Js_array2.every(b=>b)
                        && verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx=tree->proofTreeIsDisj)) {
                        let args = frm.frame.hyps->Js_array2.map(hyp => {
                            let newExpr = applySubs(
                                ~frmExpr = hyp.expr, 
                                ~subs,
                                ~createWorkVar = 
                                    _ => raise(MmException({msg:`Work variables are not supported in addParentsWithoutNewVars().`}))
                            )
                            switch tree->proofTreeGetNodeByExpr(newExpr) {
                                | Some(existingNode) => existingNode
                                | None => tree->proofTreeMakeNode( ~label=None, ~expr = newExpr, )
                            }
                        })
                        foundParents->Js_array2.push(Assertion({ args, label:frm.frame.label }))->ignore
                    }
                    Continue
                }
            )->ignore
        }
    })
    foundParents
}

let proveFloating = (~tree, ~node) => {
    let rootNode = node
    let nodesToCreateParentsFor = Belt_MutableStack.make()
    let savedNodes = Belt_MutableSet.make(~id=module(ExprCmp))
    nodesToCreateParentsFor->Belt_MutableStack.push(rootNode)
    while (rootNode->proofNodeGetProof->Belt_Option.isNone && !(nodesToCreateParentsFor->Belt_MutableStack.isEmpty)) {
        let curNode = nodesToCreateParentsFor->Belt_MutableStack.pop->Belt_Option.getExn
        switch curNode->proofNodeGetParents {
            | Some(_) => ()
            | None => {
                let curExpr = curNode->proofNodeGetExpr
                if (tree->proofTreeIsNewVarDef(curExpr)) {
                    curNode->proofNodeAddNonAsrtParent(VarType)
                } else {
                    switch tree->proofTreeGetHypByExpr(curExpr) {
                        | Some(hyp) => {
                            curNode->proofNodeAddNonAsrtParent(Hypothesis({label:hyp.label}))
                        }
                        | None => {
                            curNode.parents = Some([])
                            addParentsWithoutNewVars(~tree, ~node=curNode, ~stackOfNodesToCreateParentsFor, ~exprToStr)
                        }
                    }
                }
            }
        }
    }
}