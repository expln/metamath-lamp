open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_proof_table
open MM_progress_tracker

type rec proofTreeNode = {
    expr:expr,
    exprStr:option<string>, //for debug purposes
    label: option<string>,
    mutable parents: option<array<exprSource>>,
    mutable children: array<proofTreeNode>,
    mutable proof: option<exprSource>,
}
and exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({ args:array<proofTreeNode>, label:string })

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

let getExprStrFromNode = (node:proofTreeNode):string => {
    switch node.exprStr {
        | Some(str) => str
        | None => node.expr->Js_array2.map(Belt_Int.toString)->Js.Array2.joinWith(" ")
    }
}

let createProofTree = (
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

let getNodeByExpr = ( tree:proofTree, expr:expr ):option<proofTreeNode> => {
    tree.nodes->Belt_MutableMap.get(expr)
}

let addRootNode = (tree, node):unit => {
    tree.rootNodes->Belt_MutableMap.set(node.expr, node)
}

let createNode = ( tree:proofTree, ~label:option<string>, ~expr:expr, ~exprStr:option<string>, ):proofTreeNode => {
    let getExprStr = () => {
        switch exprStr {
            | Some(str) => str
            | None => expr->Js_array2.map(Belt_Int.toString)->Js.Array2.joinWith(" ")
        }
    }
    switch tree.nodes->Belt_MutableMap.get(expr) {
        | Some(node) => 
            raise(MmException({
                msg:`Creation of a new node '${getExprStr()}' was requested, but a node with such expr already exists.`
            }))
        | None => {
            let node = {
                label,
                expr,
                exprStr,
                parents: None,
                proof: None,
                children: [],
            }
            tree.nodes->Belt_MutableMap.set(expr, node)->ignore
            node
        }
    }
}

let exprSrcIsProved = (exprSrc:exprSource): bool => {
    switch exprSrc {
        | VarType | Hypothesis(_) => true
        | Assertion({args}) => args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)
    }
}

let getProofFromParents = (node):option<exprSource> => {
    switch node.parents {
        | None => None
        | Some(parents) => parents->Js_array2.find(exprSrcIsProved)
    }
}

let markProved = ( node:proofTreeNode ):unit => {
    switch node.proof {
        | Some(_) => ()
        | None => {
            switch getProofFromParents(node) {
                | None => ()
                | Some(nodeProof) => {
                    node.proof = Some(nodeProof)
                    let nodesToMarkProved = node.children->Belt_MutableQueue.fromArray
                    while (!(nodesToMarkProved->Belt_MutableQueue.isEmpty)) {
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
    }
}

let addChild = (node, child): unit => {
    
}

let addParent = (tree:proofTree, node:proofTreeNode, parent:exprSource):unit => {
    switch node.proof {
        | Some(_) => ()
        | None => {
            switch node.parents {
                | None => node.parents = Some([parent])
                | Some(parents) => {
                    switch parents->Js_array2.find(par => exprSourceEq(par, parent)) {
                        | Some(existingParent) => {
                            if (exprSrcIsProved(existingParent)) {
                                raise(MmException({
                                    msg:`A node '${getExprStrFromNode(node)}' has a proved parent but is not marked as proved.`
                                }))
                            }
                        }
                        | None =>
                    }
                }
            }
            if (exprSrcIsProved(parent)) {
                markProved(node)
            }
        }
    }
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