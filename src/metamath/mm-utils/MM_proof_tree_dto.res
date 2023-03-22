open MM_parser
open MM_context
open MM_proof_table
open MM_proof_tree
open MM_unification_debug

type exprSrcDto =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})
    | AssertionWithErr({args:array<int>, label:string, err:unifErr})

type proofNodeDbgDto = {
    exprStr:string,
}

type proofTreeDbgDto = {
    newVars: array<string>,
    disj: array<string>,
}

type proofNodeDto = {
    expr:expr,
    parents: array<exprSrcDto>,
    proof: option<exprSrcDto>,
    dbg:option<proofNodeDbgDto>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofNodeDto>,
    dbg: option<proofTreeDbgDto>,
}

let exprSrcToDto = (
    src:exprSrc, 
    exprToIdx:Belt_HashMap.t<expr,int,ExprHash.identity>,
):exprSrcDto => {
    let nodeToIdx = (node:proofNode):int => {
        switch exprToIdx->Belt_HashMap.get(node->pnGetExpr) {
            | None => raise(MmException({msg:`exprSrcToDto: cannot get idx by expr.`}))
            | Some(idx) => idx
        }
    }

    switch src {
        | VarType => VarType
        | Hypothesis({label}) => Hypothesis({label:label})
        | Assertion({args, frame}) => {
            Assertion({
                args: args->Js_array2.map(nodeToIdx), 
                label: frame.label,
            })
        }
        | AssertionWithErr({args, frame, err}) => {
            AssertionWithErr({
                args: args->Js_array2.map(nodeToIdx), 
                label: frame.label,
                err
            })
        }
    }
}

let proofNodeToDto = (
    node:proofNode, 
    exprToIdx:Belt_HashMap.t<expr,int,ExprHash.identity>,
):proofNodeDto => {
    {
        expr:node->pnGetExpr,
        dbg:node->pnGetDbg->Belt_Option.map(dbg => {
            {
                exprStr: dbg.exprStr,
            }
        }),
        parents: node->pnGetEParents->Js_array2.map(exprSrcToDto(_,exprToIdx)),
        proof: node->pnGetProof->Belt.Option.map(exprSrcToDto(_,exprToIdx)),
    }
}

let collectAllExprs = (
    tree:proofTree, 
    roots:array<expr>,
):Belt_HashMap.t<expr,int,ExprHash.identity> => {
    let nodesToProcess = Belt_MutableStack.make()
    roots->Js_array2.forEach(expr => nodesToProcess->Belt_MutableStack.push(tree->ptGetNode(expr)))
    let processedNodes = Belt_HashSet.make(~id=module(ExprHash), ~hintSize=100)
    let res = Belt_HashMap.make(~id=module(ExprHash), ~hintSize=100)

    let saveNodesFromSrc = (src:exprSrc) => {
        switch src {
            | Assertion({args}) | AssertionWithErr({args}) =>
                args->Js_array2.forEach(arg => nodesToProcess->Belt_MutableStack.push(arg))
            | VarType | Hypothesis(_) => ()
        }
    }

    while (!(nodesToProcess->Belt_MutableStack.isEmpty)) {
        let curNode = nodesToProcess->Belt_MutableStack.pop->Belt.Option.getExn
        let curExpr = curNode->pnGetExpr
        if (!(processedNodes->Belt_HashSet.has(curExpr))) {
            processedNodes->Belt_HashSet.add(curExpr)
            res->Belt_HashMap.set(curExpr, res->Belt_HashMap.size)
            curNode->pnGetEParents->Js_array2.forEach(saveNodesFromSrc)
            curNode->pnGetProof->Belt_Option.forEach(saveNodesFromSrc)
        }
    }
    res
}

let proofTreeToDto = (
    tree:proofTree, 
    rootStmts:array<expr>, 
):proofTreeDto => {
    let exprToIdx = collectAllExprs(tree, rootStmts)
    let nodes = Expln_utils_common.createArray(exprToIdx->Belt_HashMap.size)
    exprToIdx->Belt_HashMap.forEach((expr,idx) => {
        nodes[idx] = proofNodeToDto(tree->ptGetNode(expr), exprToIdx)
    })

    {
        newVars: tree->ptGetCopyOfNewVars,
        disj: tree->ptGetDisj,
        nodes,
        dbg: tree->ptGetDbg->Belt_Option.map(dbg => {
            {
                newVars: dbg.newVars,
                disj: dbg.disj,
            }
        })
    }
}

let createProofTable = (tree:proofTreeDto, root:proofNodeDto):proofTable => {
    let processedExprs = Belt_MutableSet.make(~id = module(ExprCmp))
    let exprToIdx = Belt_MutableMap.make(~id = module(ExprCmp))
    let tbl = []
    Expln_utils_data.traverseTree(
        (),
        root,
        (_,n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofNodeDto [1].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [1].`}))
                | Some(Hypothesis(_)) => None
                | Some(Assertion({args})) => {
                    if (processedExprs->Belt_MutableSet.has(n.expr)) {
                        None
                    } else {
                        Some(args->Js.Array2.map(idx => tree.nodes[idx]))
                    }
                }
            }
        },
        ~process = (_, n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofNodeDto [2].`}))
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
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofNodeDto [3].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [3].`}))
                | Some(Hypothesis(_)) => ()
                | Some(Assertion({args, label})) => {
                    if (exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({
                            proof:Assertion({
                                label,
                                args: args->Js_array2.map(nodeIdx => {
                                    exprToIdx->Belt_MutableMap.get(tree.nodes[nodeIdx].expr)->Belt_Option.getWithDefault(-1)
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
