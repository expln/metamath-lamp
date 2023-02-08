open MM_parser
open MM_context
open MM_proof_table
open MM_proof_tree

type exprSourceDto =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofNodeDto = {
    expr:expr,
    exprStr:option<string>, //for debug purposes
    parents: option<array<exprSourceDto>>,
    proof: option<exprSourceDto>,
}

type dbgProofTreeDto = {
    newVars: array<string>,
    disj: array<string>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofNodeDto>,
    dbg: option<dbgProofTreeDto>,
}

let exprSourceToDto = (src:exprSource, exprToIdx:Belt_HashMap.t<expr,int,ExprHash.identity>):option<exprSourceDto> => {
    switch src {
        | VarType => Some(VarType)
        | Hypothesis({label}) => Some(Hypothesis({label:label}))
        | Assertion({args, label}) => {
            if (args->Js.Array2.some(arg => arg->pnIsInvalidFloating && arg->pnGetProof->Belt.Option.isNone)) {
                None
            } else {
                Some(Assertion({
                    args: args->Js_array2.map(arg => {
                        switch exprToIdx->Belt_HashMap.get(arg->pnGetExpr) {
                            | None => raise(MmException({msg:`exprSourceToDto: cannot get idx by expr.`}))
                            | Some(idx) => idx
                        }
                    }), 
                    label
                }))
            }
        }
    }
}

let proofNodeToDto = (node:proofNode, exprToIdx:Belt_HashMap.t<expr,int,ExprHash.identity>):proofNodeDto => {
    {
        expr:node->pnGetExpr,
        exprStr:node->pnGetExprStr,
        parents: node->pnGetParents->Belt.Option.map(parents => {
            parents
                ->Js_array2.map(exprSourceToDto(_,exprToIdx))
                ->Js_array2.filter(Belt_Option.isSome)
                ->Js_array2.map(Belt_Option.getExn)
        }),
        proof: node->pnGetProof->Belt.Option.flatMap(exprSourceToDto(_,exprToIdx)),
    }
}

let collectAllExprs = (tree:proofTree, roots:array<expr>):Belt_HashMap.t<expr,int,ExprHash.identity> => {
    let nodesToProcess = Belt_MutableStack.make()
    roots->Js_array2.forEach(expr => nodesToProcess->Belt_MutableStack.push(tree->ptGetOrCreateNode(expr)))
    let processedNodes = Belt_HashSet.make(~id=module(ExprHash), ~hintSize=100)
    let res = Belt_HashMap.make(~id=module(ExprHash), ~hintSize=100)

    let saveNodesFromSrc = (src:exprSource) => {
        switch src {
            | Assertion({args}) => {
                args->Js_array2.forEach(arg => {
                    if (!(arg->pnIsInvalidFloating) || arg->pnGetProof->Belt.Option.isSome) {
                        nodesToProcess->Belt_MutableStack.push(arg)
                    }
                })
            }
            | _ => ()
        }
    }

    while (!(nodesToProcess->Belt_MutableStack.isEmpty)) {
        let curNode = nodesToProcess->Belt_MutableStack.pop->Belt.Option.getExn
        let curExpr = curNode->pnGetExpr
        if (!(processedNodes->Belt_HashSet.has(curExpr))) {
            processedNodes->Belt_HashSet.add(curExpr)
            res->Belt_HashMap.set(curExpr, res->Belt_HashMap.size)
            curNode->pnGetParents->Belt_Option.forEach(Js_array2.forEach(_, saveNodesFromSrc))
            curNode->pnGetProof->Belt_Option.forEach(saveNodesFromSrc)
        }
    }
    res
}

let proofTreeToDto = (tree:proofTree, stmts:array<expr>):proofTreeDto => {
    let exprToIdx = collectAllExprs(tree, stmts)
    let nodes = Expln_utils_common.createArray(exprToIdx->Belt_HashMap.size)
    exprToIdx->Belt_HashMap.forEach((expr,idx) => {
        nodes[idx] = proofNodeToDto(tree->ptGetOrCreateNode(expr), exprToIdx)
    })
    let newVars = tree->ptGetCopyOfNewVars
    let disj = tree->ptGetCopyOfDisj
    let dbg = switch tree->ptGetExprToStr {
        | None => None
        | Some(exprToStr) => {
            let dbgNewVars = newVars->Js.Array2.map(exprToStr)
            let dbgDisj = []
            disj->disjForEach((n,m) => {
                dbgDisj->Js.Array2.push(exprToStr([n,m]))->ignore
            })
            Some({ newVars:dbgNewVars, disj:dbgDisj })
        }
    }
    { newVars, disj, nodes, dbg }
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

let exprSourceDtoEq = (s1,s2) => {
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
                    && args1->Js.Array2.everyi((arg1,idx) => arg1 ==args2[idx])
                }
                | _ => false
            }
        }
    }
}