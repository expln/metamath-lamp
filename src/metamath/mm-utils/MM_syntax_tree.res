open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: int,
    typ:int,
    label:string,
    children:array<childNode>,
    height:int,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, symInt:int, sym:string, color:option<string>, isVar:bool})

let extractVarToRecIdxMapping = (args:array<int>, frame):result<array<int>,string> => {
    let varToRecIdxMapping = Expln_utils_common.createArray(frame.numOfVars)
    let locks = Belt_Array.make(frame.numOfVars, false)
    if (args->Array.length != frame.numOfArgs) {
        Error(`extractVarToRecIdxMapping: args.Array.length != frame.numOfArgs`)
    } else {
        let err = ref(None)
        frame.hyps->Array.forEachWithIndex((hyp,i) => {
            if (err.contents->Belt_Option.isNone && hyp.typ == F) {
                let v = hyp.expr->Array.getUnsafe(1)
                if (locks->Array.getUnsafe(v)) {
                    err := Some(Error(`extractVarToRecIdxMapping: locks[v]`))
                } else {
                    locks[v] = true
                    varToRecIdxMapping[v] = args->Array.getUnsafe(i)
                }
            }
        })
        switch err.contents {
            | Some(err) => err
            | None => {
                if (locks->Array.some(lock => !lock)) {
                    Error(`extractVarToRecIdxMapping: locks->Array.some(lock => !lock)`)
                } else {
                    Ok(varToRecIdxMapping)
                }
            }
        }
    }
}

let getHeight = (ch:childNode):int => {
    switch ch {
        | Subtree({height}) => height
        | Symbol(_) => 0
    }
}

let getMaxHeight = (children:array<childNode>):int => {
    children->Array.reduce(
        0,
        (max,ch) => Math.Int.max(max,ch->getHeight)
    )
}

let rec buildSyntaxTreeInner = (idSeq, ctx, tbl, r):result<syntaxTreeNode,string> => {
    switch r.proof {
        | Hypothesis({label}) => {
            let maxI = r.expr->Array.length - 1
            let this = {
                id: idSeq(),
                typ:r.expr->Array.getUnsafe(0),
                label,
                children: Expln_utils_common.createArray(maxI),
                height:0,
            }
            for i in 1 to maxI {
                this.children[i-1] = Symbol({
                    id: idSeq(),
                    symInt: r.expr->Array.getUnsafe(i),
                    sym: ctx->ctxIntToSymExn(r.expr->Array.getUnsafe(i)),
                    isVar: r.expr->Array.getUnsafe(i) >= 0,
                    color: None,
                })
            }
            Ok({
                ...this,
                height: getMaxHeight(this.children) + 1
            })
        }
        | Assertion({args, label}) => {
            switch ctx->getFrame(label) {
                | None => Error(`Cannot find a frame by label '${label}' in buildSyntaxTreeInner.`)
                | Some(frame) => {
                    switch extractVarToRecIdxMapping(args, frame) {
                        | Error(msg) => Error(msg)
                        | Ok(varToRecIdxMapping) => {
                            let this = {
                                id: idSeq(),
                                typ:frame.asrt->Array.getUnsafe(0),
                                label,
                                children: Expln_utils_common.createArray(frame.asrt->Array.length - 1),
                                height:0,
                            }
                            let err = ref(None)
                            frame.asrt->Array.forEachWithIndex((s,i) => {
                                if (i > 0 && err.contents->Belt_Option.isNone) {
                                    if (s < 0) {
                                        this.children[i-1] = Symbol({
                                            id: idSeq(),
                                            symInt: s,
                                            sym: ctx->ctxIntToSymExn(s),
                                            isVar: false,
                                            color: None,
                                        })
                                    } else {
                                        switch buildSyntaxTreeInner(idSeq, ctx, tbl, tbl->Array.getUnsafe(varToRecIdxMapping->Array.getUnsafe(s))) {
                                            | Error(msg) => err := Some(Error(msg))
                                            | Ok(subtree) => this.children[i-1] = Subtree(subtree)
                                        }
                                        
                                    }
                                }
                            })
                            switch err.contents {
                                | Some(err) => err
                                | None => {
                                    Ok({
                                        ...this,
                                        height: getMaxHeight(this.children) + 1
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

let buildSyntaxTree = (ctx, tbl, targetIdx):result<syntaxTreeNode,string> => {
    let nextId = ref(0)
    let idSeq = () => {
        nextId := nextId.contents + 1
        nextId.contents - 1
    }
    buildSyntaxTreeInner(idSeq, ctx, tbl, tbl->Array.getUnsafe(targetIdx))
}

let rec syntaxTreeToSymbols: syntaxTreeNode => array<string> = node => {
    node.children->Array.map(childNode => {
        switch childNode {
            | Subtree(node) => syntaxTreeToSymbols(node)
            | Symbol({sym}) => [sym]
        }
    })->Belt_Array.concatMany
}

let syntaxTreeIsEmpty: syntaxTreeNode => bool = node => {
    node.children->Array.length == 0
}

let getNodeById = (
    tree:syntaxTreeNode, 
    childId:int, 
):option<childNode> => {
    let (_, found) = Expln_utils_data.traverseTree(
        (),
        Subtree(tree),
        (_, node) => {
            switch node {
                | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                | Symbol(_) => None
            }
        },
        ~process = (_, node) => {
            switch node {
                | Subtree({id}) | Symbol({id}) => {
                    if (id == childId) {
                        Some(node)
                    } else {
                        None
                    }
                }
            }
        }
    )
    found
}

let buildSyntaxProofTableFromProofTreeDto = (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
):result<proofTable,string> => {
    switch proofTreeDto.nodes->Array.find(node => node.expr->exprEq(typeStmt)) {
        | None => Error(`buildSyntaxProofTableFromProofTreeDto: could not find a proof for: ${ctx->ctxIntsToStrExn(typeStmt)}`)
        | Some(proofNode) => Ok(MM_proof_tree_dto.createProofTable(~tree=proofTreeDto, ~root=proofNode))
    }
}

let buildSyntaxTreeFromProofTreeDto = (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
):result<syntaxTreeNode,string> => {
    switch buildSyntaxProofTableFromProofTreeDto( ~ctx, ~proofTreeDto, ~typeStmt, ) {
        | Error(msg) => Error(msg)
        | Ok(proofTable) => buildSyntaxTree(ctx, proofTable, proofTable->Array.length-1)
    }
}

type unifSubs = Belt_HashMapString.t<array<string>>

let isVar = (expr:syntaxTreeNode, isMetavar:string=>bool):option<(int,string)> => {
    @warning("-8")
    switch expr.children->Array.length {
        | 1 => {
            switch expr.children->Array.getUnsafe(0) {
                | Subtree(_) => None
                | Symbol({isVar,symInt,sym}) => if (isVar && isMetavar(sym)) { Some((symInt,sym)) } else { None }
            }
        }
        | _ => None
    }
}

let substituteInPlace = (expr:array<string>, e:string, subExpr:array<string>):unit => {
    let i = ref(0)
    while (i.contents < expr->Array.length) {
        if (expr->Array.getUnsafe(i.contents) == e) {
            expr->Array.splice(~start=i.contents, ~remove=1, ~insert=subExpr)
            i := i.contents + subExpr->Array.length
        } else {
            i := i.contents + 1
        }
    }
}

let applySubsInPlace = (expr:array<string>, subs:unifSubs):unit => {
    subs->Belt_HashMapString.forEach((v, subExpr) => substituteInPlace(expr, v, subExpr))
}

let assignSubs = (foundSubs:unifSubs, var:string, expr:array<string>):bool => {
    applySubsInPlace(expr, foundSubs)
    if (expr->Array.includes(var)) {
        false
    } else {
        switch foundSubs->Belt_HashMapString.get(var) {
            | Some(existingExpr) => expr == existingExpr
            | None => {
                foundSubs->Belt_HashMapString.set(var, expr)
                foundSubs->Belt_HashMapString.forEach((_, expr) => applySubsInPlace(expr, foundSubs))
                true
            }
        }
    }
}

let rec getAllSymbols = (syntaxTreeNode:syntaxTreeNode):array<string> => {
    syntaxTreeNode.children->Expln_utils_common.arrFlatMap(ch => {
        switch ch {
            | Subtree(syntaxTreeNode) => getAllSymbols(syntaxTreeNode)
            | Symbol({sym}) => [sym]
        }
    })
}

let eqModSubs = (subs:unifSubs, a:string, b:string):bool => {
    let expr1 = [a]
    applySubsInPlace(expr1, subs)
    let expr2 = [b]
    applySubsInPlace(expr2, subs)
    expr1 == expr2
}

/*
    The core idea of the unification algorithm is as per explanations by Mario Carneiro.
    https://github.com/expln/metamath-lamp/issues/77#issuecomment-1577804381
*/
let rec unify = ( 
    a:syntaxTreeNode, 
    b:syntaxTreeNode, 
    ~isMetavar:string=>bool, 
    ~foundSubs:unifSubs, 
    ~continue:ref<bool>
):unit => {
    switch a->isVar(isMetavar) {
        | Some((_,aVar)) => {
            switch b->isVar(isMetavar) {
                | Some((_,bVar)) => {
                    continue := eqModSubs(foundSubs, aVar, bVar) || assignSubs(foundSubs, aVar, [bVar])
                }
                | None => continue := assignSubs(foundSubs, aVar, b->getAllSymbols)
            }
        }
        | None => {
            switch b->isVar(isMetavar) {
                | Some((_,bVar)) => continue := assignSubs(foundSubs, bVar, a->getAllSymbols)
                | None => {
                    if (a.children->Array.length != b.children->Array.length) {
                        continue := false
                    } else {
                        let maxI = a.children->Array.length-1
                        let i = ref(0)
                        while (continue.contents && i.contents <= maxI) {
                            switch a.children->Array.getUnsafe(i.contents) {
                                | Symbol({sym:aSym, isVar:aIsVar}) => {
                                    switch b.children->Array.getUnsafe(i.contents) {
                                        | Symbol({sym:bSym, isVar:bIsVar}) => {
                                            continue := eqModSubs(foundSubs, aSym, bSym)
                                                || (aIsVar && isMetavar(aSym) && assignSubs(foundSubs, aSym, [bSym]))
                                                || (bIsVar && isMetavar(bSym) && assignSubs(foundSubs, bSym, [aSym]))
                                        }
                                        | Subtree(bCh) => {
                                            continue := aIsVar && isMetavar(aSym) 
                                                && assignSubs(foundSubs, aSym, bCh->getAllSymbols)
                                        }
                                    }
                                }
                                | Subtree(aCh) => {
                                    switch b.children->Array.getUnsafe(i.contents) {
                                        | Symbol({sym:bSym, isVar:bIsVar}) => {
                                            continue := bIsVar && isMetavar(bSym) 
                                                && assignSubs(foundSubs, bSym, aCh->getAllSymbols)
                                        }
                                        | Subtree(bCh) => {
                                            unify(aCh, bCh, ~isMetavar, ~foundSubs, ~continue)
                                        }
                                    }
                                }
                            } 
                            i := i.contents + 1
                        }
                    }
                }
            }
        }
    }
}

let syntaxTreeForEachNode = ( tree:childNode, consumer:childNode=>option<'r>):option<'r> => {
    let (_, res) = Expln_utils_data.traverseTree(
        (),
        tree,
        (_, node) => {
            switch node {
                | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                | Symbol(_) => None
            }
        },
        ~process = (_, node) => consumer(node)
    )
    res
}

let syntaxTreeGetIdsOfAllChildSymbols = (tree:childNode):Belt_SetInt.t => {
    let res = []
    tree->syntaxTreeForEachNode(node => {
        switch node {
            | Subtree(_) => ()
            | Symbol({id}) => res->Array.push(id)
        }
        None
    })->ignore
    Belt_SetInt.fromArray(res)
}

let syntaxTreeToText = (node:childNode):string => {
    let res = []
    node->syntaxTreeForEachNode(node => {
        switch node {
            | Subtree(_) => ()
            | Symbol({sym}) => res->Array.push(sym)
        }
        None
    })->ignore
    res->Array.joinUnsafe(" ")
}

let syntaxTreeGetNumberOfSymbols = (node:childNode):int => {
    let cnt = ref(0)
    node->syntaxTreeForEachNode(node => {
        switch node {
            | Subtree(_) => ()
            | Symbol(_) => cnt := cnt.contents + 1
        }
        None
    })->ignore
    cnt.contents
}

let rec syntaxTreeGetParent = (root:syntaxTreeNode, childId:int): option<syntaxTreeNode> => {
    root.children->Array.reduce(None, (res, ch) => {
        switch res {
            | Some(_) => res
            | None => {
                switch ch {
                    | Symbol({id}) => id == childId ? Some(root) : None
                    | Subtree(subRoot) => {
                        if (subRoot.id == childId) {
                            Some(root)
                        } else {
                            subRoot->syntaxTreeGetParent(childId)
                        }
                    }
                }
            }
        }
    })
}