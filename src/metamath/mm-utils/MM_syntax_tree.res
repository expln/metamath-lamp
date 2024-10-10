open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: int,
    parent:option<syntaxTreeNode>,
    typ:int,
    label:string,
    children:array<childNode>,
    height:int,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, parent:syntaxTreeNode, symInt:int, sym:string, color:option<string>, isVar:bool})

let extractVarToRecIdxMapping = (args:array<int>, frame):result<array<int>,string> => {
    let varToRecIdxMapping = Expln_utils_common.createArray(frame.numOfVars)
    let locks = Belt_Array.make(frame.numOfVars, false)
    if (args->Js_array2.length != frame.numOfArgs) {
        Error(`extractVarToRecIdxMapping: args.Js_array2.length != frame.numOfArgs`)
    } else {
        let err = ref(None)
        frame.hyps->Js_array2.forEachi((hyp,i) => {
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
                if (locks->Js_array2.some(lock => !lock)) {
                    Error(`extractVarToRecIdxMapping: locks->Js_array2.some(lock => !lock)`)
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
    children->Js_array2.reduce(
        (max,ch) => Js_math.max_int(max,ch->getHeight),
        0
    )
}

let rec buildSyntaxTreeInner = (idSeq, ctx, tbl, parent, r):result<syntaxTreeNode,string> => {
    switch r.proof {
        | Hypothesis({label}) => {
            let maxI = r.expr->Js_array2.length - 1
            let this = {
                id: idSeq(),
                parent,
                typ:r.expr->Array.getUnsafe(0),
                label,
                children: Expln_utils_common.createArray(maxI),
                height:0,
            }
            for i in 1 to maxI {
                this.children[i-1] = Symbol({
                    id: idSeq(),
                    parent:this,
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
                                parent,
                                typ:frame.asrt->Array.getUnsafe(0),
                                label,
                                children: Expln_utils_common.createArray(frame.asrt->Js_array2.length - 1),
                                height:0,
                            }
                            let err = ref(None)
                            frame.asrt->Js_array2.forEachi((s,i) => {
                                if (i > 0 && err.contents->Belt_Option.isNone) {
                                    if (s < 0) {
                                        this.children[i-1] = Symbol({
                                            id: idSeq(),
                                            parent:this,
                                            symInt: s,
                                            sym: ctx->ctxIntToSymExn(s),
                                            isVar: false,
                                            color: None,
                                        })
                                    } else {
                                        switch buildSyntaxTreeInner(idSeq, ctx, tbl, Some(this), tbl->Array.getUnsafe(varToRecIdxMapping->Array.getUnsafe(s))) {
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
    buildSyntaxTreeInner(idSeq, ctx, tbl, None, tbl->Array.getUnsafe(targetIdx))
}

let rec syntaxTreeToSymbols: syntaxTreeNode => array<string> = node => {
    node.children->Js_array2.map(childNode => {
        switch childNode {
            | Subtree(node) => syntaxTreeToSymbols(node)
            | Symbol({sym}) => [sym]
        }
    })->Belt_Array.concatMany
}

let syntaxTreeIsEmpty: syntaxTreeNode => bool = node => {
    node.children->Js_array2.length == 0
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
        },
        ()
    )
    found
}

let buildSyntaxProofTableFromProofTreeDto = (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
):result<proofTable,string> => {
    switch proofTreeDto.nodes->Js_array2.find(node => node.expr->exprEq(typeStmt)) {
        | None => Error(`buildSyntaxProofTableFromProofTreeDto: could not find a proof for: ${ctx->ctxIntsToStrExn(typeStmt)}`)
        | Some(proofNode) => Ok(MM_proof_tree_dto.createProofTable(~tree=proofTreeDto, ~root=proofNode, ()))
    }
}

let buildSyntaxTreeFromProofTreeDto = (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
):result<syntaxTreeNode,string> => {
    switch buildSyntaxProofTableFromProofTreeDto( ~ctx, ~proofTreeDto, ~typeStmt, ) {
        | Error(msg) => Error(msg)
        | Ok(proofTable) => buildSyntaxTree(ctx, proofTable, proofTable->Js_array2.length-1)
    }
}

type unifSubs = Belt_HashMapString.t<array<string>>

let isVar = (expr:syntaxTreeNode, isMetavar:string=>bool):option<(int,string)> => {
    @warning("-8")
    switch expr.children->Js.Array2.length {
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
    while (i.contents < expr->Js_array2.length) {
        if (expr->Array.getUnsafe(i.contents) == e) {
            expr->Js_array2.spliceInPlace(~pos=i.contents, ~remove=1, ~add=subExpr)->ignore
            i := i.contents + subExpr->Js_array2.length
        } else {
            i := i.contents + 1
        }
    }
}

let applySubsInPlace = (expr:array<string>, subs:unifSubs):unit => {
    subs->Belt_HashMapString.forEachU((. v, subExpr) => substituteInPlace(expr, v, subExpr))
}

let assignSubs = (foundSubs:unifSubs, var:string, expr:array<string>):bool => {
    if (expr->Js_array2.includes(var)) {
        false
    } else {
        applySubsInPlace(expr, foundSubs)
        switch foundSubs->Belt_HashMapString.get(var) {
            | Some(existingExpr) => expr == existingExpr
            | None => {
                foundSubs->Belt_HashMapString.set(var, expr)
                foundSubs->Belt_HashMapString.forEachU((. _, expr) => applySubsInPlace(expr, foundSubs))
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
    if (a.typ != b.typ) {
        continue := false
    } else {
        switch a->isVar(isMetavar) {
            | Some((_,aVar)) => {
                switch b->isVar(isMetavar) {
                    | Some((_,bVar)) => {
                        if (aVar != bVar) {
                            continue := assignSubs(foundSubs, aVar, b->getAllSymbols)
                        }
                    }
                    | None => {
                        continue := assignSubs(foundSubs, aVar, b->getAllSymbols)
                    }
                }
            }
            | None => {
                switch b->isVar(isMetavar) {
                    | Some((_,bVar)) => {
                        continue := assignSubs(foundSubs, bVar, a->getAllSymbols)
                    }
                    | None => {
                        if (a.children->Js.Array2.length != b.children->Js.Array2.length) {
                            continue := false
                        } else {
                            let maxI = a.children->Js.Array2.length-1
                            let i = ref(0)
                            while (continue.contents && i.contents <= maxI) {
                                switch a.children->Array.getUnsafe(i.contents) {
                                    | Symbol({sym:aSym, isVar:aIsVar}) => {
                                        switch b.children->Array.getUnsafe(i.contents) {
                                            | Symbol({sym:bSym, isVar:bIsVar}) => {
                                                if (aIsVar || bIsVar || aSym != bSym) {
                                                    continue := false
                                                }
                                            }
                                            | Subtree(_) => continue := false
                                        }
                                    }
                                    | Subtree(aCh) => {
                                        switch b.children->Array.getUnsafe(i.contents) {
                                            | Symbol(_) => continue := false
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
        ~process = (_, node) => consumer(node),
        ()
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
    res->Js.Array2.joinWith(" ")
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