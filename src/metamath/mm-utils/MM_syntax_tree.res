open MM_context
open MM_proof_table
open MM_parser

type rec syntaxTreeNode = {
    id: int,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
    height:int,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, parent:syntaxTreeNode, sym:string, color:option<string>, isVar:bool})

let extractVarToRecIdxMapping = (args:array<int>, frame):result<array<int>,string> => {
    let varToRecIdxMapping = Expln_utils_common.createArray(frame.numOfVars)
    let locks = Belt_Array.make(frame.numOfVars, false)
    if (args->Js_array2.length != frame.numOfArgs) {
        Error(`extractVarToRecIdxMapping: args.Js_array2.length != frame.numOfArgs`)
    } else {
        let err = ref(None)
        frame.hyps->Js_array2.forEachi((hyp,i) => {
            if (err.contents->Belt_Option.isNone && hyp.typ == F) {
                let v = hyp.expr[1]
                if (locks[v]) {
                    err := Some(Error(`extractVarToRecIdxMapping: locks[v]`))
                } else {
                    locks[v] = true
                    varToRecIdxMapping[v] = args[i]
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
                label,
                children: Expln_utils_common.createArray(maxI),
                height:0,
            }
            for i in 1 to maxI {
                this.children[i-1] = Symbol({
                    id: idSeq(),
                    parent:this,
                    sym: ctx->ctxIntToSymExn(r.expr[i]),
                    isVar: r.expr[i] >= 0,
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
                                            sym: ctx->ctxIntToSymExn(s),
                                            isVar: false,
                                            color: None,
                                        })
                                    } else {
                                        switch buildSyntaxTreeInner(idSeq, ctx, tbl, Some(this), tbl[varToRecIdxMapping[s]]) {
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
    buildSyntaxTreeInner(idSeq, ctx, tbl, None, tbl[targetIdx])
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

type unifSubs = array<(string,syntaxTreeNode)>

let rec syntaxTreeNodeEq = (a:syntaxTreeNode, b:syntaxTreeNode):bool => {
    a.label == b.label
    && a.children->Js.Array2.everyi((ai,i) => ai->childNodeEq(b.children[i]))
}
and childNodeEq = (a:childNode, b:childNode):bool => {
    switch a {
        | Subtree(aSyntaxTreeNode) => {
            switch b {
                | Subtree(bSyntaxTreeNode) => aSyntaxTreeNode->syntaxTreeNodeEq(bSyntaxTreeNode)
                | Symbol({sym:bSym, isVar:bIsVar}) => false
            }
        }
        | Symbol({sym:aSym, isVar:aIsVar}) => {
            switch b {
                | Subtree(bSyntaxTreeNode) => false
                | Symbol({sym:bSym, isVar:bIsVar}) => aSym == bSym && aIsVar == bIsVar
            }
        }
    }
}

let getExprType = (expr:syntaxTreeNode, ctx:mmContext):int => {
    switch ctx->getFrame(expr.label) {
        | Some(frame) => frame.asrt[0]
        | None => {
            switch ctx->getHypothesis(expr.label) {
                | Some({expr:hypExpr}) => hypExpr[0]
                | None => {
                    raise(MmException({
                        msg:`Could not determine type of a syntax tree node with label '${expr.label}'`
                    }))
                }
            }
        }
    }
}

let isVar = (expr:syntaxTreeNode):option<string> => {
    @warning("-8")
    switch expr.children->Js.Array2.length {
        | 1 => {
            switch expr.children[0] {
                | Subtree(_) => None
                | Symbol({isVar,sym}) => if (isVar) { Some(sym) } else { None }
            }
        }
        | _ => None
    }
}

let rec exprContainsVar = (expr:syntaxTreeNode, var:string):bool => {
    switch expr->isVar {
        | Some(exprVar) => exprVar == var
        | None => {
            expr.children->Js.Array2.some(ch => {
                switch ch {
                    | Symbol(_) => false
                    | Subtree(chNode) => chNode->exprContainsVar(var)
                }
            })
        }
    }
}

let subsIsNotProper = (foundSubs:unifSubs):bool => {
    foundSubs->Js_array2.some(((var,_)) => {
        foundSubs->Js_array2.some(((_,expr)) => expr->exprContainsVar(var))
    })
}

let assignSubs = (foundSubs:unifSubs, var:string, expr:syntaxTreeNode):bool => {
    switch foundSubs->Js_array2.find(((existingVar,_)) => existingVar == var) {
        | Some((_,existingExpr)) => syntaxTreeNodeEq(expr,existingExpr)
        | None => {
            foundSubs->Js_array2.push((var,expr))->ignore
            !subsIsNotProper(foundSubs)
        }
    }
}

let rec unify = (a:syntaxTreeNode, b:syntaxTreeNode, ~ctx:mmContext, ~foundSubs:unifSubs, ~continue:ref<bool>):unit => {
    if (a->getExprType(ctx) != b->getExprType(ctx)) {
        continue := false
    } else {
        switch a->isVar {
            | Some(aVar) => {
                switch b->isVar {
                    | Some(bVar) => {
                        if (aVar != bVar) {
                            continue := assignSubs(foundSubs, aVar, b)
                        }
                    }
                    | None => {
                        continue := assignSubs(foundSubs, aVar, b)
                    }
                }
            }
            | None => {
                switch b->isVar {
                    | Some(bVar) => {
                        continue := assignSubs(foundSubs, bVar, a)
                    }
                    | None => {
                        if (a.children->Js.Array2.length != b.children->Js.Array2.length) {
                            continue := false
                        } else {
                            let maxI = a.children->Js.Array2.length-1
                            let i = ref(0)
                            while (continue.contents && i.contents <= maxI) {
                                switch a.children[i.contents] {
                                    | Symbol({sym:aSym, isVar:aIsVar}) => {
                                        switch b.children[i.contents] {
                                            | Symbol({sym:bSym, isVar:bIsVar}) => {
                                                if (aIsVar || bIsVar || aSym != bSym) {
                                                    continue := false
                                                }
                                            }
                                            | Subtree(_) => continue := false
                                        }
                                    }
                                    | Subtree(aCh) => {
                                        switch b.children[i.contents] {
                                            | Symbol(_) => continue := false
                                            | Subtree(bCh) => {
                                                unify(aCh, bCh, ~ctx, ~foundSubs, ~continue)
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
