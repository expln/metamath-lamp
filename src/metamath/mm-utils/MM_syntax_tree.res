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

type unifSubs = array<(string,childNode)>

let childNodeEq = (a:childNode, b:childNode):bool => {
    raise(MmException({msg:`Not implemented.`}))
}

let assignSubs = (foundSubs:unifSubs, var:string, expr:childNode):bool => {
    switch foundSubs->Js_array2.find(((v,_)) => v == var) {
        | Some((_,chExpr)) => childNodeEq(expr,chExpr)
        | None => {
            foundSubs->Js_array2.push((var,expr))->ignore
            true
        }
    }
}

let rec unify = (a:syntaxTreeNode, b:syntaxTreeNode, foundSubs:unifSubs, continue:ref<bool>):unit => {
    if (a.label != b.label || a.children->Js.Array2.length != b.children->Js.Array2.length) {
        continue := false
    } else {
        let maxI = a.children->Js.Array2.length-1
        let i = ref(0)
        while (continue.contents && i.contents <= maxI) {
            switch a.children[i.contents] {
                | Subtree(ac) => {
                    switch b.children[i.contents] {
                        | Subtree(bc) => unify(ac,bc,foundSubs,continue)
                        | Symbol({sym:bcSym, isVar:bcIsVar}) => {
                            if (bcIsVar) {
                                continue := assignSubs(foundSubs, bcSym, a.children[i.contents])
                            } else {
                                continue := false
                            }
                        }
                    }
                }
                | Symbol({sym:acSym, isVar:acIsVar}) => {
                    switch b.children[i.contents] {
                        | Subtree(bc) => {
                            if (acIsVar) {
                                continue := assignSubs(foundSubs, acSym, b.children[i.contents])
                            } else {
                                continue := false
                            }
                        }
                        | Symbol({sym:bcSym, isVar:bcIsVar}) => {
                            if (acIsVar) {
                                continue := assignSubs(foundSubs, acSym, b.children[i.contents])
                            } else if (bcIsVar) {
                                continue := assignSubs(foundSubs, bcSym, a.children[i.contents])
                            } else if (acSym != bcSym) {
                                continue := false
                            }
                        }
                    }
                }
            }
            i := i.contents + 1
        }
    }
}