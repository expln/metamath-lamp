open MM_context
open MM_parser
open MM_proof_table

type rec syntaxTreeNode = {
    id: string,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:string, sym:string})

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

let rec buildSyntaxTreeInner = (idSeq, ctx, tbl, parent, r):result<syntaxTreeNode,string> => {
    switch r.proof {
        | Hypothesis({label}) => {
            Ok({
                id: idSeq(),
                parent,
                label,
                children: {
                    let maxI = r.expr->Js_array2.length - 1
                    let children = Expln_utils_common.createArray(maxI)
                    for i in 1 to maxI {
                        children[i-1] = Symbol({
                            id: idSeq(),
                            sym: ctx->ctxIntToSymExn(r.expr[i])
                        })
                    }
                    children
                }
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
                                children: Expln_utils_common.createArray(frame.asrt->Js_array2.length - 1)
                            }
                            let err = ref(None)
                            frame.asrt->Js_array2.forEachi((s,i) => {
                                if (i > 0 && err.contents->Belt_Option.isNone) {
                                    if (s < 0) {
                                        this.children[i-1] = Symbol({
                                            id: idSeq(),
                                            sym: ctx->ctxIntToSymExn(s)
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
                                | None => Ok(this)
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
        let id = nextId.contents->Belt_Int.toString
        nextId := nextId.contents + 1
        id
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