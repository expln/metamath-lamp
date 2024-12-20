open MM_context
open MM_proof_tree

type rec asrtSyntaxTreeNode = {
    typ:int,
    children:array<asrtChildNode>,
}
and asrtChildNode =
    | Subtree(asrtSyntaxTreeNode)
    | Symbol(int)

type sym =
    | Const(int)
    | CtxVar(int)
    | AsrtVar(int)

let symEq = (a,b) => {
    switch a {
        | Const(a) => {
            switch b {
                | Const(b) => a == b
                | CtxVar(_) => false
                | AsrtVar(_) => false
            }
        }
        | CtxVar(a) => {
            switch b {
                | Const(_) => false
                | CtxVar(b) => a == b
                | AsrtVar(_) => false
            }
        }
        | AsrtVar(a) => {
            switch b {
                | Const(_) => false
                | CtxVar(_) => false
                | AsrtVar(b) => a == b
            }
        }
    }
}

let arrSymEq = (a:array<sym>,b:array<sym>):bool => {
    a->Array.length == b->Array.length
    && a->Array.everyWithIndex((sa,i) => sa->symEq(b->Array.getUnsafe(i)))
}

module SymHash = Belt.Id.MakeHashableU({
    type t = sym
    let hash = var => {
        switch var {
            | Const(i) | CtxVar(i) | AsrtVar(i) => i
        }
    }
    let eq = symEq
})

type unifSubs = Belt_HashMap.t<sym,array<sym>,SymHash.identity>

let rec buildAsrtSyntaxTree = (proofNode:proofNode, ctxIntToAsrtInt:int=>int):result<asrtSyntaxTreeNode,string> => {
    let expr = proofNode->pnGetExpr
    switch proofNode->pnGetProof {
        | None => Error("Cannot build a syntax tree from a node without proof.")
        | Some(AssertionWithErr(_)) => Error("Cannot build a syntax tree from a node with an AssertionWithErr proof.")
        | Some(VarType) | Some(Hypothesis(_)) => {
            let maxI = expr->Array.length - 1
            let children = Expln_utils_common.createArray(maxI)
            for i in 1 to maxI {
                children[i-1] = Symbol(expr->Array.getUnsafe(i)->ctxIntToAsrtInt)
            }
            Ok({typ:expr->Array.getUnsafe(0), children})
        }
        | Some(Assertion({args, frame})) => {
            let children = Expln_utils_common.createArray(frame.asrt->Array.length - 1)
            let err = ref(None)
            frame.asrt->Array.forEachWithIndex((s,i) => {
                if (i > 0 && err.contents->Belt_Option.isNone) {
                    if (s < 0) {
                        children[i-1] = Symbol(s)
                    } else {
                        switch buildAsrtSyntaxTree(args->Array.getUnsafe(frame.varHyps->Array.getUnsafe(s)), ctxIntToAsrtInt) {
                            | Error(msg) => err := Some(Error(msg))
                            | Ok(subtree) => children[i-1] = Subtree(subtree)
                        }
                    }
                }
            })
            switch err.contents {
                | Some(err) => err
                | None => Ok({typ:frame.asrt->Array.getUnsafe(0), children})
            }
        }
    }
}

let isVar = (expr:asrtSyntaxTreeNode):option<int> => {
    @warning("-8")
    switch expr.children->Array.length {
        | 1 => {
            switch expr.children->Array.getUnsafe(0) {
                | Subtree(_) => None
                | Symbol(i) => if (i >= 0) { Some(i) } else { None }
            }
        }
        | _ => None
    }
}

let substituteInPlace = (expr:array<sym>, e:sym, subExpr:array<sym>):unit => {
    let i = ref(0)
    while (i.contents < expr->Array.length) {
        if (expr->Array.getUnsafe(i.contents)->symEq(e)) {
            expr->Array.splice(~start=i.contents, ~remove=1, ~insert=subExpr)
            i := i.contents + subExpr->Array.length
        } else {
            i := i.contents + 1
        }
    }
}

let applySubsInPlace = (expr:array<sym>, subs:unifSubs):unit => {
    subs->Belt_HashMap.forEachU((. v, subExpr) => substituteInPlace(expr, v, subExpr))
}

let assignSubs = (foundSubs:unifSubs, var:sym, expr:array<sym>):bool => {
    if (expr->Array.some(s => s->symEq(var))) {
        false
    } else {
        applySubsInPlace(expr, foundSubs)
        switch foundSubs->Belt_HashMap.get(var) {
            | Some(existingExpr) => arrSymEq(expr, existingExpr)
            | None => {
                foundSubs->Belt_HashMap.set(var, expr)
                foundSubs->Belt_HashMap.forEachU((. _, expr) => applySubsInPlace(expr, foundSubs))
                true
            }
        }
    }
}

let rec getAllSymbols = (syntaxTreeNode:asrtSyntaxTreeNode):array<sym> => {
    syntaxTreeNode.children->Expln_utils_common.arrFlatMap(ch => {
        switch ch {
            | Subtree(syntaxTreeNode) => getAllSymbols(syntaxTreeNode)
            | Symbol(i) => if (i < 0) {[Const(i)]} else {[AsrtVar(i)]}
        }
    })
}

let rec getAllSymbolsCtx = (syntaxTreeNode:MM_syntax_tree.syntaxTreeNode):array<sym> => {
    syntaxTreeNode.children->Expln_utils_common.arrFlatMap(ch => {
        switch ch {
            | Subtree(syntaxTreeNode) => getAllSymbolsCtx(syntaxTreeNode)
            | Symbol({symInt}) => if (symInt < 0) {[Const(symInt)]} else {[CtxVar(symInt)]}
        }
    })
}

let rec unify = ( 
    ~asrtExpr:asrtSyntaxTreeNode, 
    ~ctxExpr:MM_syntax_tree.syntaxTreeNode, 
    ~isMetavar:string=>bool, 
    ~foundSubs:unifSubs, 
    ~continue:ref<bool>
):unit => {
    if (asrtExpr.typ != ctxExpr.typ) {
        continue := false
    } else {
        switch asrtExpr->isVar {
            | Some(asrtVar) => {
                continue := assignSubs(foundSubs, AsrtVar(asrtVar), ctxExpr->getAllSymbolsCtx)
            }
            | None => {
                switch ctxExpr->MM_syntax_tree.isVar(isMetavar) {
                    | Some((ctxVar,_)) => {
                        continue := assignSubs(foundSubs, CtxVar(ctxVar), asrtExpr->getAllSymbols)
                    }
                    | None => {
                        if (asrtExpr.children->Array.length != ctxExpr.children->Array.length) {
                            continue := false
                        } else {
                            let maxI = asrtExpr.children->Array.length-1
                            let i = ref(0)
                            while (continue.contents && i.contents <= maxI) {
                                switch asrtExpr.children->Array.getUnsafe(i.contents) {
                                    | Symbol(asrtSymInt) => {
                                        switch ctxExpr.children->Array.getUnsafe(i.contents) {
                                            | Symbol({symInt:ctxSymInt, isVar:ctxSymIsVar}) => {
                                                if (asrtSymInt >= 0 || ctxSymIsVar || asrtSymInt != ctxSymInt) {
                                                    continue := false
                                                }
                                            }
                                            | Subtree(_) => continue := false
                                        }
                                    }
                                    | Subtree(asrtCh) => {
                                        switch ctxExpr.children->Array.getUnsafe(i.contents) {
                                            | Symbol(_) => continue := false
                                            | Subtree(ctxCh) => {
                                                unify(~asrtExpr=asrtCh, ~ctxExpr=ctxCh, ~isMetavar, ~foundSubs, ~continue)
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

let unifyMayBePossible = ( 
    ~asrtExpr:asrtSyntaxTreeNode, 
    ~ctxExpr:MM_syntax_tree.syntaxTreeNode, 
    ~isMetavar:string=>bool, 
):bool => {
    if (asrtExpr.typ != ctxExpr.typ) {
        false
    } else {
        switch asrtExpr->isVar {
            | Some(_) => true
            | None => {
                switch ctxExpr->MM_syntax_tree.isVar(isMetavar) {
                    | Some(_) => true
                    | None => asrtExpr.children->Array.length == ctxExpr.children->Array.length
                }
            }
        }
    }
}

let unifSubsMake = () => Belt_HashMap.make(~hintSize=16, ~id=module(SymHash))
let unifSubsGet = (unifSubs,sym) => unifSubs->Belt_HashMap.get(sym)
let unifSubsSize = unifSubs => Belt_HashMap.size(unifSubs)