open MM_context
open MM_proof_tree

let rec buildSyntaxTreeInner = (
    ~proofNode:proofNode,
    ~ctxIntToAsrtInt:int=>int,
    ~asrtIntToSym:int=>string,
    ~asrtVarToHypLabel:int=>string,
    ~idSeq:unit=>int,
):result<MM_syntax_tree.syntaxTreeNode,string> => {
    let expr = proofNode->pnGetExpr
    switch proofNode->pnGetProof {
        | None => Error("Cannot build a syntax tree from a node without proof.")
        | Some(AssertionWithErr(_)) => Error("Cannot build a syntax tree from a node with an AssertionWithErr proof.")
        | Some(Hypothesis({label})) => {
            Ok(makeSyntaxTreeNodeForVarTypeAndHyp( ~expr, ~label, ~idSeq, ~ctxIntToAsrtInt, ~asrtIntToSym, ))
        }
        | Some(VarType) => {
            let label = expr->Array.getUnsafe(1)->ctxIntToAsrtInt->asrtVarToHypLabel
            Ok(makeSyntaxTreeNodeForVarTypeAndHyp( ~expr, ~label, ~idSeq, ~ctxIntToAsrtInt, ~asrtIntToSym, ))
        }
        | Some(Assertion({args, frame})) => {
            let this:MM_syntax_tree.syntaxTreeNode = {
                id: idSeq(),
                typ:frame.asrt->Array.getUnsafe(0),
                label:frame.label,
                children: Expln_utils_common.createArray(frame.asrt->Array.length - 1),
                height:-1,
            }
            let err = ref(None)
            frame.asrt->Array.forEachWithIndex((s,i) => {
                if (i > 0 && err.contents->Belt_Option.isNone) {
                    if (s < 0) {
                        let symInt = s->ctxIntToAsrtInt
                        this.children[i-1] = Symbol({
                            id: idSeq(),
                            symInt,
                            sym: symInt->asrtIntToSym,
                            isVar: false,
                            color: None,
                        })
                    } else {
                        switch buildSyntaxTreeInner(
                            ~proofNode=args->Array.getUnsafe(frame.varHyps->Array.getUnsafe(s)),
                            ~ctxIntToAsrtInt,
                            ~asrtIntToSym,
                            ~asrtVarToHypLabel,
                            ~idSeq,
                        ) {
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
and let makeSyntaxTreeNodeForVarTypeAndHyp = (
    ~expr:expr,
    ~label:string,
    ~idSeq:unit=>int,
    ~ctxIntToAsrtInt:int=>int,
    ~asrtIntToSym:int=>string,
):MM_syntax_tree.syntaxTreeNode => {
    let maxI = expr->Array.length - 1
    let this:MM_syntax_tree.syntaxTreeNode = {
        id: idSeq(),
        typ:expr->Array.getUnsafe(0),
        label,
        children: Expln_utils_common.createArray(maxI),
        height:-1,
    }
    for i in 1 to maxI {
        let symInt = expr->Array.getUnsafe(i)->ctxIntToAsrtInt
        this.children[i-1] = Symbol({
            id: idSeq(),
            symInt,
            sym: symInt->asrtIntToSym,
            isVar: symInt >= 0,
            color: None,
        })
    }
    this
}

let buildSyntaxTree = (
    ~proofNode:proofNode,
    ~ctxIntToAsrtInt:int=>int,
    ~asrtIntToSym:int=>string,
    ~asrtVarToHypLabel:int=>string,
):result<MM_syntax_tree.syntaxTreeNode,string> => {
    let lastId = ref(-1)
    let idSeq = () => {
        lastId := lastId.contents + 1
        lastId.contents
    }
    buildSyntaxTreeInner(~proofNode, ~ctxIntToAsrtInt, ~asrtIntToSym, ~asrtVarToHypLabel, ~idSeq, )
}

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

let unifSubsMake = () => Belt_HashMap.make(~hintSize=16, ~id=module(SymHash))
let unifSubsGet = (unifSubs,sym) => unifSubs->Belt_HashMap.get(sym)
let unifSubsSize = unifSubs => unifSubs->Belt_HashMap.size

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
    subs->Belt_HashMap.forEachU((v, subExpr) => substituteInPlace(expr, v, subExpr))
}

let assignSubs = (foundSubs:unifSubs, var:sym, expr:array<sym>):bool => {
    if (expr->Array.some(symEq(_, var))) {
        false
    } else {
        applySubsInPlace(expr, foundSubs)
        switch foundSubs->Belt_HashMap.get(var) {
            | Some(existingExpr) => arrSymEq(expr, existingExpr)
            | None => {
                foundSubs->Belt_HashMap.set(var, expr)
                foundSubs->Belt_HashMap.forEachU((_, expr) => applySubsInPlace(expr, foundSubs))
                true
            }
        }
    }
}

let rec getAllSymbols = (syntaxTreeNode:MM_syntax_tree.syntaxTreeNode, ~isAsrt:bool):array<sym> => {
    syntaxTreeNode.children->Expln_utils_common.arrFlatMap(ch => {
        switch ch {
            | Subtree(syntaxTreeNode) => getAllSymbols(syntaxTreeNode, ~isAsrt)
            | Symbol({symInt}) => if (symInt < 0) {[Const(symInt)]} else {[isAsrt ? AsrtVar(symInt) : CtxVar(symInt)]}
        }
    })
}

/*
    The core idea of the unification algorithm is as per explanations by Mario Carneiro.
    https://github.com/expln/metamath-lamp/issues/77#issuecomment-1577804381
*/
let rec unify = ( 
    ~asrtExpr:MM_syntax_tree.syntaxTreeNode,
    ~ctxExpr:MM_syntax_tree.syntaxTreeNode,
    ~isMetavar:string=>bool,
    ~foundSubs:unifSubs,
    ~continue:ref<bool>
):unit => {
    if (asrtExpr.typ != ctxExpr.typ) {
        continue := false
    } else {
        switch asrtExpr->MM_syntax_tree.isVar(_=>true) {
            | Some((asrtVar,_)) => {
                continue := assignSubs(foundSubs, AsrtVar(asrtVar), ctxExpr->getAllSymbols(~isAsrt=false))
            }
            | None => {
                switch ctxExpr->MM_syntax_tree.isVar(isMetavar) {
                    | Some((ctxVar,_)) => {
                        continue := assignSubs(foundSubs, CtxVar(ctxVar), asrtExpr->getAllSymbols(~isAsrt=true))
                    }
                    | None => {
                        if (asrtExpr.children->Array.length != ctxExpr.children->Array.length) {
                            continue := false
                        } else {
                            let maxI = asrtExpr.children->Array.length-1
                            let i = ref(0)
                            while (continue.contents && i.contents <= maxI) {
                                switch asrtExpr.children->Array.getUnsafe(i.contents) {
                                    | Symbol({symInt:asrtSymInt}) => {
                                        switch ctxExpr.children->Array.getUnsafe(i.contents) {
                                            | Symbol({symInt:ctxSymInt}) => continue := asrtSymInt == ctxSymInt
                                            | Subtree(_) => continue := false
                                        }
                                    }
                                    | Subtree(asrtCh) => {
                                        switch ctxExpr.children->Array.getUnsafe(i.contents) {
                                            | Symbol(_) => continue := false
                                            | Subtree(ctxCh) => {
                                                unify(
                                                    ~asrtExpr=asrtCh, ~ctxExpr=ctxCh, ~isMetavar, ~foundSubs, ~continue
                                                )
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
