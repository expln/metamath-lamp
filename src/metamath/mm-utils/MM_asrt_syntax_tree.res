open MM_context
open MM_proof_tree

type rec asrtSyntaxTreeNode = {
    typ:int,
    children:array<asrtChildNode>,
}
and asrtChildNode =
    | Subtree(asrtSyntaxTreeNode)
    | Symbol(int)

let rec buildAsrtSyntaxTree = (proofNode:proofNode, ctxIntToAsrtInt:int=>int):result<asrtSyntaxTreeNode,string> => {
    let expr = proofNode->pnGetExpr
    switch proofNode->pnGetProof {
        | None => Error("Cannot build a syntax tree from a node without proof.")
        | Some(AssertionWithErr(_)) => Error("Cannot build a syntax tree from a node with an AssertionWithErr proof.")
        | Some(VarType) | Some(Hypothesis(_)) => {
            let maxI = expr->Js_array2.length - 1
            let children = Expln_utils_common.createArray(maxI)
            for i in 1 to maxI {
                children[i-1] = Symbol(expr[i]->ctxIntToAsrtInt)
            }
            Ok({typ:expr[0], children})
        }
        | Some(Assertion({args, frame})) => {
            let children = Expln_utils_common.createArray(frame.asrt->Js_array2.length - 1)
            let err = ref(None)
            frame.asrt->Js_array2.forEachi((s,i) => {
                if (i > 0 && err.contents->Belt_Option.isNone) {
                    if (s < 0) {
                        children[i-1] = Symbol(s)
                    } else {
                        switch buildAsrtSyntaxTree(args[frame.varHyps[s]], ctxIntToAsrtInt) {
                            | Error(msg) => err := Some(Error(msg))
                            | Ok(subtree) => children[i-1] = Subtree(subtree)
                        }
                    }
                }
            })
            switch err.contents {
                | Some(err) => err
                | None => Ok({typ:frame.asrt[0], children})
            }
        }
    }
}