open MM_parser
open MM_context
open MM_substitution
open MM_parenCounter
open MM_unification_debug
open MM_statements_dto

type rootStmt = {
    isHyp: bool,
    label: string,
    expr: expr,
    jstf: option<jstf>,
}

type rec proofNode = {
    expr:expr,
    exprStr:option<string>, //for debug purposes
    mutable parents: option<array<exprSource>>,
    mutable children: array<proofNode>,
    mutable proof: option<exprSource>,
    mutable isInvalidFloating: bool,
    mutable dist: option<int>,
}

and exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame, err:option<unifErr>})

and proofTree = {
    frms: Belt_MapString.t<frmSubsData>,
    hypsByExpr: Belt_HashMap.t<expr,hypothesis,ExprHash.identity>,
    hypsByLabel: Belt_HashMapString.t<hypothesis>,
    ctxMaxVar:int,
    mutable maxVar:int,
    newVars: Belt_HashSet.t<expr,ExprHash.identity>,
    dbgNewVars: array<string>,
    disj: disjMutable,
    dbgDisj: array<string>,
    parenCnt:parenCnt,
    nodes: Belt_HashMap.t<expr,proofNode,ExprHash.identity>,
    rootStmts:array<rootStmt>,
    exprToStr: option<expr=>string>, //for debug purposes
}

let jstfEq = (jstf1, jstf2) => {
    jstf1.args == jstf2.args && jstf1.label == jstf2.label
}

let exprSourceEq = (s1,s2) => {
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
        | Assertion({ args:args1, frame:frame1, }) => {
            switch s2 {
                | Assertion({ args:args2, frame:frame2, }) => {
                    frame1.label == frame2.label
                    && args1->Js.Array2.length == args2->Js.Array2.length
                    && args1->Js.Array2.everyi((arg1,idx) => exprEq(arg1.expr, args2[idx].expr))
                }
                | _ => false
            }
        }
    }
}

let ptGetFrms = tree => tree.frms
let ptGetParenCnt = tree => tree.parenCnt
let ptIsDisj = (tree:proofTree, n, m) => tree.disj->disjContains(n,m)
let ptIsNewVarDef = (tree:proofTree, expr) => tree.newVars->Belt_HashSet.has(expr)

let ptMake = (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~ctxMaxVar: int,
    ~disj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => {
    {
        frms,
        hypsByLabel: hyps->Belt_MapString.toArray->Belt_HashMapString.fromArray,
        hypsByExpr: hyps
                        ->Belt_MapString.toArray
                        ->Js_array2.map(((_,hyp)) => (hyp.expr, hyp))
                        ->Belt_HashMap.fromArray(~id=module(ExprHash)),
        ctxMaxVar,
        maxVar:ctxMaxVar,
        newVars: Belt_HashSet.make(~id=module(ExprHash), ~hintSize=16),
        dbgNewVars: [],
        disj,
        dbgDisj: [],
        parenCnt,
        nodes: Belt_HashMap.make(~id=module(ExprHash), ~hintSize=16),
        rootStmts: [],
        exprToStr,
    }
}

let ptGetHypByExpr = ( tree:proofTree, expr:expr ):option<hypothesis> => {
    tree.hypsByExpr->Belt_HashMap.get(expr)
}

let ptGetHypByLabel = ( tree:proofTree, label:string ):option<hypothesis> => {
    tree.hypsByLabel->Belt_HashMapString.get(label)
}

let ptGetMaxVar = tree => tree.maxVar

let ptGetCtxMaxVar = tree => tree.ctxMaxVar

let proofNodeGetExprStr = (node:proofNode):string => {
    switch node.exprStr {
        | Some(str) => str
        | None => node.expr->Js_array2.map(Belt_Int.toString)->Js.Array2.joinWith(" ")
    }
}

let ptMakeNode = ( tree:proofTree, expr:expr, ):proofNode => {
    switch tree.nodes->Belt_HashMap.get(expr) {
        | Some(existingNode) => 
            raise(MmException({
                msg:`Creation of a new node was requested, ` 
                    ++ `but a node with the same expression already exists '${existingNode->proofNodeGetExprStr}'.`
            }))
        | None => {
            let node = {
                expr,
                exprStr: tree.exprToStr->Belt.Option.map(f => f(expr)),
                parents: None,
                proof: None,
                children: [],
                isInvalidFloating: false,
                dist: None,
            }
            tree.nodes->Belt_HashMap.set(expr, node)->ignore
            node
        }
    }
}

let ptGetNuberOfNodes = (tree:proofTree) => tree.nodes->Belt_HashMap.size

let ptGetRootStmts = tree => {
    tree.rootStmts
}

let ptGetNodeByExpr = ( tree:proofTree, expr:expr ):option<proofNode> => {
    tree.nodes->Belt_HashMap.get(expr)
}

let ptGetOrCreateNode = ( tree:proofTree, expr:expr):proofNode => {
    switch tree->ptGetNodeByExpr(expr) {
        | Some(node) => node
        | None => tree->ptMakeNode(expr)
    }
}

let ptAddRootStmt = (tree, stmt:rootStmt) => {
    switch tree.rootStmts->Js_array2.find(existingStmt => existingStmt.expr->exprEq(stmt.expr)) {
        | Some(_) => ()
        | None => tree.rootStmts->Js_array2.push(stmt)->ignore
    }
}

let ptClearDists = tree => {
    tree.nodes->Belt_HashMap.forEach((_,node) => node.dist = None)
}

let esIsProved = (exprSrc:exprSource): bool => {
    switch exprSrc {
        | VarType | Hypothesis(_) => true
        | Assertion({args}) => args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)
    }
}

let pnGetProofFromParents = (node):option<exprSource> => {
    switch node.parents {
        | None => None
        | Some(parents) => parents->Js_array2.find(esIsProved)
    }
}

let pnMarkProved = ( node:proofNode ):unit => {
    switch node.proof {
        | Some(_) => ()
        | None => {
            switch pnGetProofFromParents(node) {
                | None => ()
                | Some(nodeProof) => {
                    node.proof = Some(nodeProof)
                    let nodesToMarkProved = node.children->Belt_MutableQueue.fromArray
                    while (!(nodesToMarkProved->Belt_MutableQueue.isEmpty)) {
                        let curNode = nodesToMarkProved->Belt_MutableQueue.pop->Belt_Option.getExn
                        if (curNode.proof->Belt_Option.isNone) {
                            switch pnGetProofFromParents(curNode) {
                                | None => ()
                                | Some(curNodeProof) => {
                                    curNode.proof = Some(curNodeProof)
                                    curNode.children->Js_array2.forEach( nodesToMarkProved->Belt_MutableQueue.add )
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let pnAddChild = (node, child): unit => {
    if (!exprEq(node.expr, child.expr)) {
        switch node.children->Js.Array2.find(existingChild => exprEq(existingChild.expr,child.expr)) {
            | None => node.children->Js_array2.push(child)->ignore
            | Some(_) => ()
        }
    }
}

let pnGetExpr = node => node.expr
let pnGetExprStr = node => node.exprStr
let pnGetProof = node => node.proof
let pnGetParents = node => node.parents

let pnAddParent = (node:proofNode, parent:exprSource):unit => {
    switch node.proof {
        | Some(_) => ()
        | None => {
            let newParentWasAdded = ref(false)
            switch node.parents {
                | None => {
                    node.parents = Some([parent])
                    newParentWasAdded.contents = true
                }
                | Some(parents) => {
                    switch parents->Js_array2.find(par => exprSourceEq(par, parent)) {
                        | Some(existingParent) => {
                            if (esIsProved(existingParent)) {
                                raise(MmException({
                                    msg:`Unexpected: an unproved node '${proofNodeGetExprStr(node)}' has a proved parent.`
                                }))
                            }
                        }
                        | None => {
                            parents->Js_array2.push(parent)->ignore
                            newParentWasAdded.contents = true
                        }
                    }
                }
            }
            if (newParentWasAdded.contents) {
                switch parent {
                    | VarType | Hypothesis(_) => ()
                    | Assertion({args}) => args->Js_array2.forEach(pnAddChild(_, node))
                }
                if (esIsProved(parent)) {
                    pnMarkProved(node)
                }
            }
        }
    }
}

let pnSetInvalidFloating = (node,isInvalidFloating) => {
    node.isInvalidFloating = isInvalidFloating
}

let pnGetDist = node => node.dist

let pnSetDist = (node,dist) => node.dist = Some(dist)

let pnIsInvalidFloating = node => node.isInvalidFloating

let ptAddNewVar = (tree, typ):int => {
    tree.maxVar = tree.maxVar + 1
    let newVar = tree.maxVar
    tree.newVars->Belt_HashSet.add([typ, newVar])
    switch tree.exprToStr {
        | None => ()
        | Some(exprToStr) => tree.dbgNewVars->Js.Array2.push(exprToStr([typ, newVar]))->ignore
    }
    newVar
}

let ptAddDisjPair = (tree, n, m) => {
    tree.disj->disjAddPair( n,m )
    switch tree.exprToStr {
        | None => ()
        | Some(exprToStr) => tree.dbgDisj->Js.Array2.push(exprToStr([n,m]))->ignore
    }
}

let ptGetCopyOfNewVars = tree => tree.newVars->Belt_HashSet.toArray

let ptGetCopyOfDisj = tree => {
    let disj = disjMutableMake()
    tree.disj->disjForEach(disj->disjAddPair)
    disj
}

let ptGetExprToStr = tree => tree.exprToStr