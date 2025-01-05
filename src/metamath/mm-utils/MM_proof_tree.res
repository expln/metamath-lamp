open MM_context
open MM_substitution
open MM_parenCounter
open MM_unification_debug
open MM_statements_dto
open Common

type rootStmt = {
    isHyp: bool,
    label: string,
    expr: expr,
    jstf: option<jstf>,
}

type proofNodeDbg = {
    exprStr:string,
}

type proofTreeDbg = {
    newVars: array<string>,
    exprToStr: expr=>string,
}

type rec proofNode = {
    id: int,
    expr: expr,
    mutable fParents: option<array<exprSrc>>,
    mutable eParents: array<exprSrc>,
    mutable children: array<proofNode>,
    mutable proof: option<exprSrc>,
    mutable isInvalidFloating: bool,
    mutable dist: option<int>,
    pnDbg: option<proofNodeDbg>,
}

and exprSrc =
    | VarType // it is used for new variables only
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame})
    | AssertionWithErr({args:array<proofNode>, frame:frame, err:unifErr})

and proofTree = {
    frms: frms,
    hypsByExpr: Belt_HashMap.t<expr,hypothesis,ExprHash.identity>,
    hypsByLabel: Belt_HashMapString.t<hypothesis>,
    ctxMaxVar:int,
    ctxDisj: disjMutable,
    mutable maxVar:int,
    newVars: Belt_HashSet.t<expr,ExprHash.identity>,
    parenCnt:parenCnt,
    mutable nextNodeId: int,
    nodes: Belt_HashMap.t<expr,proofNode,ExprHash.identity>,
    rootStmts:array<rootStmt>,
    syntaxProofs: Belt_HashMap.t<expr,proofNode,ExprHash.identity>,
    ptDbg: option<proofTreeDbg>,
}

let exprSrcEq = (a:exprSrc,b:exprSrc):bool => {
    switch a {
        | VarType => {
            switch b {
                | VarType => true
                | _ => false
            }
        }
        | Hypothesis({label:aLabel}) => {
            switch b {
                | Hypothesis({label:bLabel}) => aLabel == bLabel
                | _ => false
            }
        }
        | Assertion({ args:aArgs, frame:aFrame, }) => {
            switch b {
                | Assertion({ args:bArgs, frame:bFrame, }) => {
                    aFrame.label == bFrame.label
                    && aArgs->Array.length == bArgs->Array.length
                    && aArgs->Array.everyWithIndex((aArg,idx) => aArg.id == (bArgs->Array.getUnsafe(idx)).id)
                }
                | _ => false
            }
        }
        | AssertionWithErr(_) => false
    }
}

let exprSrcIsProved = (exprSrc:exprSrc): bool => {
    switch exprSrc {
        | VarType | Hypothesis(_) => true
        | Assertion({args}) => args->Array.every(arg => arg.proof->Belt_Option.isSome)
        | AssertionWithErr(_) => false
    }
}

let pnGetId = node => node.id
let pnGetExpr = node => node.expr
let pnGetProof = node => node.proof
let pnGetFParents = node => node.fParents
let pnGetEParents = node => node.eParents
let pnIsInvalidFloating = node => node.isInvalidFloating
let pnSetInvalidFloating = node => node.isInvalidFloating = true
let pnGetDist = node => node.dist
let pnSetDist = (node,dist) => node.dist = Some(dist)
let pnGetDbg = node => node.pnDbg

let ptGetFrms = tree => tree.frms
let ptGetParenCnt = tree => tree.parenCnt
let ptIsDisjInCtx = (tree:proofTree, n, m) => tree.ctxDisj->disjContains(n,m)
let ptIsNewVarDef = (tree:proofTree, expr) => tree.newVars->Belt_HashSet.has(expr)
let ptGetHypByExpr = ( tree:proofTree, expr:expr ):option<hypothesis> => tree.hypsByExpr->Belt_HashMap.get(expr)
let ptGetHypByLabel = ( tree:proofTree, label:string ):option<hypothesis> => 
    tree.hypsByLabel->Belt_HashMapString.get(label)
let ptGetMaxVar = tree => tree.maxVar
let ptGetCtxMaxVar = tree => tree.ctxMaxVar
let ptGetRootStmts = tree => tree.rootStmts
let ptGetDbg = (tree:proofTree) => tree.ptDbg
let ptGetCopyOfNewVars = tree => tree.newVars->Belt_HashSet.toArray
let ptGetCtxDisj = tree => tree.ctxDisj

let ptMake = (
    ~frms: frms,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~ctxMaxVar: int,
    ~ctxDisj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => {
    let hypsArr = hyps->Belt_MapString.toArray
    {
        frms: frms,
        hypsByLabel: hypsArr->Belt_HashMapString.fromArray,
        hypsByExpr: hypsArr
                    ->Array.map(((_,hyp)) => (hyp.expr, hyp))
                    ->Belt_HashMap.fromArray(~id=module(ExprHash)),
        ctxMaxVar,
        maxVar:ctxMaxVar,
        newVars: Belt_HashSet.make(~id=module(ExprHash), ~hintSize=16),
        ctxDisj,
        parenCnt,
        nextNodeId: 0,
        nodes: Belt_HashMap.make(~id=module(ExprHash), ~hintSize=128),
        rootStmts: [],
        syntaxProofs: Belt_HashMap.make(~id=module(ExprHash), ~hintSize=128),
        ptDbg: exprToStr->Belt_Option.map(exprToStr => {
            {
                newVars: [],
                exprToStr,
            }
        })
    }
}

let pnGetExprStr = (node:proofNode):string => {
    switch node.pnDbg {
        | Some({exprStr}) => exprStr
        | None => node.expr->Array.map(Belt_Int.toString(_))->Array.joinUnsafe(" ")
    }
}

let ptGetNode = ( tree:proofTree, expr:expr):proofNode => {
    switch tree.nodes->Belt_HashMap.get(expr) {
        | Some(node) => node
        | None => {
            let node = {
                id: tree.nextNodeId,
                expr,
                fParents: None,
                eParents: [],
                proof: None,
                children: [],
                isInvalidFloating: false,
                dist: None,
                pnDbg: tree.ptDbg->Belt_Option.map(dbg => {
                    {
                        exprStr: dbg.exprToStr(expr),
                    }
                })
            }
            tree.nodes->Belt_HashMap.set(expr, node)->ignore
            tree.nextNodeId = tree.nextNodeId + 1
            node
        }
    }
}

let ptAddRootStmt = (tree, stmt:rootStmt) => {
    switch tree.rootStmts->Array.find(existingStmt => existingStmt.expr->exprEq(stmt.expr)) {
        | Some(_) => ()
        | None => tree.rootStmts->Array.push(stmt)
    }
}

let ptAddSyntaxProof = (tree, expr:expr, syntaxProof:proofNode):unit => {
    tree.syntaxProofs->Belt_HashMap.set(expr,syntaxProof)
}

let ptGetSyntaxProof = (tree, expr):option<proofNode> => {
    tree.syntaxProofs->Belt_HashMap.get(expr)
}

let ptGetAllSyntaxProofs = (tree):array<(expr,proofNode)> => {
    tree.syntaxProofs->Belt_HashMap.toArray
}

let ptClearDists = tree => {
    tree.nodes->Belt_HashMap.forEach((_,node) => node.dist = None)
}

let pnGetProofFromParents = (node):option<exprSrc> => {
    let fProof = switch node.fParents {
        | None => None
        | Some(fParents) => fParents->Array.find(exprSrcIsProved)
    }
    if (fProof->Belt_Option.isSome) {
        fProof
    } else {
        node.eParents->Array.find(exprSrcIsProved)
    }
}

let pnMarkProved = ( node:proofNode ):unit => {
    if (node.proof->Belt_Option.isNone) {
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
                                curNode.children->Array.forEach( Belt_MutableQueue.add(nodesToMarkProved, _) )
                            }
                        }
                    }
                }
            }
        }
    }
}

let pnAddChild = (node, child): unit => {
    if (node.id != child.id) {
        switch node.children->Array.find(existingChild => existingChild.id == child.id) {
            | None => node.children->Array.push(child)
            | Some(_) => ()
        }
    }
}

let pnAddParent = (node:proofNode, parent:exprSrc, isEssential:bool, forceAdd:bool):unit => {
    if (node.proof->Belt.Option.isNone || forceAdd) {
        let newParentWasAdded = ref(false)
        let parents = if (isEssential) {
            node.eParents
        } else {
            switch node.fParents {
                | None => {
                    let parents = [parent]
                    node.fParents = Some(parents)
                    newParentWasAdded.contents = true
                    parents
                }
                | Some(parents) => parents
            }
        }
        if (!newParentWasAdded.contents) {
            switch parents->Array.find(par => exprSrcEq(par, parent)) {
                | Some(existingParent) => {
                    if (exprSrcIsProved(existingParent)) {
                        raise(MmException({
                            msg:`Unexpected: an unproved node '${pnGetExprStr(node)}' has a proved parent.`
                        }))
                    }
                }
                | None => {
                    parents->Array.push(parent)
                    newParentWasAdded.contents = true
                }
            }
        }
        if (newParentWasAdded.contents) {
            switch parent {
                | Assertion({args}) => args->Array.forEach(pnAddChild(_, node))
                | VarType | Hypothesis(_) | AssertionWithErr(_) => ()
            }
            if (exprSrcIsProved(parent)) {
                pnMarkProved(node)
                if (forceAdd) {
                    node.proof = Some(parent)
                }
            }
        }
    }
}

let ptAddNewVar = (tree, typ):int => {
    tree.maxVar = tree.maxVar + 1
    let newVar = tree.maxVar
    tree.newVars->Belt_HashSet.add([typ, newVar])
    switch tree.ptDbg {
        | None => ()
        | Some({exprToStr, newVars}) => newVars->Array.push(exprToStr([typ, newVar]))
    }
    newVar
}

let jstfEqSrc = (jstfArgs:array<expr>, jstfLabel:string, src:exprSrc):bool => {
    switch src {
        | VarType | Hypothesis(_) | AssertionWithErr(_) => false
        | Assertion({args:srcArgs, frame}) => {
            if (jstfLabel != frame.label) {
                false
            } else {
                let jLen = jstfArgs->Array.length
                let hLen = frame.hyps->Array.length
                if (jLen > hLen) {
                    false
                } else {
                    let ji = ref(0)
                    let hi = ref(0)
                    let eq = ref(true)
                    while (eq.contents && ji.contents < jLen && hi.contents < hLen) {
                        let hyp = frame.hyps->Array.getUnsafe(hi.contents)
                        if (hyp.typ == F) {
                            hi := hi.contents + 1
                        } else {
                            eq := jstfArgs->Array.getUnsafe(ji.contents)->exprEq(srcArgs->Array.getUnsafe(hi.contents)->pnGetExpr)
                            ji := ji.contents + 1
                            hi := hi.contents + 1
                        }
                    }
                    while (eq.contents && hi.contents < hLen) {
                        if ((frame.hyps->Array.getUnsafe(hi.contents)).typ == F) {
                            hi := hi.contents + 1
                        } else {
                            eq := false
                        }
                    }
                    eq.contents && ji.contents == jLen && hi.contents == hLen
                }
            }
        }
    }
}

let ptPrintStats = ( tree:proofTree ):string => {
    let nodes = tree.nodes->Belt_HashMap.toArray->Array.map(((_,node)) => node)
    let nodeCnt = nodes->Array.length
    let nodeCntFl = nodeCnt->Belt.Int.toFloat
    Console.log2(`nodeCnt`, nodeCnt)
    let provedNodeCnt = nodes->Array.filter(node => node.proof->Belt_Option.isSome)->Array.length
    Console.log3(`provedNodeCnt`, provedNodeCnt, Common.floatToPctStr(provedNodeCnt->Belt_Int.toFloat /. nodeCntFl))
    let invalidFloatingCnt = nodes->Array.filter(node => node.isInvalidFloating)->Array.length
    Console.log3(`invalidFloatingCnt`, invalidFloatingCnt, Common.floatToPctStr(invalidFloatingCnt->Belt_Int.toFloat /. nodeCntFl))
    switch tree.ptDbg {
        | None => "Debug is off"
        | Some(dbg) => {
            let unprovedNodes = nodes
                ->Array.filter(node => 
                    node.proof->Belt_Option.isNone && !node.isInvalidFloating
                    // && node.fParents->Belt_Option.mapWithDefault(0, fParents => fParents->Array.length) > 0
                )
            unprovedNodes->Expln_utils_common.sortInPlaceWith((a,b) =>
                Belt_Float.fromInt(a.expr->Array.length - b.expr->Array.length)
            )->ignore
            let unprovedExprs = unprovedNodes->Array.map(node => node.expr)
            unprovedExprs->Array.map(dbg.exprToStr)->Array.joinUnsafe("\n")
        }
    }
}