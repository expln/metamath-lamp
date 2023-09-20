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

type proofNodeDbg = {
    exprStr:string,
}

type proofTreeDbg = {
    newVars: array<string>,
    disj: array<string>,
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
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofNode>, frame:frame})
    | AssertionWithErr({args:array<proofNode>, frame:frame, err:unifErr})

and proofTree = {
    frms: Belt_HashMapInt.t<array<frmSubsData>>,
    hypsByExpr: Belt_HashMap.t<expr,hypothesis,ExprHash.identity>,
    hypsByLabel: Belt_HashMapString.t<hypothesis>,
    ctxMaxVar:int,
    mutable maxVar:int,
    newVars: Belt_HashSet.t<expr,ExprHash.identity>,
    disj: disjMutable,
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
                    && aArgs->Js.Array2.length == bArgs->Js.Array2.length
                    && aArgs->Js.Array2.everyi((aArg,idx) => aArg.id == bArgs[idx].id)
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
        | Assertion({args}) => args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)
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

let emptyFrmArr = []
let ptGetFrms = (tree,typ) => tree.frms->Belt_HashMapInt.get(typ)->Belt.Option.getWithDefault(emptyFrmArr)
let ptGetParenCnt = tree => tree.parenCnt
let ptIsDisj = (tree:proofTree, n, m) => tree.disj->disjContains(n,m)
let ptIsNewVarDef = (tree:proofTree, expr) => tree.newVars->Belt_HashSet.has(expr)
let ptGetHypByExpr = ( tree:proofTree, expr:expr ):option<hypothesis> => tree.hypsByExpr->Belt_HashMap.get(expr)
let ptGetHypByLabel = ( tree:proofTree, label:string ):option<hypothesis> => 
    tree.hypsByLabel->Belt_HashMapString.get(label)
let ptGetMaxVar = tree => tree.maxVar
let ptGetCtxMaxVar = tree => tree.ctxMaxVar
let ptGetRootStmts = tree => tree.rootStmts
let ptGetDbg = (tree:proofTree) => tree.ptDbg
let ptGetCopyOfNewVars = tree => tree.newVars->Belt_HashSet.toArray
let ptGetDisj = tree => tree.disj

let ptMake = (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~ctxMaxVar: int,
    ~disj: disjMutable,
    ~parenCnt: parenCnt,
    ~exprToStr: option<expr=>string>,
) => {
    let hypsArr = hyps->Belt_MapString.toArray
    {
        frms: frms->Belt_MapString.toArray->Js.Array2.reduce(
            (map, (_,frm)) => {
                let typ = frm.frame.asrt[0]
                switch map->Belt_HashMapInt.get(typ) {
                    | None => map->Belt_HashMapInt.set(typ, [frm])
                    | Some(frms) => frms->Js_array2.push(frm)->ignore
                }
                map
            },
            Belt_HashMapInt.make(~hintSize=4)
        ),
        hypsByLabel: hypsArr->Belt_HashMapString.fromArray,
        hypsByExpr: hypsArr
                    ->Js_array2.map(((_,hyp)) => (hyp.expr, hyp))
                    ->Belt_HashMap.fromArray(~id=module(ExprHash)),
        ctxMaxVar,
        maxVar:ctxMaxVar,
        newVars: Belt_HashSet.make(~id=module(ExprHash), ~hintSize=16),
        disj,
        parenCnt,
        nextNodeId: 0,
        nodes: Belt_HashMap.make(~id=module(ExprHash), ~hintSize=500_000),
        rootStmts: [],
        syntaxProofs: Belt_HashMap.make(~id=module(ExprHash), ~hintSize=50_000),
        ptDbg: exprToStr->Belt_Option.map(exprToStr => {
            {
                newVars: [],
                disj: [],
                exprToStr,
            }
        })
    }
}

let pnGetExprStr = (node:proofNode):string => {
    switch node.pnDbg {
        | Some({exprStr}) => exprStr
        | None => node.expr->Js_array2.map(Belt_Int.toString)->Js.Array2.joinWith(" ")
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
    switch tree.rootStmts->Js_array2.find(existingStmt => existingStmt.expr->exprEq(stmt.expr)) {
        | Some(_) => ()
        | None => tree.rootStmts->Js_array2.push(stmt)->ignore
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
        | Some(fParents) => fParents->Js_array2.find(exprSrcIsProved)
    }
    if (fProof->Belt_Option.isSome) {
        fProof
    } else {
        node.eParents->Js_array2.find(exprSrcIsProved)
    }
}

let pnMarkProved = ( node:proofNode ):unit => {
    if (node.proof->Belt_Option.isNone) {
        switch pnGetProofFromParents(node) {
            | None => ()
            | Some(nodeProof) => {
                node.proof = Some(nodeProof)
                // node->pnDecIsNeededCntForFParents
                let nodesToMarkProved = node.children->Belt_MutableQueue.fromArray
                while (!(nodesToMarkProved->Belt_MutableQueue.isEmpty)) {
                    let curNode = nodesToMarkProved->Belt_MutableQueue.pop->Belt_Option.getExn
                    if (curNode.proof->Belt_Option.isNone) {
                        switch pnGetProofFromParents(curNode) {
                            | None => ()
                            | Some(curNodeProof) => {
                                curNode.proof = Some(curNodeProof)
                                curNode.children->Js_array2.forEach( nodesToMarkProved->Belt_MutableQueue.add )
                                // curNode->pnDecIsNeededCntForFParents
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
        switch node.children->Js.Array2.find(existingChild => existingChild.id  == child.id) {
            | None => node.children->Js_array2.push(child)->ignore
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
            switch parents->Js_array2.find(par => exprSrcEq(par, parent)) {
                | Some(existingParent) => {
                    if (exprSrcIsProved(existingParent)) {
                        raise(MmException({
                            msg:`Unexpected: an unproved node '${pnGetExprStr(node)}' has a proved parent.`
                        }))
                    }
                }
                | None => {
                    parents->Js_array2.push(parent)->ignore
                    newParentWasAdded.contents = true
                }
            }
        }
        if (newParentWasAdded.contents) {
            switch parent {
                | Assertion({args}) => args->Js_array2.forEach(pnAddChild(_, node))
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
        | Some({exprToStr, newVars}) => newVars->Js.Array2.push(exprToStr([typ, newVar]))->ignore
    }
    newVar
}

let ptAddDisjPair = (tree, n, m) => {
    tree.disj->disjAddPair( n,m )
    switch tree.ptDbg {
        | None => ()
        | Some({exprToStr, disj}) => disj->Js.Array2.push(exprToStr([n,m]))->ignore
    }
}

let jstfEqSrc = (jstfArgs:array<expr>, jstfLabel:string, src:exprSrc):bool => {
    switch src {
        | VarType | Hypothesis(_) | AssertionWithErr(_) => false
        | Assertion({args:srcArgs, frame}) => {
            if (jstfLabel != frame.label) {
                false
            } else {
                let jLen = jstfArgs->Js.Array2.length
                let hLen = frame.hyps->Js.Array2.length
                if (jLen > hLen) {
                    false
                } else {
                    let ji = ref(0)
                    let hi = ref(0)
                    let eq = ref(true)
                    while (eq.contents && ji.contents < jLen && hi.contents < hLen) {
                        let hyp = frame.hyps[hi.contents]
                        if (hyp.typ == F) {
                            hi := hi.contents + 1
                        } else {
                            eq := jstfArgs[ji.contents]->exprEq(srcArgs[hi.contents]->pnGetExpr)
                            ji := ji.contents + 1
                            hi := hi.contents + 1
                        }
                    }
                    while (eq.contents && hi.contents < hLen) {
                        if (frame.hyps[hi.contents].typ == F) {
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
    let nodes = tree.nodes->Belt_HashMap.toArray->Js.Array2.map(((_,node)) => node)
    let nodeCnt = nodes->Js.Array2.length
    let nodeCntFl = nodeCnt->Belt.Int.toFloat
    Js.Console.log2(`nodeCnt`, nodeCnt)
    let provedNodeCnt = nodes->Js.Array2.filter(node => node.proof->Belt_Option.isSome)->Js.Array2.length
    Js.Console.log3(`provedNodeCnt`, provedNodeCnt, Common.floatToPctStr(provedNodeCnt->Belt_Int.toFloat /. nodeCntFl))
    let invalidFloatingCnt = nodes->Js.Array2.filter(node => node.isInvalidFloating)->Js.Array2.length
    Js.Console.log3(`invalidFloatingCnt`, invalidFloatingCnt, Common.floatToPctStr(invalidFloatingCnt->Belt_Int.toFloat /. nodeCntFl))
    switch tree.ptDbg {
        | None => "Debug is off"
        | Some(dbg) => {
            let unprovedNodes = nodes
                ->Js.Array2.filter(node => 
                    node.proof->Belt_Option.isNone && !node.isInvalidFloating
                    // && node.fParents->Belt_Option.mapWithDefault(0, fParents => fParents->Js_array2.length) > 0
                )
            unprovedNodes->Js.Array2.sortInPlaceWith((a,b) => 
                a.expr->Js_array2.length - b.expr->Js_array2.length
            )->ignore
            let unprovedExprs = unprovedNodes->Js.Array2.map(node => node.expr)
            unprovedExprs->Js.Array2.map(dbg.exprToStr)->Js.Array2.joinWith("\n")
        }
    }
}