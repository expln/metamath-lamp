open MM_context
open Expln_utils_promise
open MM_wrk_ctx_proc
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_statements_dto
open MM_wrk_settings
open MM_proof_verifier
open Common

let procName = "MM_wrk_unify"

type request = 
    | Unify({
        rootStmts: array<rootStmt>, 
        bottomUpProverParams:option<bottomUpProverParams>,
        allowedFrms:allowedFrms,
        combCntMax:int,
        syntaxTypes:option<array<int>>,
        exprsToSyntaxCheck:option<array<expr>>,
        debugLevel:int,
    })

type response =
    | OnProgress(string)
    | Result(proofTreeDto)

let bottomUpProverParamsToStr = (params:option<bottomUpProverParams>):string => {
    switch params {
        | None => "None"
        | Some(params) => {
            params.frameParams->Array.map(p => {
                `{` 
                    ++ `allowNewDisjForExistingVars=${if (p.allowNewDisjForExistingVars) {"true"} else {"false"}}` 
                    ++ `}`
            })->Array.joinUnsafe("; ")
        }
    }
}

let reqToStr = req => {
    switch req {
        | Unify({bottomUpProverParams,debugLevel}) => 
            `Unify(debugLevel=${debugLevel->Belt_Int.toString}` 
                ++ `, bottomUpProverParams=${bottomUpProverParamsToStr(bottomUpProverParams)}` 
                ++ `)`
    }
}

let respToStr = resp => {
    switch resp {
        | OnProgress(msg) => `OnProgress("${msg}")`
        | Result(_) => `Result`
    }
}

let unify = (
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~rootStmts: array<rootStmt>,
    ~bottomUpProverParams: option<bottomUpProverParams>,
    ~allowedFrms:allowedFrms,
    ~syntaxTypes:option<array<int>>,
    ~exprsToSyntaxCheck:option<array<expr>>,
    ~debugLevel:int,
    ~onProgress:string=>unit,
): promise<proofTreeDto> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~settingsVer,
            ~settings,
            ~preCtxVer,
            ~preCtx,
            ~varsText,
            ~disjText,
            ~procName,
            ~initialRequest = Unify({
                rootStmts:rootStmts, 
                bottomUpProverParams, 
                allowedFrms,
                combCntMax:settings.combCntMax,
                syntaxTypes,
                exprsToSyntaxCheck,
                debugLevel,
            }),
            ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(msg) => onProgress(msg)
                    | Result(proofTree) => {
                        endWorkerInteraction()
                        resolve(proofTree)
                    }
                }
            },
            ~enableTrace=false
        )
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | Unify({rootStmts, bottomUpProverParams, allowedFrms, combCntMax, syntaxTypes, exprsToSyntaxCheck, debugLevel}) => {
            let proofTree = unifyAll(
                ~parenCnt = getWrkParenCntExn(),
                ~frms = getWrkFrmsExn(),
                ~wrkCtx = getWrkCtxExn(),
                ~rootStmts,
                ~bottomUpProverParams?,
                ~allowedFrms,
                ~combCntMax,
                ~syntaxTypes?, 
                ~exprsToSyntaxCheck?,
                ~debugLevel,
                ~onProgress = msg => sendToClient(OnProgress(msg))
            )
            sendToClient(Result(proofTree->proofTreeToDto(rootStmts->Array.map(stmt=>stmt.expr))))
        }
    }
}

let extractSubstitution = (~frame:frame, ~args:array<int>, ~tree:proofTreeDto):array<expr> => {
    let subs = Expln_utils_common.createArray(frame.numOfVars)
    frame.hyps->Array.forEachWithIndex((hyp,i) => {
        if (hyp.typ == F) {
            subs[hyp.expr->Array.getUnsafe(1)] = (tree.nodes->Array.getUnsafe(args->Array.getUnsafe(i))).expr
        }
    })
    subs
}

let srcToNewStmts = (
    ~src:exprSrcDto,
    ~exprToProve: expr,
    ~tree:proofTreeDto, 
    ~newVarTypes:Belt_HashMapInt.t<int>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~rootExprToLabel: Belt_HashMap.t<expr,string,ExprHash.identity>,
    ~reservedLabels: array<string>,
):option<stmtsDto> => {
    switch src {
        | VarType | Hypothesis(_) | AssertionWithErr(_) => None
        | Assertion({args, label}) => {
            let hasAsrtWithErr = ref(false)
            let hasBackRefErr = ref(false)
            let hasError = () => hasAsrtWithErr.contents || hasBackRefErr.contents

            let res = {
                newVars: [],
                newVarTypes: [],
                newDisj: disjMake(),
                newDisjStr: [],
                stmts: [],
            }

            let exprToLabelArr = rootExprToLabel->Belt_HashMap.toArray
            let exprToLabel = exprToLabelArr->Belt_HashMap.fromArray(~id=module(ExprHash))
            let reservedLabels = reservedLabels->Belt_HashSetString.fromArray
            let getOrCreateLabelForExpr = (expr:expr, prefix:string, createIfAbsent:bool):string => {
                switch exprToLabel->Belt_HashMap.get(expr) {
                    | Some(label) => label
                    | None => {
                        switch ctx->getHypByExpr(expr) {
                            | Some(hyp) => hyp.label
                            | None => {
                                if (createIfAbsent) {
                                    let newLabel = generateNewLabels(
                                        ~ctx, ~prefix, ~amount=1, ~reservedLabels, ~checkHypsOnly=true
                                    )->Array.getUnsafe(0)
                                    exprToLabel->Belt_HashMap.set(expr, newLabel)
                                    reservedLabels->Belt_HashSetString.add(newLabel)
                                    newLabel
                                } else {
                                    raise(MmException({msg:`Could not find a label for an expr.`}))
                                }
                            }
                        }
                    }
                }
            }
            let createLabelForExpr = (expr:expr, prefix:string):string => 
                getOrCreateLabelForExpr(expr, prefix, true)
            let getLabelForExpr = (expr:expr):string => getOrCreateLabelForExpr(expr, "", false)

            let newVarNames = Belt_HashMapInt.make(~hintSize=8)
            let reservedVarNames = Belt_HashSetString.make(~hintSize=8)
            let addNewVarToResult = (~newVarInt:int, ~newVarType:int):unit => {
                res.newVars->Array.push(newVarInt)
                res.newVarTypes->Array.push(newVarType)
                let newVarName = generateNewVarNames( ~ctx, ~types = [newVarType],
                    ~typeToPrefix, ~reservedNames=reservedVarNames
                )->Array.getUnsafe(0)
                newVarNames->Belt_HashMapInt.set(newVarInt, newVarName)
                reservedVarNames->Belt_HashSetString.add(newVarName)
                createLabelForExpr([newVarType, newVarInt], "v")->ignore
            }

            let maxCtxVar = ctx->getNumOfVars - 1
            let intToSym = i => {
                if (i <= maxCtxVar) {
                    switch ctx->ctxIntToSym(i) {
                        | None => raise(MmException({msg:`Cannot determine sym for an existing int in srcToNewStmts.`}))
                        | Some(sym) => sym
                    }
                } else {
                    switch newVarNames->Belt_HashMapInt.get(i) {
                        | None => raise(MmException({msg:`Cannot determine name of a new var in srcToNewStmts.`}))
                        | Some(name) => name
                    }
                }
            }

            let getFrame = (label:string):frame => {
                switch ctx->getFrame(label) {
                    | None => raise(MmException({msg:`Cannot get a frame by label '${label} in srcToNewStmts.'`}))
                    | Some(frame) => frame
                }
            }

            let addExprToResult = (~label, ~expr, ~src, ~isProved) => {
                expr->Array.forEach(ei => {
                    if (ei > maxCtxVar && !(res.newVars->Array.includes(ei))) {
                        switch newVarTypes->Belt_HashMapInt.get(ei) {
                            | None => raise(MmException({msg:`Cannot determine type of a new var in srcToNewStmts.`}))
                            | Some(typ) => addNewVarToResult(~newVarInt=ei, ~newVarType=typ)
                        }
                    }
                })
                let exprStr = expr->Array.map(intToSym)->Array.joinUnsafe(" ")
                let jstf = switch src {
                    | None | Some(VarType | Hypothesis(_)) => None
                    | Some(AssertionWithErr(_)) => {
                        hasAsrtWithErr.contents = true
                        None
                    }
                    | Some(Assertion({args, label})) => {
                        let argLabels = []
                        getFrame(label).hyps->Array.forEachWithIndex((hyp,i) => {
                            if (hyp.typ == E) {
                                argLabels->Array.push(
                                    getLabelForExpr((tree.nodes->Array.getUnsafe(args->Array.getUnsafe(i))).expr)
                                )
                            }
                        })
                        Some({ args:argLabels, label})
                    }
                }
                res.stmts->Array.push( { label, expr, exprStr, jstf, isProved, } )
            }

            let frame = getFrame(label)
            let eArgs = []
            frame.hyps->Array.forEachWithIndex((hyp,i) => {
                if (hyp.typ == E) {
                    eArgs->Array.push(tree.nodes->Array.getUnsafe(args->Array.getUnsafe(i)))
                }
            })
            let childrenReturnedFor = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
            let savedExprs = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
            eArgs->Array.forEach(node => {
                Expln_utils_data.traverseTree(
                    Belt_HashSet.fromArray([exprToProve], ~id=module(ExprHash)),
                    node,
                    (_,node) => {
                        if (childrenReturnedFor->Belt_HashSet.has(node.expr)) {
                            None
                        } else {
                            childrenReturnedFor->Belt_HashSet.add(node.expr)
                            switch node.proof {
                                | None | Some(VarType | Hypothesis(_)) => None
                                | Some(AssertionWithErr(_)) => {
                                    hasAsrtWithErr.contents = true
                                    None
                                }
                                | Some(Assertion({args,label})) => {
                                    let children = []
                                    getFrame(label).hyps->Array.forEachWithIndex((hyp,i) => {
                                        if (hyp.typ == E) {
                                            children->Array.push( tree.nodes->Array.getUnsafe(args->Array.getUnsafe(i)) )
                                        }
                                    })
                                    Some(children)
                                }
                            }
                        }
                    },
                    ~preProcess = (path,node) => {
                        if (hasError()) {
                            Some(())
                        } else {
                            if (path->Belt_HashSet.has(node.expr)) {
                                hasBackRefErr.contents = true
                                Some(())
                            } else {
                                path->Belt_HashSet.add(node.expr)
                                None
                            }
                        }
                    },
                    ~postProcess = (path,node) => {
                        path->Belt_HashSet.remove(node.expr)
                        if (hasError()) {
                            Some(())
                        } else if (!(savedExprs->Belt_HashSet.has(node.expr))) {
                            savedExprs->Belt_HashSet.add(node.expr)
                            switch node.proof {
                                | Some(VarType) | Some(Hypothesis(_)) => None
                                | Some(AssertionWithErr(_)) => {
                                    hasAsrtWithErr.contents = true
                                    Some(())
                                }
                                | Some(Assertion({args,label})) => {
                                    let frame = getFrame(label)
                                    verifyDisjoints(
                                        ~subs = extractSubstitution(~frame, ~args, ~tree),
                                        ~frmDisj = frame.disj,
                                        ~isDisjInCtx = (n,m) => {
                                            if (!(ctx->isDisj(n,m))) {
                                                res.newDisj->disjAddPair(n,m)
                                            }
                                            true
                                        },
                                    )->ignore
                                    addExprToResult(
                                        ~label = createLabelForExpr(node.expr, ""),
                                        ~expr = node.expr, 
                                        ~src = node.proof, 
                                        ~isProved=true
                                    )
                                    None
                                }
                                | None => {
                                    addExprToResult(
                                        ~label = createLabelForExpr(node.expr, ""),
                                        ~expr = node.expr, 
                                        ~src = None, 
                                        ~isProved=false
                                    )
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    }
                )->ignore
            })
            if (hasError()) {
                None
            } else {
                addExprToResult(
                    ~label=getLabelForExpr(exprToProve),
                    ~expr = exprToProve,
                    ~src = Some(src),
                    ~isProved = args->Array.every(idx => (tree.nodes->Array.getUnsafe(idx)).proof->Belt_Option.isSome)
                )
                verifyDisjoints(
                    ~subs = extractSubstitution(~frame, ~args, ~tree),
                    ~frmDisj = frame.disj,
                    ~isDisjInCtx = (n,m) => {
                        if (!(ctx->isDisj(n,m))) {
                            res.newDisj->disjAddPair(n,m)
                        }
                        true
                    },
                )->ignore
                res.newDisj->disjForEachArr(disjArr => {
                    res.newDisjStr->Array.push(
                        `$d ${disjArr->Array.map(intToSym)->Array.joinUnsafe(" ")} $.`
                    )
                })
                Some(res)
            }
        }
    }
}

let proofTreeDtoToNewStmtsDto = (
    ~treeDto:proofTreeDto, 
    ~exprToProve: expr,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~rootExprToLabel: Belt_HashMap.t<expr,string,ExprHash.identity>,
    ~reservedLabels: array<string>,
):array<stmtsDto> => {
    let newVarTypes = treeDto.newVars->Array.map(@warning("-8")([typ, var]) => (var, typ))->Belt_HashMapInt.fromArray
    let proofNode = switch treeDto.nodes->Array.find(node => node.expr->exprEq(exprToProve)) {
        | None => raise(MmException({msg:`the proof tree DTO doesn't contain the node to prove.`}))
        | Some(node) => node
    }

    proofNode.parents
        ->Array.map(src => srcToNewStmts(
            ~exprToProve,
            ~rootExprToLabel,
            ~reservedLabels,
            ~src,
            ~tree = treeDto,
            ~newVarTypes,
            ~ctx,
            ~typeToPrefix: Belt_MapString.t<string>,
        ))
        ->Array.filter(Belt_Option.isSome(_))
        ->Array.map(Belt_Option.getExn(_))
}