open MM_context
open MM_parser
open Expln_utils_promise
open MM_wrk_ctx_data
open MM_wrk_ctx_proc
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_statements_dto
open MM_wrk_settings

let procName = "MM_wrk_unify"

type request = 
    | Unify({
        rootStmts: array<rootStmt>, 
        bottomUpProverParams:option<bottomUpProverParams>,
        debugLevel:int,
    })

type response =
    | OnProgress(string)
    | Result(proofTreeDto)

let reqToStr = req => {
    switch req {
        | Unify({debugLevel}) => `Unify(debugLevel=${debugLevel->Belt_Int.toString})`
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
            ~initialRequest = Unify({rootStmts:rootStmts, bottomUpProverParams, debugLevel}),
            ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(msg) => onProgress(msg)
                    | Result(proofTree) => {
                        endWorkerInteraction()
                        resolve(proofTree)
                    }
                }
            },
            ~enableTrace=false,
            ()
        )
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | Unify({rootStmts, bottomUpProverParams, debugLevel}) => {
            let proofTree = unifyAll(
                ~parenCnt = getWrkParenCntExn(),
                ~frms = getWrkFrmsExn(),
                ~wrkCtx = getWrkCtxExn(),
                ~rootStmts,
                ~bottomUpProverParams?,
                ~debugLevel,
                ~onProgress = msg => sendToClient(OnProgress(msg)),
                ()
            )
            sendToClient(Result(proofTree->proofTreeToDto(rootStmts->Js_array2.map(stmt=>stmt.expr))))
        }
    }
}

let doesntHaveBackRefs = (newStmtsDto:stmtsDto):bool => {
    let res = newStmtsDto.stmts->Js.Array2.reduce(
        (res, stmt) => {
            switch res {
                | Error(_) => res
                | Ok(refs) => {
                    switch stmt.jstf {
                        | None => ()
                        | Some(jstf) => {
                            jstf.args->Js.Array2.forEach(ref => refs->Js_array2.push(ref)->ignore)
                        }
                    }
                    if (refs->Js_array2.includes(stmt.label)) {
                        Error(())
                    } else {
                        Ok(refs)
                    }
                }
            }
        },
        Ok([])
    )
    switch res {
        | Ok(_) => true
        | Error(_) => false
    }
}

//todo: review this function
let srcToNewStmts = (
    ~src:exprSrcDto,
    ~exprToProve: expr,
    ~tree:proofTreeDto, 
    ~newVarTypes:Belt_HashMapInt.t<int>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~rootExprToLabel: Belt_HashMapString.t<expr,string,ExprHash.identity>,
):option<stmtsDto> => {
    switch src {
        | VarType | Hypothesis(_) | AssertionWithErr(_) => None
        | Assertion({args, label, missingDisj}) => {
            let hasAsrtWithErr = ref(false)
            let res = {
                newVars: [],
                newVarTypes: [],
                newDisj: disjMake(),
                newDisjStr: [],
                stmts: [],
            }

            let exprToLabelArr = rootExprToLabel->Belt_HashMap.toArray
            let exprToLabel = exprToLabelArr->Belt_HashMap.fromArray(~id=module(ExprHash))
            let reservedLabels = exprToLabelArr->Js_array2.map(((_,label)) => label)->Belt_HashSetString.fromArray
            let getLabelForExpr = (expr:expr):string => {
                switch exprToLabel->Belt_HashMap.get(expr) {
                    | Some(label) => label
                    | None => {
                        switch ctx->getHypByExpr(expr) {
                            | Some(hyp) => hyp.label
                            | None => {
                                let newLabel = generateNewLabels(
                                    ~ctx, ~prefix="", ~amount=1, ~reservedLabels, ~checkHypsOnly=true, ()
                                )[0]
                                exprToLabel->Belt_HashMap.set(expr, newLabel)
                                reservedLabels->Belt_HashSetString.add(newLabel)
                                newLabel
                            }
                        }
                    }
                }
            }

            let newVarNames = Belt_HashMapInt.make(~hintSize=8)
            let reservedVarNames = Belt_HashSetString.make(~hintSize=8)
            let addNewVarToResult = (~newVarInt:int, ~newVarType:int):unit => {
                res.newVars->Js_array2.push(newVarInt)->ignore
                res.newVarTypes->Js_array2.push(newVarType)->ignore
                let newVarName = generateNewVarNames( ~ctx, ~types = [newVarType],
                    ~typeToPrefix, ~reservedNames=reservedVarNames, ()
                )[0]
                newVarNames->Belt_HashMapInt.set(newVarInt, newVarName)
                reservedVarNames->Belt_HashSetString.add(newVarName)
                getLabelForExpr([newVarType, newVarInt])->ignore
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
                expr->Js_array2.forEach(ei => {
                    if (ei > maxCtxVar && !(res.newVars->Js_array2.includes(ei))) {
                        switch newVarTypes->Belt_HashMapInt.get(ei) {
                            | None => raise(MmException({msg:`Cannot determine type of a new var in srcToNewStmts.`}))
                            | Some(typ) => addNewVarToResult(~newVarInt=ei, ~newVarType=typ)
                        }
                    }
                })
                let exprStr = expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" ")
                let jstf = switch src {
                    | Some(VarType) | Hypothesis(_) => None
                    | AssertionWithErr(_) => {
                        hasAsrtWithErr.contents = true
                        None
                    }
                    | Some(Assertion({args, label})) => {
                        let argLabels = []
                        getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                            if (hyp.typ == E) {
                                argLabels->Js_array2.push(
                                    getLabelForExpr(tree.nodes[args[i]].expr)
                                )->ignore
                            }
                        })
                        Some({ args:argLabels, label})
                    }
                }
                res.stmts->Js_array2.push( { label, expr, exprStr, jstf, isProved, } )->ignore
            }

            let frame = getFrame(label)
            let eArgs = []
            frame.hyps->Js.Array2.forEachi((hyp,i) => {
                if (hyp.typ == E) {
                    eArgs->Js_array2.push(tree.nodes[args[i]])->ignore
                }
            })
            let childrenReturnedFor = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
            let savedExprs = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
            eArgs->Js.Array2.forEach(node => {
                Expln_utils_data.traverseTree(
                    (),
                    node,
                    (_,node) => {
                        if (childrenReturnedFor->Belt_HashSet.has(node.expr)) {
                            None
                        } else {
                            childrenReturnedFor->Belt_HashSet.add(node.expr)
                            switch node.proof {
                                | Some(VarType) | Some(Hypothesis(_)) => None
                                | Some(AssertionWithErr(_)) => {
                                    hasAsrtWithErr.contents = true
                                    None
                                }
                                | Some(Assertion({args,label})) => {
                                    let children = []
                                    getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                                        if (hyp.typ == E) {
                                            children->Js_array2.push( tree.nodes[args[i]] )->ignore
                                        }
                                    })
                                    Some(children)
                                }
                            }
                        }
                    },
                    ~postProcess = (_,node) => {
                        if (hasAsrtWithErr.contents) {
                            Some(())
                        } else if (!(savedExprs->Belt_HashSet.has(node.expr))) {
                            savedExprs->Belt_HashSet.add(node.expr)
                            switch node.proof {
                                | Some(VarType) | Some(Hypothesis(_)) => None
                                | Some(AssertionWithErr(_)) => {
                                    hasAsrtWithErr.contents = true
                                    Some(())
                                }
                                | Some(Assertion({missingDisj})) => {
                                    addExprToResult(
                                        ~label = getLabelForExpr(node.expr), 
                                        ~expr = node.expr, 
                                        ~src = node.proof, 
                                        ~isProved=node.proof->Belt_Option.isSome
                                    )
                                    switch missingDisj {
                                        | None => None
                                        | Some(missingDisj) => {
                                            missingDisj->disjForEach((n,m) => {
                                                res.newDisj->disjAddPair(n,m)
                                                res.newDisjStr
                                                    ->Js.Array2.push(`$d ${intToSym(n)} ${intToSym(m)} $.`)->ignore
                                            })
                                        }
                                    }
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    },
                    ()
                )->ignore
            })
            addExprToResult(
                ~label=getLabelForExpr(exprToProve),
                ~expr = exprToProve,
                ~src = Some(src),
                ~isProved = args->Js_array2.every(idx => tree.nodes[idx].proof->Belt_Option.isSome)
            )
            let varIsUsed = v => v <= maxCtxVar || res.newVars->Js.Array2.includes(v)
            tree.disj->disjForEach((n,m) => {
                if (varIsUsed(n) && varIsUsed(m) && !(ctx->isDisj(n,m))) {
                    res.newDisj->disjAddPair(n,m)
                    res.newDisjStr->Js.Array2.push(`$d ${intToSym(n)} ${intToSym(m)} $.`)->ignore
                }
            })
            if (hasAsrtWithErr.contents) {
                None
            } else {
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
    ~reservedLabels: array<string>,
    ~rootExprToLabel: Belt_HashMapString.t<expr,string,ExprHash.identity>,
):array<stmtsDto> => {
    let newVarTypes = treeDto.newVars->Js_array2.map(([typ, var]) => (var, typ))->Belt_HashMapInt.fromArray
    let proofNode = switch treeDto.nodes->Js_array2.find(node => node.expr->exprEq(stmtToProve.expr)) {
        | None => raise(MmException({msg:`the proof tree DTO doesn't contain the node to prove.`}))
        | Some(node) => node
    }

    proofNode.parents
        ->Js_array2.map(src => srcToNewStmts(
            ~exprToProve,
            ~reservedLabels,
            ~src,
            ~tree = treeDto,
            ~newVarTypes,
            ~ctx,
            ~typeToPrefix: Belt_MapString.t<string>,
        ))
        ->Js.Array2.filter(Belt_Option.isSome)
        ->Js.Array2.map(Belt_Option.getExn)
        // ->Js.Array2.filter(doesntHaveBackRefs)
}