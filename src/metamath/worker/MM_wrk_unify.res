open MM_context
open MM_parser
open Expln_utils_promise
open MM_wrk_ctx
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_statements_dto

let procName = "MM_wrk_unify"

type request = 
    | Unify({
        stmts: array<rootStmt>, 
        bottomUpProverParams:option<bottomUpProverParams>,
    })

type response =
    | OnProgress(string)
    | Result(proofTreeDto)

let unify = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~stmts: array<rootStmt>,
    ~bottomUpProverParams: option<bottomUpProverParams>,
    ~onProgress:string=>unit,
): promise<proofTreeDto> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~preCtxVer,
            ~preCtx,
            ~parenStr,
            ~varsText,
            ~disjText,
            ~hyps,
            ~procName,
            ~initialRequest = Unify({stmts:stmts, bottomUpProverParams}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
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
        | Unify({stmts, bottomUpProverParams}) => {
            let proofTree = unifyAll(
                ~parenCnt = getWrkParenCntExn(),
                ~frms = getWrkFrmsExn(),
                ~ctx = getWrkCtxExn(),
                ~stmts,
                ~bottomUpProverParams?,
                ~onProgress = msg => sendToClient(OnProgress(msg)),
                ()
            )
            sendToClient(Result(proofTree->proofTreeToDto(stmts->Js_array2.map(stmt=>stmt.expr))))
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

let srcToNewStmts = (
    ~rootStmts:array<rootStmt>,
    ~src:exprSourceDto, 
    ~tree:proofTreeDto, 
    ~newVarTypes:Belt_HashMapInt.t<int>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
):option<stmtsDto> => {
    switch src {
        | Assertion({args, label}) => {
            let maxCtxVar = ctx->getNumOfVars - 1
            let res = {
                newVars: [],
                newVarTypes: [],
                newDisj: disjMutableMake(),
                newDisjStr: [],
                stmts: [],
                newUnprovedStmts: [],
            }
            let exprToLabel = rootStmts
                ->Js.Array2.map(stmt=>(stmt.expr,stmt.label))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let varNames = Belt_HashMapInt.make(~hintSize=8)
            let usedVarNames = Belt_HashSetString.make(~hintSize=8)
            let usedLabels = rootStmts->Js.Array2.map(stmt=>stmt.label)->Belt_HashSetString.fromArray

            let getFrame = label => {
                switch ctx->getFrame(label) {
                    | None => raise(MmException({msg:`Cannot get a frame by label '${label} in srcToNewStmts.'`}))
                    | Some(frame) => frame
                }
            }

            let intToSym = i => {
                if (i <= maxCtxVar) {
                    switch ctx->ctxIntToSym(i) {
                        | None => raise(MmException({msg:`Cannot determine sym for an existing int in nodeToNewStmts.`}))
                        | Some(sym) => sym
                    }
                } else {
                    switch varNames->Belt_HashMapInt.get(i) {
                        | None => raise(MmException({msg:`Cannot determine name of a new var in nodeToNewStmts.`}))
                        | Some(name) => name
                    }
                }
            }

            let isRootStmt = expr => {
                rootStmts
                    ->Js.Array2.find(stmt => stmt.expr->exprEq(expr))
                    ->Belt.Option.isSome
            }

            let addExprToResult = (~label, ~expr, ~jstf, ~isProved) => {
                expr->Js_array2.forEach(ei => {
                    if (ei > maxCtxVar && !(res.newVars->Js_array2.includes(ei))) {
                        switch newVarTypes->Belt_HashMapInt.get(ei) {
                            | None => raise(MmException({msg:`Cannot determine type of a new var in nodeToNewStmts.`}))
                            | Some(typ) => {
                                res.newVars->Js_array2.push(ei)->ignore
                                res.newVarTypes->Js_array2.push(typ)->ignore
                                let newVarName = generateNewVarNames( ~ctx, ~types = [typ],
                                    ~typeToPrefix, ~usedNames=usedVarNames, ()
                                )[0]
                                usedVarNames->Belt_HashSetString.add(newVarName)
                                varNames->Belt_HashMapInt.set(ei, newVarName)
                            }
                        }
                    }
                })
                let exprStr = expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" ")
                let jstf = switch jstf {
                    | Some(Assertion({args, label})) => {
                        let argLabels = []
                        getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                            if (hyp.typ == E) {
                                switch exprToLabel->Belt_HashMap.get(tree.nodes[args[i]].expr) {
                                    | None => raise(MmException({msg:`Cannot get a label for an arg by arg's expr.`}))
                                    | Some(argLabel) => argLabels->Js_array2.push(argLabel)->ignore
                                }
                            }
                        })
                        Some({ args:argLabels, label})
                    }
                    | _ => None
                }
                let stmt = {
                    label,
                    expr,
                    exprStr,
                    jstf,
                    isProved,
                }
                res.stmts->Js_array2.push( stmt )->ignore
                if (!isProved && !isRootStmt(expr)) {
                    res.newUnprovedStmts->Js_array2.push( stmt )->ignore
                }
            }

            let frame = getFrame(label)
            let eArgs = []
            frame.hyps->Js.Array2.forEachi((hyp,i) => {
                if (hyp.typ == E) {
                    eArgs->Js_array2.push(tree.nodes[args[i]])->ignore
                }
            })
            eArgs->Js.Array2.forEach(node => {
                let childrenReturnedFor = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
                let savedExprs = Belt_HashSet.make(~hintSize=16, ~id=module(ExprHash))
                Expln_utils_data.traverseTree(
                    (),
                    node,
                    (_,node) => {
                        if (childrenReturnedFor->Belt_HashSet.has(node.expr)) {
                            None
                        } else {
                            childrenReturnedFor->Belt_HashSet.add(node.expr)
                            switch node.proof {
                                | Some(Assertion({args,label})) => {
                                    let children = []
                                    getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                                        if (hyp.typ == E) {
                                            children->Js_array2.push( tree.nodes[args[i]] )->ignore
                                        }
                                    })
                                    Some(children)
                                }
                                | _ => None
                            }
                        }
                    },
                    ~postProcess = (_,node) => {
                        if (!(savedExprs->Belt_HashSet.has(node.expr))) {
                            savedExprs->Belt_HashSet.add(node.expr)
                            let label = switch exprToLabel->Belt_HashMap.get(node.expr) {
                                | Some(label) => label
                                | None => generateNewLabels(~ctx, ~prefix="", ~amount=1, ~usedLabels, ())[0]
                            }
                            exprToLabel->Belt_HashMap.set(node.expr, label)
                            usedLabels->Belt_HashSetString.add(label)
                            addExprToResult(
                                ~label, 
                                ~expr = node.expr, 
                                ~jstf = node.proof, 
                                ~isProved=node.proof->Belt_Option.isSome
                            )
                        }
                        None
                    },
                    ()
                )->ignore
            })
            let stmtToProve = rootStmts[rootStmts->Js.Array2.length-1]
            addExprToResult(
                ~label=stmtToProve.label,
                ~expr = stmtToProve.expr,
                ~jstf = Some(src),
                ~isProved = args->Js_array2.every(idx => tree.nodes[idx].proof->Belt_Option.isSome)
            )
            let varIsUsed = v => v <= maxCtxVar || res.newVars->Js.Array2.includes(v)
            tree.disj->disjForEach((n,m) => {
                if (varIsUsed(n) && varIsUsed(m) && !(ctx->isDisj(n,m))) {
                    res.newDisj->disjAddPair(n,m)
                    res.newDisjStr->Js.Array2.push(`$d ${intToSym(n)} ${intToSym(m)} $.`)->ignore
                }
            })
            Some(res)
        }
        | _ => None
    }
}

let proofTreeDtoToNewStmtsDto = (
    ~treeDto:proofTreeDto, 
    ~rootStmts:array<rootStmt>,
    ~ctx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
):array<stmtsDto> => {
    let newVarTypes = treeDto.newVars->Js_array2.map(([typ, var]) => (var, typ))->Belt_HashMapInt.fromArray
    let stmtToProve = rootStmts[rootStmts->Js_array2.length-1]
    let proofNode = switch treeDto.nodes->Js_array2.find(node => node.expr->exprEq(stmtToProve.expr)) {
        | None => raise(MmException({msg:`the proof tree DTO doesn't contain the node to prove.`}))
        | Some(node) => node
    }

    switch proofNode.parents {
        | None => []
        | Some(parents) => {
            parents
                ->Js_array2.map(src => srcToNewStmts(
                    ~rootStmts,
                    ~src, 
                    ~tree = treeDto, 
                    ~newVarTypes,
                    ~ctx,
                    ~typeToPrefix: Belt_MapString.t<string>,
                ))
                ->Js.Array2.filter(Belt_Option.isSome)
                ->Js.Array2.map(Belt_Option.getExn)
                ->Js.Array2.filter(doesntHaveBackRefs)
        }
    }
}