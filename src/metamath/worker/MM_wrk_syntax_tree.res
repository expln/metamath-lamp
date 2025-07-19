open Common
open MM_context
open MM_wrk_ctx_proc
open MM_wrk_settings
open Expln_utils_common
open MM_syntax_tree
open MM_substitution
open MM_parenCounter
open MM_provers
open MM_proof_tree

let procName = "MM_wrk_syntax_tree"

type request = 
    | BuildSyntaxTreesForAllAsrts

type response =
    | OnProgress(float)
    | Result(array<(string,syntaxTreeNode)>)

let reqToStr = req => {
    switch req {
        | BuildSyntaxTreesForAllAsrts => `BuildSyntaxTreesForAllAsrts`
    }
}

let respToStr = resp => {
    switch resp {
        | OnProgress(pct) => `OnProgress(pct=${pct->Belt_Float.toString})`
        | Result(_) => `Result`
    }
}

let buildSyntaxTreesForAllAssertions = (
    ~settingsV:version<settings>,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~onProgress:float=>unit,
): promise<array<(string,syntaxTreeNode)>> => {
    Promise.make((resolve,_) => {
        beginWorkerInteractionUsingCtx(
            ~settingsVer=settingsV.ver,
            ~settings=settingsV.val,
            ~preCtxVer,
            ~preCtx,
            ~varsText="",
            ~disjText="",
            ~procName,
            ~initialRequest=BuildSyntaxTreesForAllAsrts,
            ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | Result(res) => {
                        endWorkerInteraction()
                        resolve(res)
                    }
                }
            },
            ~enableTrace=false
        )
    })
}

let doBuildSyntaxTreesForAllAssertions = (
    ~ctx:mmContext,
    ~frms:frms,
    ~parenCnt:parenCnt,
    ~allowedFrmsInSyntax:frameRestrict,
    ~syntaxTypes:array<int>,
    ~onProgress:option<float=>unit>=?,
):array<(string,syntaxTreeNode)> => {
    let ctx = createContext(~parent=ctx)

    let typToLocVar: Belt_HashMapInt.t<int> = Belt_HashMapInt.make(~hintSize=4)

    let tmpCtxHyps:Belt_HashMapString.t<hypothesis> = Belt_HashMapString.make(~hintSize=4)

    let createNewVar = (typ:int):int => {
        @warning("-8")
        let [label] = generateNewLabels( ~ctx=ctx, ~prefix="locVar", ~amount=1 )
        @warning("-8")
        let [varName] = generateNewVarNames( ~ctx=ctx, ~types=[typ] )
        ctx->applySingleStmt( Var({symbols:[varName]}) )
        ctx->applySingleStmt( Floating({label, expr:[ctx->ctxIntToSymExn(typ), varName]}) )
        tmpCtxHyps->Belt_HashMapString.set(label, ctx->getHypothesis(label)->Option.getExn(~message="tmpCtxHyps"))
        ctx->ctxSymToIntExn(varName)
    }

    let getCtxLocVar = (typ:int):int => {
        switch typToLocVar->Belt_HashMapInt.get(typ) {
            | None => {
                let newVar = createNewVar(typ)
                typToLocVar->Belt_HashMapInt.set(typ,newVar)
                newVar
            }
            | Some(locVar) => locVar
        }
    }

    let asrtExprsWithCtxVars:Belt_HashMapString.t<expr> = Belt_HashMapString.make(~hintSize=1000)
    ctx->forEachFrame(frame => {
        asrtExprsWithCtxVars->Belt_HashMapString.set(
            frame.label,
            frame.asrt->Array.map(i => i < 0 ? i : getCtxLocVar(frame.varTypes->Array.getUnsafe(i)))
        )            
        None
    })->ignore
    let exprsToSyntaxProve = asrtExprsWithCtxVars->Belt_HashMapString.valuesToArray
        ->Expln_utils_common.sortInPlaceWith(comparatorByInt(Array.length(_))->cmpRev)
        ->Array.map(expr => expr->Array.sliceToEnd(~start=1))

    // let asrtExprStr = asrtExprsWithCtxVars->Array.map(ctxIntsToStrExn(ctx, _))->Array.joinUnsafe("\n")
    // Expln_utils_files.writeStringToFile(asrtExprStr, "./asrtExprStr.txt")

    let proofTree = proveSyntaxTypes(
        ~wrkCtx=ctx,
        ~frms,
        ~frameRestrict=allowedFrmsInSyntax,
        ~parenCnt,
        ~exprs=exprsToSyntaxProve,
        ~syntaxTypes,
        ~onProgress?,
    )

    // Expln_utils_files.writeStringToFile(proofTree->ptPrintStats, "./unprovedNodes.txt")

    let makeCtxIntToAsrtInt = (asrtExpr:expr):(int=>int) => {
        let curIdx = ref(-1)
        let maxIdx = asrtExpr->Array.length-1
        (ctxInt:int) => {
            if (ctxInt < 0) {
                ctxInt
            } else {
                curIdx := curIdx.contents + 1
                while (curIdx.contents <= maxIdx && asrtExpr->Array.getUnsafe(curIdx.contents) < 0) {
                    curIdx := curIdx.contents + 1
                }
                if (maxIdx < curIdx.contents) {
                    let errMsg = `makeCtxIntToAsrtInt: asrtExpr=${asrtExpr->Expln_utils_common.stringify}`
                        ++ `, ctxInt=${ctxInt->Int.toString}`
                    Console.error(errMsg)
                    Exn.raiseError(errMsg)
                } else {
                    asrtExpr->Array.getUnsafe(curIdx.contents)
                }
            }
        }
    }

    let ctxHypLabelToAsrtHypLabel = (ctxHypLabel:string, asrtVar:int, frame:frame):option<string> => {
        tmpCtxHyps->Belt_HashMapString.get(ctxHypLabel)->Option.flatMap(ctxHyp => {
            let typ = ctxHyp.typ
            let asrtHyp = frame.hyps->Array.getUnsafe(frame.varHyps->Array.getUnsafe(asrtVar))
            if (asrtHyp.typ == typ) {
                Some(asrtHyp.label)
            } else {
                None
            }
        })
    }

    let result:array<(string,syntaxTreeNode)> = []
    ctx->forEachFrame(frame => {
        switch asrtExprsWithCtxVars->Belt_HashMapString.get(frame.label) {
            | None => ()
            | Some(asrtExprWithCtxVars) => {
                switch proofTree->ptGetSyntaxProof(asrtExprWithCtxVars->Array.sliceToEnd(~start=1)) {
                    | None => ()
                    | Some(proofNode) => {
                        switch MM_asrt_syntax_tree.buildSyntaxTree(
                            ~proofNode,
                            ~ctxIntToAsrtInt=makeCtxIntToAsrtInt(frame.asrt),
                            ~asrtIntToSym=asrtInt=>ctx->frmIntToSymExn(frame,asrtInt),
                            ~ctxHypLabelAndAsrtVarToAsrtHypLabel=
                                (label,asrtVar)=>ctxHypLabelToAsrtHypLabel(label,asrtVar,frame),
                        ) {
                            | Error(msg) => Console.error("Could not build an asrt syntax tree: " ++ msg)
                            | Ok(syntaxTree) => result->Array.push((frame.label, syntaxTree))
                        }
                    }
                }
            }
        }
        None
    })->ignore
    result
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | BuildSyntaxTreesForAllAsrts => {
            sendToClient(Result(
                doBuildSyntaxTreesForAllAssertions(
                    ~ctx=getWrkCtxExn(),
                    ~frms=getWrkFrmsExn(),
                    ~parenCnt=getWrkParenCntExn(),
                    ~allowedFrmsInSyntax=getSettingsExn().allowedFrms.inSyntax,
                    ~syntaxTypes=getSyntaxTypesExn(),
                    ~onProgress = pct => sendToClient(OnProgress(pct))
                )
            ))
        }
    }
}