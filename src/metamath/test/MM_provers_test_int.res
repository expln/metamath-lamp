open Expln_test
open MM_parser
open MM_context
open MM_provers
open MM_substitution
open Common
open MM_proof_tree
open Expln_utils_common
open MM_asrt_syntax_tree

let mmFilePath = "./src/metamath/test/resources/set._mm"

let getCurrMillis = () => Date.make()->Date.getTime
let durationToSeconds = (start,end):int => ((end -. start) /. 1000.0)->Belt_Float.toInt
let durationToSecondsStr = (start,end):string => durationToSeconds(start,end)->Belt.Int.toString
let compareExprBySize:comparator<expr> = comparatorBy(Array.length(_))

let log = msg => Console.log(`${currTimeStr()} ${msg}`)

let getIndentForLevel = (level:int):string => "    "->String.repeat(level)

let rec printSyntaxTree = (tree:MM_syntax_tree.childNode, ~level:int=0):unit => {
    switch tree {
        | Symbol({sym}) => Console.log(getIndentForLevel(level) ++ sym)
        | Subtree({label, children}) => {
            Console.log(getIndentForLevel(level+1) ++ `#${label}`)
            children->Array.forEach(printSyntaxTree(_, ~level=level+1))
        }
    }
}

describe("proveSyntaxTypes", _ => {

    it("finds syntax proofs for each assertion in set.mm", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)

        let ctx = ast->loadContext(
            // ~descrRegexToDisc = "\\(New usage is discouraged\\.\\)"->strToRegex->Belt_Result.getExn,
            ~labelRegexToDisc = "bj-0"->strToRegex->Belt_Result.getExn,
            // ~stopBefore="mathbox",
            // ~debug=true
        )
        let parens = "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( ))"
        let ctx = ctx->ctxOptimizeForProver(~parens)
        ctx->openChildContext
        let (_,syntaxTypes) = MM_wrk_pre_ctx_data.findTypes(ctx)

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
        let asrtExprsToProve = asrtExprsWithCtxVars->Belt_HashMapString.valuesToArray
            ->Expln_utils_common.sortInPlaceWith(compareExprBySize->comparatorInverse)


        // let asrtExprStr = asrtExprsWithCtxVars->Array.map(ctxIntsToStrExn(ctx, _))->Array.joinUnsafe("\n")
        // Expln_utils_files.writeStringToFile(asrtExprStr, "./asrtExprStr.txt")

        let exprsToSyntaxProve = asrtExprsToProve->Array.map(expr => expr->Array.sliceToEnd(~start=1))
        let frms = prepareFrmSubsData(~ctx)
        let parenCnt = makeParenCnt(~ctx, ~parens)

        let startMs = getCurrMillis()
        let lastPct = ref(startMs)
        let frameRestrict:MM_wrk_settings.frameRestrict = {
            useDisc:false,
            useDepr:true,
            useTranDepr:true,
        }
        log(`started proving syntax, number of expressions = (${exprsToSyntaxProve->Array.length->Int.toString})`)

        //when
        let proofTree = proveSyntaxTypes(
            ~wrkCtx=ctx,
            ~frms,
            ~frameRestrict,
            ~parenCnt,
            ~exprs=exprsToSyntaxProve,
            ~syntaxTypes,
            ~onProgress=pct=>{
                let currMs = getCurrMillis()
                log(`proving syntax: ${pct->floatToPctStr} - ${durationToSecondsStr(lastPct.contents, currMs)} sec`)
                lastPct := currMs
            }
        )

        //then
        let endMs = getCurrMillis()
        log(`Overall duration (sec): ${durationToSecondsStr(startMs, endMs)}` )

        // Expln_utils_files.writeStringToFile(proofTree->ptPrintStats, "./unprovedNodes.txt")

        let unprovedAsrtExprs = asrtExprsToProve
            ->Array.filter(expr => proofTree->ptGetSyntaxProof(expr->Array.sliceToEnd(~start=1))->Belt_Option.isNone)
        // let unprovedAsrtExprStr = unprovedAsrtExprs->Array.map(ctxIntsToStrExn(ctx, _))->Array.joinUnsafe("\n")
        // Expln_utils_files.writeStringToFile(unprovedAsrtExprStr, "./unprovedAsrtExprStr.txt")
        assertEqMsg(unprovedAsrtExprs->Array.length, 0, "unprovedAsrtExprs->Array.length = 0")

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
                        Exn.raiseError(
                            `makeCtxIntToAsrtInt: asrtExpr=${asrtExpr->Expln_utils_common.stringify}`
                            ++ `, ctxInt=${ctxInt->Int.toString}`
                        )
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

        let syntaxTrees:Belt_HashMapString.t<MM_syntax_tree.syntaxTreeNode> = 
            Belt_HashMapString.make(~hintSize=asrtExprsWithCtxVars->Belt_HashMapString.size)
        ctx->forEachFrame(frame => {
            switch asrtExprsWithCtxVars->Belt_HashMapString.get(frame.label) {
                | None => Exn.raiseError(`asrtExprsWithCtxVars->Belt_HashMapString.get("${frame.label}") is None`)
                | Some(asrtExprWithCtxVars) => {
                    switch buildSyntaxTree(
                        ~proofNode=proofTree->ptGetSyntaxProof(asrtExprWithCtxVars->Array.sliceToEnd(~start=1))->Belt_Option.getExn,
                        ~ctxIntToAsrtInt=makeCtxIntToAsrtInt(frame.asrt),
                        ~asrtIntToSym=asrtInt=>ctx->frmIntToSymExn(frame,asrtInt),
                        ~ctxHypLabelAndAsrtVarToAsrtHypLabel=(label,asrtVar)=>ctxHypLabelToAsrtHypLabel(label,asrtVar,frame),
                    ) {
                        | Error(msg) => Exn.raiseError("Could not build an asrt syntax tree: " ++ msg)
                        | Ok(syntaxTree) => {
                            syntaxTrees->Belt_HashMapString.set(frame.label, syntaxTree)
                        }
                    }
                }
            }
            None
        })->ignore

        let asrtToPrint = "mdsymlem8"
        Console.log(`--- ${asrtToPrint} ------------------------------------------------`)
        syntaxTrees->Belt_HashMapString.get(asrtToPrint)->Option.getExn->Subtree->printSyntaxTree
        Console.log(`-------------------------------------------------------------------`)
        let ctxExprStr = "( ( ch -> ph ) -> th )"
        let matchedAsrts = []
        Console.log2("Find matching assertions for ", ctxExprStr)
        Expln_test.startTimer("find match")
        switch MM_wrk_editor.textToSyntaxTree(
            ~wrkCtx=ctx,
            ~syms=[ctxExprStr->getSpaceSeparatedValuesAsArray],
            ~syntaxTypes,
            ~frms,
            ~frameRestrict,
            ~parenCnt,
            ~lastSyntaxType=None,
            ~onLastSyntaxTypeChange= _ => (),
        ) {
            | Error(msg) => Exn.raiseError(`Could not build a syntax tree for the expression '${ctxExprStr}', error message: ${msg}`)
            | Ok(arr) => {
                switch arr->Array.getUnsafe(0) {
                    | Error(msg) => Exn.raiseError(`Could not build a syntax tree for the expression '${ctxExprStr}', error message: ${msg}`)
                    | Ok(ctxSyntaxTree) => {
                        Console.log(`--- ${ctxExprStr} ------------------------------------------------`)
                        Subtree(ctxSyntaxTree)->printSyntaxTree
                        Console.log(`-------------------------------------------------------------------`)
                        let foundSubs = MM_asrt_syntax_tree.unifSubsMake()
                        syntaxTrees->Belt_HashMapString.forEach((label,asrtTree) => {
                            let continue = ref(true)
                            MM_asrt_syntax_tree.unifSubsReset(foundSubs)
                            MM_asrt_syntax_tree.unify(
                                ~asrtExpr=asrtTree,
                                ~ctxExpr=ctxSyntaxTree,
                                ~isMetavar = _ => true,
                                ~foundSubs,
                                ~continue,
                            )
                            if (continue.contents) {
                                matchedAsrts->Array.push(label)
                                // let frame = ctx->getFrameExn(label)
                                // Console.log(`${label}: ${ctx->frmIntsToStrExn(frame, frame.asrt)}`)
                            }
                        })
                    }
                }
            }
        }
        Expln_test.stopTimer("find match")
        matchedAsrts->Array.sort(strCmp)
        Console.log(`Found ${matchedAsrts->Array.length->Int.toString} matching assertions.`)
        // matchedAsrts->Array.forEach(Console.log)
    })
})