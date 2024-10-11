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
let compareExprBySize = comparatorBy(Array.length(_))

let log = msg => Console.log(`${currTimeStr()} ${msg}`)

describe("proveSyntaxTypes", _ => {

    it("finds syntax proofs for each assertion in set.mm", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let ctx = ast->loadContext(
            ~descrRegexToDisc = "\\(New usage is discouraged\\.\\)"->strToRegex->Belt_Result.getExn,
            // ~stopBefore="mathbox",
            // ~debug=true,
            ()
        )
        let parens = "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( ))"
        let ctx = ctx->ctxOptimizeForProver(~parens, ())
        ctx->openChildContext
        let (_,syntaxTypes) = MM_wrk_pre_ctx_data.findTypes(ctx)

        let typToLocVars: Belt_HashMapInt.t<array<int>> = Belt_HashMapInt.make(~hintSize=4)
        let typToNextLocVarIdx: Belt_HashMapInt.t<int> = Belt_HashMapInt.make(~hintSize=16)

        let createNewVar = (typ:int):int => {
            @warning("-8")
            let [label] = generateNewLabels( ~ctx=ctx, ~prefix="locVar", ~amount=1, (), )
            @warning("-8")
            let [varName] = generateNewVarNames( ~ctx=ctx, ~types=[typ], () )
            ctx->applySingleStmt( Var({symbols:[varName]}), () )
            ctx->applySingleStmt( Floating({label, expr:[ctx->ctxIntToSymExn(typ), varName]}), () )
            ctx->ctxSymToIntExn(varName)
        }

        let getCtxLocVar = (typ:int):int => {
            switch typToLocVars->Belt_HashMapInt.get(typ) {
                | None => {
                    let newVar = createNewVar(typ)
                    typToLocVars->Belt_HashMapInt.set(typ,[newVar])
                    typToNextLocVarIdx->Belt_HashMapInt.set(typ,1)
                    newVar
                }
                | Some(locVars) => {
                    switch typToNextLocVarIdx->Belt_HashMapInt.get(typ) {
                        | None => Exn.raiseError("None == typToNextLocVarIdx->Belt_HashMapInt.get(typ)")
                        | Some(idx) => {
                            if (locVars->Array.length <= idx) {
                                let newVar = createNewVar(typ)
                                locVars->Array.push(newVar)
                                typToNextLocVarIdx->Belt_HashMapInt.set(typ,locVars->Array.length)
                                newVar
                            } else {
                                let existingVar = locVars->Array.getUnsafe(idx)
                                typToNextLocVarIdx->Belt_HashMapInt.set(typ,idx+1)
                                existingVar
                            }
                        }
                    }
                }
            }
        }

        let resetCtxLocVars = () => {
            typToNextLocVarIdx->Belt_HashMapInt.keysToArray->Array.forEach(typ => {
                typToNextLocVarIdx->Belt_HashMapInt.set(typ,0)
            })
        }

        let asrtVarsToLocVars = (asrtVarTypes:array<int>):array<int> => {
            asrtVarTypes->Array.map(getCtxLocVar)
        }

        let asrtIntToCtxInt = (i:int,asrtVarToLocVar:array<int>):int => {
            if (i < 0) {
                i
            } else {
                asrtVarToLocVar->Array.getUnsafe(i)
            }
        }

        let maxNumOfVars = ref(0)

        let asrtExprs:Belt_HashMapString.t<{"ctxExpr":array<int>, "ctxVarToAsrtVar":Belt_HashMapInt.t<int>}> = Belt_HashMapString.make(~hintSize=1000)
        ctx->forEachFrame(frame => {
            resetCtxLocVars()
            let asrtVarToLocVar = asrtVarsToLocVars(frame.varTypes)
            asrtExprs->Belt_HashMapString.set(
                frame.label,
                {
                    "ctxExpr": frame.asrt->Array.map(asrtIntToCtxInt(_,asrtVarToLocVar)),
                    "ctxVarToAsrtVar": 
                        asrtVarToLocVar->Array.mapWithIndex((ctxVar,asrtVar) => (ctxVar,asrtVar))->Belt_HashMapInt.fromArray
                }
            )            
            if (maxNumOfVars.contents < frame.numOfVars) {
                maxNumOfVars := frame.numOfVars
            }
            None
        })->ignore
        let asrtExprsToProve = asrtExprs->Belt_HashMapString.valuesToArray
            ->Array.map(obj => obj["ctxExpr"])
            ->Expln_utils_common.sortInPlaceWith(compareExprBySize->comparatorInverse)

        Console.log2(`maxNumOfVars`, maxNumOfVars.contents)

        // let asrtExprStr = asrtExprs->Array.map(ctxIntsToStrExn(ctx, _))->Array.joinUnsafe("\n")
        // Expln_utils_files.writeStringToFile(asrtExprStr, "./asrtExprStr.txt")

        let numOfExpr = asrtExprsToProve->Array.length
        let from = 0
        let to_ = from + numOfExpr
        let exprsToSyntaxProve = asrtExprsToProve->Js.Array2.slice(~start=from,~end_=to_)
            ->Array.map(expr => expr->Js_array2.sliceFrom(1))
        let frms = prepareFrmSubsData(~ctx, ())
        let parenCnt = MM_provers.makeParenCnt(~ctx, ~parens)

        let totalSize =exprsToSyntaxProve->Array.reduce(
            0,
            (size,expr) => {
                size + expr->Array.length
            }
        )
        Console.log2(`totalSize`, totalSize)

        let startMs = getCurrMillis()
        let lastPct = ref(startMs)
        let frameRestrict:MM_wrk_settings.frameRestrict = {
            useDisc:true,
            useDepr:true,
            useTranDepr:true,
        }
        log(`started proving syntax (from = ${from->Belt.Int.toString}, to = ${(to_-1)->Belt.Int.toString})`)

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
            },
            ()
        )

        //then
        let endMs = getCurrMillis()
        log(`Overall duration (sec): ${durationToSecondsStr(startMs, endMs)}` )

        // Expln_utils_files.writeStringToFile(proofTree->ptPrintStats, "./unprovedNodes.txt")

        let unprovedAsrtExprs = asrtExprsToProve
            ->Array.filter(expr => proofTree->ptGetSyntaxProof(expr->Js_array2.sliceFrom(1))->Belt_Option.isNone)
        // let unprovedAsrtExprStr = unprovedAsrtExprs->Array.map(ctxIntsToStrExn(ctx, _))->Array.joinUnsafe("\n")
        // Expln_utils_files.writeStringToFile(unprovedAsrtExprStr, "./unprovedAsrtExprStr.txt")
        assertEqMsg(unprovedAsrtExprs->Array.length, 0, "unprovedAsrtExprs->Array.length = 0")

        let ctxIntToAsrtInt = label => {
            let ctxVarToAsrtVar = (asrtExprs->Belt_HashMapString.get(label)->Belt.Option.getExn)["ctxVarToAsrtVar"]
            i => {
                if (i < 0) {
                    i
                } else {
                    ctxVarToAsrtVar->Belt_HashMapInt.get(i)->Belt.Option.getExn
                }
            }
        }

        let syntaxTrees:Belt_HashMapString.t<asrtSyntaxTreeNode> = 
            Belt_HashMapString.make(~hintSize=asrtExprs->Belt_HashMapString.size)
        asrtExprs->Belt_HashMapString.forEach((label,obj) => {
            switch buildAsrtSyntaxTree(proofTree->ptGetSyntaxProof(obj["ctxExpr"]->Js_array2.sliceFrom(1))->Belt_Option.getExn, ctxIntToAsrtInt(label)) {
                | Error(msg) => Exn.raiseError("Could not build an asrt syntax tree: " ++ msg)
                | Ok(syntaxTree) => {
                    syntaxTrees->Belt_HashMapString.set(label, syntaxTree)
                }
            }
        })
        Console.log2(`syntaxTrees->Belt_HashMapString.size`, syntaxTrees->Belt_HashMapString.size)
        // let asrtToPrint = "sylcom"
        // Console.log2(`${asrtToPrint}:`, syntaxTrees->Belt_HashMapString.get(asrtToPrint)->Expln_utils_common.stringify)
        let ctxExprStr = "( ( ch -> ph ) -> th )"
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
                        syntaxTrees->Belt_HashMapString.forEach((label,asrtTree) => {
                            if (
                                MM_asrt_syntax_tree.unifyMayBePossible(
                                    ~asrtExpr=asrtTree,
                                    ~ctxExpr=ctxSyntaxTree,
                                    ~isMetavar = _ => true,
                                )
                            ) {
                                let continue = ref(true)
                                let foundSubs = MM_asrt_syntax_tree.unifSubsMake()
                                MM_asrt_syntax_tree.unify(
                                    ~asrtExpr=asrtTree,
                                    ~ctxExpr=ctxSyntaxTree,
                                    ~isMetavar = _ => true,
                                    ~foundSubs,
                                    ~continue,
                                )
                                if (continue.contents && foundSubs->MM_asrt_syntax_tree.unifSubsSize > 1) {
                                    Console.log(`found match: ${label}`)
                                }
                            }
                        })
                    }
                }
            }
        }
        Expln_test.stopTimer("find match")
    })
})