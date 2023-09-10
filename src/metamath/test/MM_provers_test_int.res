open Expln_test
open MM_parser
open MM_context
open MM_provers
open MM_substitution
open MM_parenCounter
open Common
open MM_proof_tree
open Expln_utils_common

let mmFilePath = "./src/metamath/test/resources/set._mm"

let getCurrMillis = () => Js.Date.make()->Js.Date.getTime
let durationToSeconds = (start,end):int => ((end -. start) /. 1000.0)->Belt_Float.toInt
let durationToSecondsStr = (start,end):string => durationToSeconds(start,end)->Belt.Int.toString
let compareExprBySize = comparatorBy(Js_array2.length)

let log = msg => Js.Console.log(`${currTimeStr()} ${msg}`)

describe("proveSyntaxTypes", _ => {

    it("finds syntax proofs for each assertion in set.mm", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let ctx = ast->loadContext(
            ~descrRegexToDisc = "\\(New usage is discouraged\\.\\)"->strToRegex->Belt_Result.getExn,
            // ~debug=true,
            ()
        )
        let parens = "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( )) [b /b"
        ctx->moveConstsToBegin(parens)
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
                    // typToNextLocVarIdx->Belt_HashMapInt.set(typ,1)
                    typToNextLocVarIdx->Belt_HashMapInt.set(typ,0)
                    newVar
                }
                | Some(locVars) => {
                    switch typToNextLocVarIdx->Belt_HashMapInt.get(typ) {
                        | None => Js.Exn.raiseError("None == typToNextLocVarIdx->Belt_HashMapInt.get(typ)")
                        | Some(idx) => {
                            if (locVars->Js_array2.length <= idx) {
                                let newVar = createNewVar(typ)
                                locVars->Js_array2.push(newVar)->ignore
                                // typToNextLocVarIdx->Belt_HashMapInt.set(typ,locVars->Js_array2.length)
                                newVar
                            } else {
                                let existingVar = locVars[idx]
                                // typToNextLocVarIdx->Belt_HashMapInt.set(typ,idx+1)
                                existingVar
                            }
                        }
                    }
                }
            }
        }

        let resetCtxLocVars = () => {
            typToNextLocVarIdx->Belt_HashMapInt.toArray->Js.Array2.forEach(((typ,_)) => {
                typToNextLocVarIdx->Belt_HashMapInt.set(typ,0)
            })
        }

        let asrtIntToCtxInt = (i,frame):int => {
            if (i < 0) {
                i
            } else {
                getCtxLocVar(frame.varTypes[i])
            }
        }

        let asrtExprs:array<expr> = []
        ctx->forEachFrame(frame => {
            resetCtxLocVars()
            asrtExprs->Js.Array2.push(
                frame.asrt->Js_array2.map(asrtIntToCtxInt(_,frame))
            )->ignore
            None
        })->ignore
        asrtExprs->Js.Array2.sortInPlaceWith(compareExprBySize->comparatorInverse)->ignore

        // let asrtExprStr = asrtExprs->Js.Array2.map(ctx->ctxIntsToStrExn)->Js.Array2.joinWith("\n")
        // Expln_utils_files.writeStringToFile(asrtExprStr, "./asrtExprStr.txt")

        let numOfExpr = 100000
        let from = 0
        let to_ = from + numOfExpr
        let exprsToSyntaxProve = asrtExprs->Js.Array2.slice(~start=from,~end_=to_)
            ->Js_array2.map(expr => expr->Js_array2.sliceFrom(1))
        let frms= prepareFrmSubsData(~ctx, ())
        let parenCnt = parenCntMake(ctx->ctxStrToIntsExn(parens), ())

        let totalSize =exprsToSyntaxProve->Js_array2.reduce(
            (size,expr) => size + expr->Js_array2.length,
            0
        )
        Js.Console.log2(`totalSize`, totalSize)

        let startMs = getCurrMillis()
        let lastPct = ref(startMs)
        log(`started proving syntax (from = ${from->Belt.Int.toString}, to = ${(to_-1)->Belt.Int.toString})`)

        //when
        // let batchSize = 3000
        let batchSize = totalSize / 10
        let i = ref(0)
        while (i.contents < exprsToSyntaxProve->Js_array2.length) {
            let batch = []
            let sumLen = ref(0)
            while (sumLen.contents < batchSize && i.contents < exprsToSyntaxProve->Js_array2.length) {
                let expr = exprsToSyntaxProve[i.contents]
                batch->Js.Array2.push(expr)->ignore
                sumLen := sumLen.contents + expr->Js_array2.length
                i := i.contents + 1
            }
            let proofTree = proveSyntaxTypes(
                ~wrkCtx=ctx,
                ~frms,
                ~frameRestrict={
                    useDisc:false,
                    useDepr:true,
                    useTranDepr:true,
                },
                ~parenCnt,
                // ~exprs=exprsToSyntaxProve->Js_array2.slice(~start=i.contents, ~end_=i.contents+batchSize),
                ~exprs=batch,
                ~syntaxTypes,
                ~onProgress=pct=>{
                    let currMs = getCurrMillis()
                    log(`proving syntax: ${pct->floatToPctStr} - ${durationToSecondsStr(lastPct.contents, currMs)} sec`)
                    lastPct := currMs
                },
                ()
            )->ignore
            // i := i.contents + batchSize
        }

        //then
        let endMs = getCurrMillis()
        log(`Overall duration (sec): ${durationToSecondsStr(startMs, endMs)}` )

        // Expln_utils_files.writeStringToFile(proofTree->ptPrintStats, "./unprovedNodes.txt")

        // let unprovedAsrtExprs = asrtExprs
        //     ->Js.Array2.filter(expr => proofTree->ptGetSyntaxProof(expr->Js_array2.sliceFrom(1))->Belt_Option.isNone)
        // assertEqMsg(unprovedAsrtExprs->Js.Array2.length, 0, "unprovedAsrtExprs->Js.Array2.length = 0")

        // // let unprovedAsrtExprStr = unprovedAsrtExprs->Js.Array2.map(ctx->ctxIntsToStrExn)->Js.Array2.joinWith("\n")
        // // Expln_utils_files.writeStringToFile(unprovedAsrtExprStr, "./unprovedAsrtExprStr.txt")
    })
})