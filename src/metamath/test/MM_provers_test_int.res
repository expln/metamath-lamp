open Expln_test
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier
open MM_int_test_utils
open MM_provers
open MM_substitution
open MM_parenCounter
open Common

let mmFilePath = "./src/metamath/test/resources/set._mm"

let getCurrMillis = () => Js.Date.make()->Js.Date.getTime
let durationToSeconds = (start,end):int => ((end -. start) /. 1000.0)->Belt_Float.toInt
let durationToSecondsStr = (start,end):string => durationToSeconds(start,end)->Belt.Int.toString

let log = msg => Js.Console.log(`${currTimeStr()} ${msg}`)

describe("proveSyntaxTypes", _ => {

    it("finds syntax proofs for each assertion in set.mm", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        // let progressTracker = testProgressTrackerMake(
        //     ~step=0.01, 
        //     ~maxCnt = countFrames(ast, ()),
        // )

        let ctx = ast->loadContext(
            ~descrRegexToDisc = "\\(New usage is discouraged\\.\\)"->strToRegex->Belt_Result.getExn,
            ()
        )
        let parens = "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( )) [b /b"
        ctx->moveConstsToBegin(parens)
        ctx->openChildContext

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
                        | None => Js.Exn.raiseError("None == typToNextLocVarIdx->Belt_HashMapInt.get(typ)")
                        | Some(idx) => {
                            if (locVars->Js_array2.length <= idx) {
                                let newVar = createNewVar(typ)
                                locVars->Js_array2.push(newVar)->ignore
                                typToNextLocVarIdx->Belt_HashMapInt.set(typ,locVars->Js_array2.length)
                                newVar
                            } else {
                                let existingVar = locVars[idx]
                                typToNextLocVarIdx->Belt_HashMapInt.set(typ,idx+1)
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
        asrtExprs->Js.Array2.sortInPlaceWith((a,b) => a->Js_array2.length - b->Js_array2.length)->ignore

        // let asrtExprStr = asrtExprs->Js.Array2.map(ctx->ctxIntsToStrExn)->Js.Array2.joinWith("\n")
        // Expln_utils_files.writeStringToFile(asrtExprStr, "./asrtExprStr.txt")

        let startMs = getCurrMillis()
        let lastPct = ref(startMs)
        log(`started proving syntax`)
        proveSyntaxTypes(
            ~wrkCtx=ctx,
            ~frms=prepareFrmSubsData(~ctx, ()),
            ~frameRestrict={
                useDisc:false,
                useDepr:true,
                useTranDepr:true,
            },
            ~parenCnt=parenCntMake(ctx->ctxStrToIntsExn(parens), ()),
            ~exprs=asrtExprs->Js_array2.map(expr => expr->Js_array2.sliceFrom(1)),
            ~syntaxTypes=ctx->ctxStrToIntsExn("wff class setvar"),
            ~onProgress=pct=>{
                let currMs = getCurrMillis()
                log(`proving syntax: ${pct->floatToPctStr} - ${durationToSecondsStr(lastPct.contents, currMs)} sec`)
                lastPct := currMs
            },
            ()
        )->ignore
        let endMs = getCurrMillis()
        log(`Overall duration (sec): ${durationToSecondsStr(startMs, endMs)}` )
    })
})