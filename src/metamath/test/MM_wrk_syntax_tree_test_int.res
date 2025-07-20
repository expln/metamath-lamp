open Expln_test
open MM_parser
open MM_context
open MM_provers
open MM_substitution
open Common
open Expln_utils_common

let mmFilePath = "./src/metamath/test/resources/set._mm"

let getCurrMillis = () => Date.make()->Date.getTime
let durationToSeconds = (start,end):int => ((end -. start) /. 1000.0)->Belt_Float.toInt
let durationToSecondsStr = (start,end):string => durationToSeconds(start,end)->Belt.Int.toString
let compareExprBySize:comparator<expr> = comparatorByInt(Array.length(_))

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

describe("doBuildSyntaxTreesForAllAssertions", _ => {

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
        let ctx = ctx->ctxOptimizeForProver(
            ~parens, ~removeAsrtDescr=true, ~removeProofs=true, ~updateUsageCntForFrames=false
        )
        let frms = prepareFrmSubsData(~ctx)
        let parenCnt = makeParenCnt(~ctx, ~parens)
        let (_,syntaxTypes) = MM_wrk_pre_ctx_data.findTypes(ctx)
        let frameRestrict:MM_wrk_settings.frameRestrict = { useDisc:false, useDepr:true, useTranDepr:true, }

        let startMs = getCurrMillis()
        let lastPct = ref(startMs)
        log(`started proving syntax, number of expressions = ${frms->frmsGetAll->Array.length->Int.toString}`)


        //when
        let syntaxTreesArr = MM_wrk_syntax_tree.doBuildSyntaxTreesForAllAssertions(
            ~ctx,
            ~frms,
            ~parenCnt,
            ~allowedFrmsInSyntax = frameRestrict,
            ~syntaxTypes,
            ~onProgress = pct => {
                let currMs = getCurrMillis()
                log(`proving syntax: ${pct->floatToPctStr} - ${durationToSecondsStr(lastPct.contents, currMs)} sec`)
                lastPct := currMs
            },
        )

        //then
        let endMs = getCurrMillis()
        log(`Overall duration (sec): ${durationToSecondsStr(startMs, endMs)}` )

        let syntaxTrees = Belt_HashMapString.fromArray(syntaxTreesArr)

        let unprovedAsrts = frms->frmsGetAll
            ->Array.map(frm => frm.frame)
            ->Array.filter(frame => !(syntaxTrees->Belt_HashMapString.has(frame.label)))
        // let unprovedAsrtsStr = unprovedAsrts
        //     ->Array.map(frame => frmIntsToStrExn(ctx, frame, frame.asrt))
        //     ->Array.joinUnsafe("\n")
        // Expln_utils_files.writeStringToFile(unprovedAsrtsStr, "./unprovedAsrtsStr.txt")
        assertEqMsg(unprovedAsrts->Array.length, 0, "unprovedAsrts->Array.length = 0")
            
        // let asrtToPrint = "mdsymlem8"
        // Console.log(`--- ${asrtToPrint} ------------------------------------------------`)
        // syntaxTrees->Belt_HashMapString.get(asrtToPrint)->Option.getExn->Subtree->printSyntaxTree
        // Console.log(`-------------------------------------------------------------------`)

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
                        // Console.log(`--- ${ctxExprStr} ------------------------------------------------`)
                        // Subtree(ctxSyntaxTree)->printSyntaxTree
                        // Console.log(`-------------------------------------------------------------------`)
                        let ctxDisj = ctx->getAllDisj
                        let foundSubs = MM_asrt_syntax_tree.unifSubsMake()
                        syntaxTrees->Belt_HashMapString.forEach((label,asrtTree) => {
                            let frame = ctx->getFrameExn(label)
                            MM_asrt_syntax_tree.unifSubsReset(foundSubs)
                            if (
                                MM_asrt_syntax_tree.unify(
                                    ~asrtDisj=frame.disj,
                                    ~ctxDisj,
                                    ~asrtExpr=asrtTree,
                                    ~ctxExpr=ctxSyntaxTree,
                                    ~isMetavar = _ => true,
                                    ~foundSubs,
                                )
                            ) {
                                matchedAsrts->Array.push(label)
                                // Console.log(`${label}: ${ctx->frmIntsToStrExn(frame, frame.asrt)}`)
                            }
                        })
                    }
                }
            }
        }
        Expln_test.stopTimer("find match")
        // matchedAsrts->Array.sort(strCmp)
        // matchedAsrts->Array.forEach(Console.log)
        assertEqMsg(matchedAsrts->Array.length, 10023, "matchedAsrts->Array.length")
    })
})