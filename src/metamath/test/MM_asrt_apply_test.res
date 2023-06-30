open Expln_test
open MM_parser
open MM_context
open MM_substitution
open MM_parenCounter
open MM_asrt_apply
open Common

type labeledExpr = {
    label:string,
    expr:expr
}

describe("iterateCombinations", _ => {
    it("iterates all possible combinations", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~debugLevel=0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            },
            ~errConsumer = _ => Continue
        )->ignore

        //then
        assertEq(
            res, 
            [
                "0 0",
                "0 1",
                "0 2",
                "1 0",
                "1 1",
                "1 2",
                "2 0",
                "2 1",
                "2 2",
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "1 -1",
                "2 -1",
            ]
        )
    })

    it("iterates all possible combinations until stoped", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~debugLevel=0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                if (comb[0] == 1 && comb[1] == -1) {
                    Stop
                } else {
                    Continue
                }
            },
            ~errConsumer = _ => Continue
        )->ignore

        //then
        assertEq(
            res, 
            [
                "0 0",
                "0 1",
                "0 2",
                "1 0",
                "1 1",
                "1 2",
                "2 0",
                "2 1",
                "2 2",
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "1 -1",
            ]
        )
    })

    it("iterates all applicable combinations only", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (s,h) => mod( (s+h)->Js.Math.abs_int, 2) == 1,
            ~debugLevel=0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            },
            ~errConsumer = _ => Continue
        )->ignore

        //then
        assertEq(
            res, 
            [
                "1 0",
                "1 2",
                "-1 0",
                "-1 2",
            ]
        )
    })

    it("doesn't iterate at all if there is at least one unmatched hypothesis", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,h) => h != 0,
            ~debugLevel=0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            },
            ~errConsumer = _ => Continue
        )->ignore

        //then
        assertEq( res, [ ] )
    })
})

let testApplyAssertions = (
    ~mmFilePath:string,
    ~stopBefore:string="",
    ~stopAfter:string="",
    ~additionalStatements:array<stmt>=[],
    ~statements:array<(string,string)>,
    ~frameFilter:frame=>bool=_=>true,
    ~result:option<string>=?,
    ~fileWithExpectedResult:string,
    ()
) => {
    let printApplyAssertionResult = (workCtx, statements:array<labeledExpr>, res:applyAssertionResult):string => {
        workCtx->openChildContext
        let maxWorkCtxVar = workCtx->getNumOfVars - 1
        let workVarHypLabels = generateNewLabels(~ctx=workCtx, ~prefix="workVar", ~amount=res.newVarTypes->Js_array2.length, ())
        let workVarTypes = res.newVarTypes->Js_array2.map(workCtx->ctxIntToSymExn)
        let workVarNames = generateNewVarNames(~ctx=workCtx, ~types=res.newVarTypes, ~typeToPrefix=Belt_MapString.empty, ())
        let disjArrStr = []
        res.newDisj->disjForEachArr(disj => {
            disjArrStr->Js.Array2.push(
                "[ " ++ 
                disj->Js.Array2.map(v => {
                    if (v <= maxWorkCtxVar) {workCtx->ctxIntToSymExn(v)} else {workVarNames[v-maxWorkCtxVar-1]}
                })->Js_array2.joinWith(" ") ++ 
                " ]"
            )->ignore
        })
        let disjStr = if (disjArrStr->Js.Array2.length == 0) {""} else {"    " ++ disjArrStr->Js.Array2.joinWith("\n    ")}

        workCtx->applySingleStmt(Var({symbols:workVarNames}))
        workVarHypLabels->Js.Array2.forEachi((label,i) => {
            workCtx->applySingleStmt(Floating({label, expr:[workVarTypes[i], workVarNames[i]]}))
        })
        let args = []
        let argLabels = []
        let frame = workCtx->getFrame(res.asrtLabel)->Belt_Option.getExn
        frame.hyps->Js_array2.forEach(hyp => {
            if (hyp.typ == E) {
                let argExpr = applySubs(
                    ~frmExpr=hyp.expr,
                    ~subs=res.subs,
                    ~createWorkVar=_=>raise(MmException({msg:`Cannot create work var in testApplyAssertions[1]`}))
                )
                switch statements->Js.Array2.find(({expr}) => exprEq(expr,argExpr)) {
                    | Some({label}) => {
                        args->Js_array2.push(`[${label}]`)->ignore
                        argLabels->Js_array2.push(label)->ignore
                    }
                    | None => {
                        let newStmtLabel = generateNewLabels(~ctx=workCtx, ~prefix="provable", ~amount=1, ())
                        let label = newStmtLabel[0]
                        let exprArrStr = argExpr->Js_array2.map(workCtx->ctxIntToSymExn)
                        workCtx->applySingleStmt(Provable({
                            label, 
                            expr:exprArrStr,
                            proof:Some(Uncompressed({labels:[]}))
                        }))
                        args->Js_array2.push(`${label}: ${exprArrStr->Js_array2.joinWith(" ")}`)->ignore
                        argLabels->Js_array2.push(label)->ignore
                    }
                }
            }
        })
        let asrtExprStr = workCtx->ctxIntsToStrExn(
            applySubs(
                ~frmExpr=frame.asrt,
                ~subs=res.subs,
                ~createWorkVar=_=>raise(MmException({msg:`Cannot create work var in testApplyAssertions[2]`}))
            )
        )
        workCtx->resetToParentContext

        let workVarsStr = if (workVarHypLabels->Js_array2.length == 0) {
            ""
        } else {
            "    " ++ workVarHypLabels->Js.Array2.mapi((label,i) => {
                `${label} ${workVarTypes[i]} ${workVarNames[i]}`
            })->Js_array2.joinWith("\n    ")
        }
        let argsStr = if (args->Js.Array2.length > 0) {
            "    " ++ args->Js_array2.joinWith("\n    ")
        } else {
            ""
        }
        let proofStr = `:${argLabels->Js_array2.joinWith(",")}:${res.asrtLabel}`
        `------------------------\n` ++ 
            `Work variables:\n${workVarsStr}\nDisjoints:\n${disjStr}\nArguments:\n${argsStr}\nProof:\n    ${proofStr}\n` ++
            `Result:\n    ${asrtExprStr}\n\n`
    }

    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
    let preCtx = loadContext(ast, ~stopBefore, ~stopAfter, ())
    let parens = "( ) { } [ ]"
    preCtx->moveConstsToBegin(parens)
    additionalStatements->Js_array2.forEach(preCtx->applySingleStmt)
    let workCtx = createContext(~parent=preCtx, ())
    let frms = prepareFrmSubsData(~ctx=workCtx, ())
    let parenCnt = parenCntMake(workCtx->ctxStrToIntsExn(parens), ())

    let actualResults:Belt_MutableMapString.t<array<string>> = Belt_MutableMapString.make()
    let stmtsForAppl = statements->Js_array2.map(((_,exprStr)) => ctxStrToIntsExn(workCtx,exprStr))
    let statements = statements->Js_array2.map(((label,exprStr)) => {
        {
            label, 
            expr:exprStr->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(workCtx,_)
        }
    })

    //when
    applyAssertions(
        ~maxVar = workCtx->getNumOfVars-1,
        ~isDisjInCtx = workCtx->isDisj,
        ~frms,
        ~statements = stmtsForAppl,
        ~parenCnt,
        ~frameFilter,
        ~result=?result->Belt_Option.map(str => str->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(workCtx,_)),
        ~onMatchFound = res => {
            switch actualResults->Belt_MutableMapString.get(res.asrtLabel) {
                | None => {
                    actualResults->Belt_MutableMapString.set(res.asrtLabel, [printApplyAssertionResult(workCtx, statements, res)])
                }
                | Some(arr) => {
                    arr->Js.Array2.push(printApplyAssertionResult(workCtx, statements, res))->ignore
                }
            }
            // Js.Console.log("onMatchFound ------------------------------------------------------------------")
            // Js.Console.log(printApplyAssertionResult(res))
            Continue
        },
        ()
    )

    //then
    let actualResultsStr = actualResults->Belt_MutableMapString.keysToArray
        ->Js_array2.sortInPlace
        ->Js_array2.map(astrLabel => {
            switch actualResults->Belt_MutableMapString.get(astrLabel) {
                | None => failMsg("actualResults->Belt_MutableMapString.get(astrLabel) == None")
                | Some(arr) => arr->Js_array2.joinWith("\n")
            }
        })
        ->Js_array2.joinWith("\n")
    let expectedResultStr = Expln_utils_files.readStringFromFile(fileWithExpectedResult)
        ->Js.String2.replaceByRe(%re("/\r/g"), "")
    if (actualResultsStr != expectedResultStr) {
        let fileWithActualResult = fileWithExpectedResult ++ ".actual"
        Expln_utils_files.writeStringToFile(actualResultsStr, fileWithActualResult)
        assertEq( fileWithActualResult, fileWithExpectedResult )
    }
}

describe("applyAssertions", _ => {
    let demo0 = "./src/metamath/test/resources/demo0._mm"
    let asrtWithoutVars = "./src/metamath/test/resources/asrt-without-vars._mm"
    it("applies assertions when there are no statements", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [],
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-no-statements.txt",
            ()
        )
    })
    it("applies assertions when there is one statement, for modus ponens", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- ( t + 0 ) = t")
            ],
            ~frameFilter=frame=>frame.label=="mp",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement-mp.txt",
            ()
        )
    })
    it("applies assertions when there is one statement, for all assertions from demo0", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- ( t + 0 ) = t")
            ],
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement.txt",
            ()
        )
    })
    it("applies assertions when there is one statement and a result, for mp assertion from demo0", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- P")
            ],
            ~frameFilter = frame => frame.label == "mp",
            ~result="|- P",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement-with-result.txt",
            ()
        )
    })
    it("doesn't fail when there are no variables in the frame's assertion", _ => {
        testApplyAssertions(
            ~mmFilePath = asrtWithoutVars,
            ~stopAfter = "asrt-without-vars",
            ~additionalStatements = [],
            ~statements = [ ],
            ~frameFilter = frame => frame.label == "asrt-without-vars",
            ~result="|- T.",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/asrt-without-vars.txt",
            ()
        )
    })
    it("matches all non-blank hyps before blank ones to maximize number of bound variables", _ => {
        testApplyAssertions(
            ~mmFilePath = "./src/metamath/test/resources/applAsrt-correct-order-of-hyps-matching._mm",
            ~statements = [
                ("8", "|- 4 = ( ( 2 + 1 ) + 1 )"),
                ("4", "|- ( 2 + 2 ) = ( 2 + ( 1 + 1 ) )"),
            ],
            ~result="|- ( 2 + 2 ) = 4",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/correct-order-of-hyps-matching.txt",
            ()
        )
    })
})

describe("applyAssertionResultEq", _ => {
    it("returns false when both results have errors", _ => {
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:Some(UnifErr),
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:Some(UnifErr),
                }
            ),
            false
        )
    })
    it("returns false when only one result has error", _ => {
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:None,
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:Some(UnifErr),
                }
            ),
            false
        )
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:Some(UnifErr),
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:None,
                }
            ),
            false
        )
    })
    it("returns false when labels are different", _ => {
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "a",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:None,
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "bc",
                    subs: {
                        size: 0,
                        begins: [],
                        ends: [],
                        exprs: [],
                        isDefined: [],
                    },
                    err:None,
                }
            ),
            false
        )
    })
    it("returns false when subs are different", _ => {
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [1,2],
                        ends: [3,5],
                        exprs: [[1,2,3,4,5],[6,7,8,9,10,11,12]],
                        isDefined: [true,true],
                    },
                    err:None,
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,90,10,11;
                        begins: [1,2],
                        ends: [3,5],
                        exprs: [[1,2,3,4,5],[6,7,8,90,10,11,12]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            false
        )
    })
    it("returns true when labels and subs are same", _ => {
        assertEq(
            applyAssertionResultEq(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [1,2],
                        ends: [3,5],
                        exprs: [[1,2,3,4,5],[6,7,8,9,10,11,12]],
                        isDefined: [true,true],
                    },
                    err:None,
                },
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [0,4],
                        ends: [2,7],
                        exprs: [[2,3,4,50],[6,7,7,7,8,9,10,11]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            true
        )
    })
})

describe("applyAssertionResultHash", _ => {
    it("returns same hash for equal results", _ => {
        assertEq(
            applyAssertionResultHash(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [1,2],
                        ends: [3,5],
                        exprs: [[1,2,3,4,5],[6,7,8,9,10,11,12]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            93430539
        )
        assertEq(
            applyAssertionResultHash(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [0,4],
                        ends: [2,7],
                        exprs: [[2,3,4,50],[6,7,7,7,8,9,10,11]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            93430539
        )
    })
    it("returns different hashes for not equal results", _ => {
        assertEq(
            applyAssertionResultHash(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,9,10,11;
                        begins: [1,2],
                        ends: [3,5],
                        exprs: [[1,2,3,4,5],[6,7,8,9,10,11,12]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            93430539
        )
        assertEq(
            applyAssertionResultHash(
                {
                    newVars: [],
                    newVarTypes: [],
                    newDisj:disjMake(),
                    asrtLabel: "asrt",
                    subs: {
                        size: 2, //2,3,4; 8,90,10,11;
                        begins: [0,4],
                        ends: [2,7],
                        exprs: [[2,3,4,50],[6,7,7,7,8,90,10,11]],
                        isDefined: [true,true],
                    },
                    err:None,
                }
            ),
            93508380
        )
    })
})

describe("getNextHypIdxToMatch", _ => {
    it("all types: returns idx of the first non blank hyp in the beginning of matching", _ => {
        assertEq(getNextHypIdxToMatch(-1, [1, -1, 2, -1]), 0)
        assertEq(getNextHypIdxToMatch(-1, [-1, -1, 2, -1, 3]), 2)
    })
    it("all types: returns 0 in the beginning of matching if comb is empty", _ => {
        assertEq(getNextHypIdxToMatch(-1, []), 0)
    })
    it("all types: returns idx of the next non blank hyp in the middle of matching", _ => {
        assertEq(getNextHypIdxToMatch(0, [1, -1, 2, -1, 3]), 2)
        assertEq(getNextHypIdxToMatch(2, [1, -1, 2, -1, 3]), 4)
        assertEq(getNextHypIdxToMatch(2, [-1, -1, 2, -1, 3]), 4)
    })
    it("all types: returns idx of the first blank hyp in the middle of matching", _ => {
        assertEq(getNextHypIdxToMatch(4, [1, -1, 2, -1, 3]), 1)
        assertEq(getNextHypIdxToMatch(4, [-1, -1, 2, -1, 3]), 0)
    })
    it("all types: returns idx of the next blank hyp in the middle of matching", _ => {
        assertEq(getNextHypIdxToMatch(1, [1, -1, 2, -1, 3]), 3)
        assertEq(getNextHypIdxToMatch(0, [-1, -1, 2, -1, 3]), 1)
    })
    it("all types: returns comb.length in the end of matching", _ => {
        assertEq(getNextHypIdxToMatch(3, [1, -1, 2, -1, 3]), 5)
        assertEq(getNextHypIdxToMatch(3, [-1, -1, 2, -1, 3]), 5)
    })

    it("non-blanks only: returns idx of the first non blank hyp in the beginning of matching", _ => {
        assertEq(getNextHypIdxToMatch(-1, [1, 2, 3]), 0)
    })
    it("non-blanks only: returns idx of the next non blank hyp in the middle of matching", _ => {
        assertEq(getNextHypIdxToMatch(0, [1, 2, 3]), 1)
        assertEq(getNextHypIdxToMatch(1, [1, 2, 3]), 2)
    })
    it("non-blanks only: returns comb.length in the end of matching", _ => {
        assertEq(getNextHypIdxToMatch(2, [1, 2, 3]), 3)
    })
    
    it("blanks only: returns idx of the first blank hyp in the beginning of matching", _ => {
        assertEq(getNextHypIdxToMatch(-1, [-1, -1, -1]), 0)
    })
    it("blanks only: returns idx of the next blank hyp in the middle of matching", _ => {
        assertEq(getNextHypIdxToMatch(0, [-1, -1, -1]), 1)
        assertEq(getNextHypIdxToMatch(1, [-1, -1, -1]), 2)
    })
    it("blanks only: returns comb.length in the end of matching", _ => {
        assertEq(getNextHypIdxToMatch(2, [-1, -1, -1]), 3)
    })
})