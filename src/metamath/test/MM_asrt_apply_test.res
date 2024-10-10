open Expln_test
open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open Common

type labeledExpr = {
    label:string,
    expr:expr
}

let createEmptyFrame = (label:string):frame => {
    {
        ord:0,
        isAxiom:false,
        disj: Belt_MapInt.empty,
        hyps: [],
        asrt: [],
        label,
        frameVarToSymb: [],
        varTypes: [],
        varHyps: [],
        numOfVars: 0,
        numOfArgs: 0,
        descr:None,
        proof:None,
        isDisc:false,
        isDepr:false,
        isTranDepr:false,
        dbg: None,
    }
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
                res->Array.push(comb->Array.joinUnsafe(" "))
                Continue
            },
            ~combCntMax=10000,
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
            ~combCntMax=10000,
            ~combinationConsumer = comb => {
                res->Array.push(comb->Array.joinUnsafe(" "))
                if (comb->Array.getUnsafe(0) == 1 && comb->Array.getUnsafe(1) == -1) {
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
            ~combCntMax=10000,
            ~combinationConsumer = comb => {
                res->Array.push(comb->Array.joinUnsafe(" "))
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
            ~combCntMax=10000,
            ~combinationConsumer = comb => {
                res->Array.push(comb->Array.joinUnsafe(" "))
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
    ~isFrameAllowed:frame=>bool=_=>true,
    ~allowEmptyArgs:bool=true,
    ~allowNewDisjForExistingVars:bool=false,
    ~result:option<string>=?,
    ~fileWithExpectedResult:string,
    ()
) => {
    let printApplyAssertionResult = (workCtx, statements:array<labeledExpr>, res:applyAssertionResult):string => {
        workCtx->openChildContext
        let workVarHypLabels = generateNewLabels(~ctx=workCtx, ~prefix="workVar", ~amount=res.newVarTypes->Array.length, ())
        let workVarTypes = res.newVarTypes->Array.map(ctxIntToSymExn(workCtx, _))
        let workVarNames = generateNewVarNames(~ctx=workCtx, ~types=res.newVarTypes, ~typeToPrefix=Belt_MapString.empty, ())

        workCtx->applySingleStmt(Var({symbols:workVarNames}), ())
        workVarHypLabels->Array.forEachWithIndex((label,i) => {
            workCtx->applySingleStmt(Floating({label, expr:[workVarTypes->Array.getUnsafe(i), workVarNames->Array.getUnsafe(i)]}), ())
        })
        let args = []
        let argLabels = []
        let frame = res.frame
        frame.hyps->Array.forEach(hyp => {
            if (hyp.typ == E) {
                let argExpr = applySubs(
                    ~frmExpr=hyp.expr,
                    ~subs=res.subs,
                    ~createWorkVar=_=>raise(MmException({msg:`Cannot create work var in testApplyAssertions[1]`}))
                )
                switch statements->Array.find(({expr}) => exprEq(expr,argExpr)) {
                    | Some({label}) => {
                        args->Array.push(`[${label}]`)
                        argLabels->Array.push(label)
                    }
                    | None => {
                        let newStmtLabel = generateNewLabels(~ctx=workCtx, ~prefix="provable", ~amount=1, ())
                        let label = newStmtLabel->Array.getUnsafe(0)
                        let exprArrStr = argExpr->Array.map(ctxIntToSymExn(workCtx, _))
                        workCtx->applySingleStmt(Provable({
                            label, 
                            expr:exprArrStr,
                            proof:Some(Uncompressed({labels:[]}))
                        }), ())
                        args->Array.push(`${label}: ${exprArrStr->Array.joinUnsafe(" ")}`)
                        argLabels->Array.push(label)
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

        let workVarsStr = if (workVarHypLabels->Array.length == 0) {
            ""
        } else {
            "    " ++ workVarHypLabels->Array.mapWithIndex((label,i) => {
                `${label} ${workVarTypes->Array.getUnsafe(i)} ${workVarNames->Array.getUnsafe(i)}`
            })->Array.joinUnsafe("\n    ")
        }
        let argsStr = if (args->Array.length > 0) {
            "    " ++ args->Array.joinUnsafe("\n    ")
        } else {
            ""
        }
        let proofStr = `:${argLabels->Array.joinUnsafe(",")}:${res.frame.label}`
        `------------------------\n` ++ 
            `Work variables:\n${workVarsStr}\nArguments:\n${argsStr}\nProof:\n    ${proofStr}\n` ++
            `Result:\n    ${asrtExprStr}\n\n`
    }

    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
    let preCtx = loadContext(ast, ~stopBefore, ~stopAfter, ())
    additionalStatements->Array.forEach(stmt => preCtx->applySingleStmt(stmt, ()))
    let parens = "( ) { } [ ]"
    let workCtx = createContext(~parent=preCtx, ())
    let workCtx = workCtx->ctxOptimizeForProver(~parens, ())
    let frms = prepareFrmSubsData(~ctx=workCtx, ())
    let parenCnt = MM_provers.makeParenCnt(~ctx=workCtx, ~parens)

    let actualResults:Belt_MutableMapString.t<array<string>> = Belt_MutableMapString.make()
    let stmtsForAppl = statements->Array.map(((_,exprStr)) => ctxStrToIntsExn(workCtx,exprStr))
    let statements = statements->Array.map(((label,exprStr)) => {
        {
            label, 
            expr:exprStr->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(workCtx,_)
        }
    })

    //when
    applyAssertions(
        ~maxVar = workCtx->getNumOfVars-1,
        ~isDisjInCtx = isDisj(workCtx, ...),
        ~frms,
        ~statements = stmtsForAppl,
        ~parenCnt,
        ~isFrameAllowed,
        ~allowEmptyArgs,
        ~allowNewDisjForExistingVars,
        ~result=?result->Belt_Option.map(str => str->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(workCtx,_)),
        ~onMatchFound = res => {
            switch actualResults->Belt_MutableMapString.get(res.frame.label) {
                | None => {
                    actualResults->Belt_MutableMapString.set(res.frame.label, [printApplyAssertionResult(workCtx, statements, res)])
                }
                | Some(arr) => {
                    arr->Array.push(printApplyAssertionResult(workCtx, statements, res))
                }
            }
            // Console.log("onMatchFound ------------------------------------------------------------------")
            // Console.log(printApplyAssertionResult(res))
            Continue
        },
        ()
    )

    //then
    let actualResultsStr = actualResults->Belt_MutableMapString.keysToArray
        ->Js_array2.sortInPlace
        ->Array.map(astrLabel => {
            switch actualResults->Belt_MutableMapString.get(astrLabel) {
                | None => failMsg("actualResults->Belt_MutableMapString.get(astrLabel) == None")
                | Some(arr) => arr->Array.joinUnsafe("\n")
            }
        })
        ->Array.joinUnsafe("\n")
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
            ~isFrameAllowed=frame=>frame.label=="mp",
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
            ~isFrameAllowed = frame => frame.label == "mp",
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
            ~isFrameAllowed = frame => frame.label == "asrt-without-vars",
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
    it("does not introduce broken disjoints", _ => {
        //https://github.com/expln/metamath-lamp/issues/166#issuecomment-1751814880
        testApplyAssertions(
            ~mmFilePath = "./src/metamath/test/resources/applAsrt-no-broken-disjoints._mm",
            ~statements = [
                ("1", "|- ( x = y -> x = y )"),
            ],
            ~result="|- ( E. x E. y x = y -> x = y )",
            ~allowEmptyArgs=false,
            ~allowNewDisjForExistingVars=true,
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/no-broken-disjoints.txt",
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame(""),
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
                    frame: createEmptyFrame("a"),
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
                    frame: createEmptyFrame("bc"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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
                    frame: createEmptyFrame("asrt"),
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