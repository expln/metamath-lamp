open MM_context
open MM_parser
open MM_proof_verifier

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proof:exprSource,
}

type proofTable = array<proofRecord>

let rightPad = (~content:string, ~char:string, ~totalLen:int):string => {
    let contentLen = content->Js_string2.length
    if (totalLen <= contentLen) {
        content
    } else {
        content ++ Js_string2.repeat(char, totalLen - contentLen)
    }
}

let exprSourceToStr = src => {
    switch src {
        | Hypothesis({label}) => label
        | Assertion({args, label}) => {
            if (args->Js.Array2.length == 0) {
                ": " ++ label
            } else {
                args->Js_array2.map(i=>i+1)->Js_array2.joinWith(",") ++ " : " ++ label
            }
        }
    }
}

let maxLength = (arr:array<string>):int => {
    arr->Js.Array2.map(Js_string2.length)->Js.Array2.reduce(Js_math.max_int, 0)
}

let proofTableToArrStr = (ctx:mmContext,tbl:proofTable):array<string> => {
    let srcs = tbl->Js_array2.map(r => r.proof->exprSourceToStr)
    let exprs = tbl->Js_array2.map(r => ctx->ctxIntsToStrExn(r.expr))

    let maxNumOfDigits = tbl->Js_array2.length->Belt.Int.toFloat->Js_math.log10->Js_math.floor_int + 1
    let col1Width = maxNumOfDigits + 1
    let col2Width = maxLength(srcs) + 1

    tbl->Js_array2.mapi((_,i) => {
        rightPad(~content=Belt_Int.toString(i+1), ~char=" ", ~totalLen=col1Width)
            ++ "| " ++ rightPad(~content=srcs[i], ~char=" ", ~totalLen=col2Width)
            ++ "| " ++ exprs[i]
    })
}

let proofTableToStr = (ctx,tbl,title):string => {
    `--- TBL ${title} ---------------------------------------------------------------------------\n`
        ++ proofTableToArrStr(ctx,tbl)->Js_array2.joinWith("\n")
        ++ `\n--------------------------------------------------------------------------------------------`
}

let proofTablePrint = (ctx,tbl,title):unit => Js.Console.log(proofTableToStr(ctx,tbl,title))

let traverseRecordsInRpnOrder = (tbl,targetIdx,~onUse,~onReuse) => {
    let savedExprs = Belt_MutableSet.make(~id=module(ExprCmp))
    let reusedExprsSet = Belt_MutableSet.make(~id=module(ExprCmp))
    Expln_utils_data.traverseTree(
        (),
        tbl[targetIdx],
        (_, r) => {
            switch r.proof {
                | Hypothesis(_) => None
                | Assertion({args}) => if (savedExprs->Belt_MutableSet.has(r.expr)) { None } else { Some(args->Js_array2.map(a=>tbl[a])) }
            }
        },
        ~process = (_, r) => {
            switch r.proof {
                | Hypothesis(_) => onUse(r)
                | _ => ()
            }
            None
        },
        ~postProcess = (_, r) => {
            switch r.proof {
                | Assertion(_) => {
                    if (!(savedExprs->Belt_MutableSet.has(r.expr))) {
                        savedExprs->Belt_MutableSet.add(r.expr)
                        onUse(r)
                    } else {
                        let firstReusage = !(reusedExprsSet->Belt_MutableSet.has(r.expr))
                        if (firstReusage) {
                            reusedExprsSet->Belt_MutableSet.add(r.expr)
                        }
                        onReuse(r,firstReusage)
                    }
                }
                | _ => ()
            }
            None
        },
        ()
    )->ignore
}

let collectReusedExprs = (tbl,targetIdx):Belt_Set.t<expr, ExprCmp.identity> => {
    let reusedExprs = []
    traverseRecordsInRpnOrder(tbl,targetIdx,
        ~onUse = _ => (),
        ~onReuse = (r,firstReusage) => {
            if (firstReusage) {
                reusedExprs->Js_array2.push(r.expr)->ignore
            }
        }
    )
    Belt_Set.fromArray(reusedExprs, ~id=module(ExprCmp))
}

let createProof = (ctx:mmContext, tbl:proofTable, targetIdx:int):proof => {
    let tblLen = tbl->Js_array2.length
    if (tblLen <= targetIdx) {
        raise(MmException({msg:`tblLen <= targetIdx`}))
    }
    let mandHyps = getMandHyps(ctx, tbl[targetIdx].expr)
    let mandHypLabelToInt = Belt_MapString.fromArray(
        mandHyps->Js_array2.mapi(({label}, i) => (label, i+1))
    )
    let mandHypLen = mandHypLabelToInt->Belt_MapString.size
    let labels = []
    let labelToInt = label => {
        mandHypLen + switch labels->Js_array2.indexOf(label) {
            | -1 => labels->Js_array2.push(label)
            | i => i+1
        }
    }
    let reusedExprs = collectReusedExprs(tbl,targetIdx)
    let reusedExprToInt = Belt_MutableMap.make(~id=module(ExprCmp))
    let proofSteps = []
    traverseRecordsInRpnOrder(tbl,targetIdx,
        ~onUse = r => {
            let idx = switch r.proof {
                | Hypothesis({label}) => {
                    switch mandHypLabelToInt->Belt_MapString.get(label) {
                        | Some(i) => i
                        | None => labelToInt(label)
                    }
                }
                | Assertion({label}) => labelToInt(label)
            }
            proofSteps->Js_array2.push(idx)->ignore
            if (reusedExprs->Belt_Set.has(r.expr)) {
                proofSteps->Js_array2.push(0)->ignore
                reusedExprToInt->Belt_MutableMap.set(r.expr, reusedExprToInt->Belt_MutableMap.size + 1)
            }
        },
        ~onReuse = (r,_) => {
            proofSteps->Js_array2.push(-(reusedExprToInt->Belt_MutableMap.getExn(r.expr)))->ignore
        }
    )
    let labelsLastIdx = mandHypLen + labels->Js.Array2.length
    Compressed({
        labels,
        compressedProofBlock: proofSteps->Js_array2.map(i => {
            if (i == 0) {
                "Z"
            } else if (i < 0) {
                intToCompressedProofStr(labelsLastIdx - i)
            } else {
                intToCompressedProofStr(i)
            }
        })->Js_array2.joinWith("")
    })

}

let createProofTableFromProof = (proofNode:proofNode):proofTable => {
    let childrenReturnedFor = Belt_HashSet.make(~id = module(ExprHash), ~hintSize=16)
    let exprToNode = createExprToNode(proofNode)
    let exprToIdx = Belt_HashMap.make(~id = module(ExprHash), ~hintSize=16)
    let tbl = []

    let exprToStr = expr => "expr[" ++ expr->Js_array2.map(Belt_Int.toString)->Js.Array2.joinWith(" ") ++ "]"

    let getNodeByExpr = (expr:expr):proofNode => {
        Js.Console.log2("getNodeByExpr", exprToStr(expr))
        switch exprToNode->Belt_HashMap.get(expr) {
            | None => raise(MmException({ msg:`Could not determine node by expr in createProofTableFromProof().` }))
            | Some(node) => node
        }
    }

    let exprIsSavedToTbl = expr => {
        Js.Console.log2("exprIsSavedToTbl", exprToStr(expr))
        exprToIdx->Belt_HashMap.has(expr)
    }

    let saveExprToTbl = (expr:expr,proof:exprSource):unit => {
        Js.Console.log2("saveExprToTbl", exprToStr(expr))
        let idx = tbl->Js_array2.push({expr, proof})-1
        exprToIdx->Belt_HashMap.set(expr,idx)
    }

    let getIdxByExpr = (expr:expr):int => {
        Js.Console.log2("getIdxByExpr", exprToStr(expr))
        switch exprToIdx->Belt_HashMap.get(expr) {
            | None => {
                raise(MmException({ msg:`Could not determine idx by expr in createProofTableFromProof().` }))
            }
            | Some(idx) => idx
        }
    }

    Expln_utils_data.traverseTree(
        ref(0),
        proofNode->getExprFromNode,
        (depth,expr) => {
            Js.Console.log3(rightPad(~content="", ~char=" ", ~totalLen=depth.contents*4), "getChildren", exprToStr(expr))
            if (childrenReturnedFor->Belt_HashSet.has(expr)) {
                None
            } else {
                childrenReturnedFor->Belt_HashSet.add(expr)
                switch getNodeByExpr(expr) {
                    | Hypothesis(_) => None
                    | Calculated({args}) => Some( args->Js_array2.map(getExprFromNode) )
                }
            }
        },
        ~preProcess = (depth, expr) => {
            Js.Console.log3(rightPad(~content="", ~char=" ", ~totalLen=depth.contents*4), "preProcess", exprToStr(expr))
            depth.contents = depth.contents + 1
            None
        },
        ~postProcess = (depth, expr) => {
            depth.contents = depth.contents - 1
            Js.Console.log3(rightPad(~content="", ~char=" ", ~totalLen=depth.contents*4), "postProcess", exprToStr(expr))
            if (!exprIsSavedToTbl(expr)) {
                switch getNodeByExpr(expr) {
                    | Hypothesis({hypLabel}) => saveExprToTbl(expr, Hypothesis({label:hypLabel}))
                    | Calculated({args,asrtLabel}) => {
                        switch args->Js.Array2.find(argNode => argNode->getExprFromNode->exprEq(expr)) {
                            | None => ()
                            | Some(_) => raise(MmException({ msg:`Found!!!` }))
                        }
                        saveExprToTbl(
                            expr, 
                            Assertion({
                                label:asrtLabel,
                                args: args->Js_array2.map(argNode => argNode->getExprFromNode->getIdxByExpr)
                            })
                        )
                    }
                }
            }
            None
        },
        ()
    )->ignore
    tbl
}