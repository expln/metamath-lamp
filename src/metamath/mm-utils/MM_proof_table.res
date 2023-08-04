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

let leftPad = (~content:string, ~char:string, ~totalLen:int):string => {
    let contentLen = content->Js_string2.length
    if (totalLen <= contentLen) {
        content
    } else {
        Js_string2.repeat(char, totalLen - contentLen) ++ content
    }
}

let exprSourceToArgsStr = src => {
    switch src {
        | Hypothesis(_) => ""
        | Assertion({args}) => args->Js_array2.map(i=>i+1)->Js_array2.joinWith(",")
    }
}

let exprSourceToLabelStr = src => {
    switch src {
        | Hypothesis({label}) => label
        | Assertion({label}) => label
    }
}

let maxLength = (arr:array<string>):int => {
    arr->Js.Array2.map(Js_string2.length)->Js.Array2.reduce(Js_math.max_int, 0)
}

let proofTableToArrStr = (ctx:mmContext,tbl:proofTable):array<string> => {
    let srcsArgs = tbl->Js_array2.map(r => r.proof->exprSourceToArgsStr)
    let srcsLabels = tbl->Js_array2.map(r => r.proof->exprSourceToLabelStr)
    let exprs = tbl->Js_array2.map(r => ctx->ctxIntsToStrExn(r.expr))

    let maxNumOfDigits = tbl->Js_array2.length->Belt.Int.toFloat->Js_math.log10->Js_math.floor_int + 1
    let numColWidth = maxNumOfDigits + 1
    let argsColWidth = maxLength(srcsArgs) + 1
    let labelColWidth = maxLength(srcsLabels) + 1

    tbl->Js_array2.mapi((_,i) => {
        leftPad(~content=Belt_Int.toString(i+1), ~char=" ", ~totalLen=numColWidth)
            ++ "| " ++ rightPad(~content=srcsArgs[i], ~char=" ", ~totalLen=argsColWidth)
            ++ "| " ++ rightPad(~content=srcsLabels[i], ~char=" ", ~totalLen=labelColWidth)
            ++ "| " ++ exprs[i]
    })
}

let proofTableToStr = (ctx,tbl,title):string => {
    let proofTableArrStr = proofTableToArrStr(ctx,tbl)
    let tableWidth = maxLength(proofTableArrStr)
    rightPad(~content=`--- ${title} `, ~char="-", ~totalLen=tableWidth) ++ "\n"
        ++ proofTableArrStr->Js_array2.joinWith("\n") ++ "\n"
        ++ rightPad(~content="", ~char="-", ~totalLen=maxLength(proofTableArrStr))
}

let proofTablePrint = (ctx,tbl,title):unit => Js.Console.log(proofTableToStr(ctx,tbl,title))

let traverseIdxsInRpnOrder = (tbl:proofTable,rootIdx:int,~onUse:int=>unit,~onReuse:int=>unit) => {
    let saved = Belt_HashSetInt.make(~hintSize=64)
    Expln_utils_data.traverseTree(
        (),
        rootIdx,
        (_, idx) => {
            switch tbl[idx].proof {
                | Hypothesis(_) => None
                | Assertion({args}) => {
                    if (saved->Belt_HashSetInt.has(idx)) {
                        None
                    } else {
                        Some(args)
                    }
                }
            }
        },
        ~postProcess = (_, idx) => {
            switch tbl[idx].proof {
                | Hypothesis(_) => onUse(idx)
                | Assertion(_) => {
                    if (!(saved->Belt_HashSetInt.has(idx))) {
                        saved->Belt_HashSetInt.add(idx)
                        onUse(idx)
                    } else {
                        onReuse(idx)
                    }
                }
            }
            None
        },
        ()
    )->ignore
}

let collectReusedIdxs = (tbl,rootIdx):Belt_HashSetInt.t => {
    let reused = Belt_HashSetInt.make(~hintSize=64)
    traverseIdxsInRpnOrder(tbl,rootIdx,
        ~onUse = _ => (),
        ~onReuse = idx => reused->Belt_HashSetInt.add(idx)
    )
    reused
}

let createProof = (mandHyps:array<hypothesis>, tbl:proofTable, rootIdx:int):proof => {
    let tblLen = tbl->Js_array2.length
    if (tblLen <= rootIdx) {
        raise(MmException({msg:`tblLen <= rootIdx`}))
    }
    let mandHypLen = mandHyps->Js.Array2.length
    let mandHypLabelToInt = Belt_HashMapString.fromArray(
        mandHyps->Js_array2.mapi(({label}, i) => (label, i+1))
    )
    let labels = []
    let labelToIntMap = Belt_HashMapString.make(~hintSize=64)
    let labelToInt = (label:string):int => {
        switch mandHypLabelToInt->Belt_HashMapString.get(label) {
            | Some(i) => i
            | None => {
                switch labelToIntMap->Belt_HashMapString.get(label) {
                    | Some(i) => i
                    | None => {
                        labels->Js.Array2.push(label)->ignore
                        let res = mandHypLen + labels->Js.Array2.length
                        labelToIntMap->Belt_HashMapString.set(label, res)
                        res
                    }
                }
            }
        }
    }
    let reusedIdxs = collectReusedIdxs(tbl,rootIdx)
    let reusedIdxToInt = Belt_HashMapInt.make(~hintSize=64)
    let proofSteps = []
    traverseIdxsInRpnOrder(tbl,rootIdx,
        ~onUse = idx => {
            let stepNum = switch tbl[idx].proof {
                | Hypothesis({label}) | Assertion({label}) => labelToInt(label)
            }
            proofSteps->Js_array2.push(stepNum)->ignore
            if (reusedIdxs->Belt_HashSetInt.has(idx)) {
                proofSteps->Js_array2.push(0)->ignore
                reusedIdxToInt->Belt_HashMapInt.set(idx, reusedIdxToInt->Belt_HashMapInt.size + 1)
            }
        },
        ~onReuse = idx => {
            proofSteps->Js_array2.push(-(reusedIdxToInt->Belt_HashMapInt.get(idx)->Belt_Option.getExn))->ignore
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

let createProofTableFromProof = (~proofNode:proofNode, ~mergeSameRows:bool=true, ()):proofTable => {
    let nodeIdToIdx = Belt_HashMapInt.make(~hintSize=proofNode->proofNodeGetId)
    let tbl = []

    let getIdxByNodeId = (nodeId:int):option<int> => nodeIdToIdx->Belt_HashMapInt.get(nodeId)

    let getIdxByNodeIdExn = (nodeId:int):int => {
        switch getIdxByNodeId(nodeId) {
            | None => raise(MmException({ msg:`Could not determine idx by nodeId in createProofTableFromProof().` }))
            | Some(idx) => idx
        }
    }

    let saveExprToTblWithoutChecks = (nodeId:int,expr:expr,proof:exprSource):unit => {
        let idx = tbl->Js_array2.push({expr, proof})-1
        nodeIdToIdx->Belt_HashMapInt.set(nodeId,idx)
    }

    let saveExprToTbl = (nodeId:int,expr:expr,proof:exprSource):unit => {
        if (getIdxByNodeId(nodeId)->Belt_Option.isSome) {
            raise(MmException({ msg:`getIdxByNodeId(nodeId)->Belt_Option.isSome in createProofTableFromProof()` }))
        }
        if (mergeSameRows) {
            switch tbl->Js.Array2.findIndex(r => r.expr->exprEq(expr) && r.proof == proof) {
                | -1 => saveExprToTblWithoutChecks(nodeId,expr,proof)
                | idx => nodeIdToIdx->Belt_HashMapInt.set(nodeId, idx)
            }
        } else {
            saveExprToTblWithoutChecks(nodeId,expr,proof)
        }
    }

    Expln_utils_data.traverseTree(
        (),
        proofNode,
        (_,node) => {
            switch node {
                | Hypothesis(_) => None
                | Calculated({args}) => Some(args)
            }
        },
        ~postProcess = (_, node) => {
            switch node {
                | Hypothesis({id,hypLabel,expr}) => {
                    if (getIdxByNodeId(id)->Belt_Option.isNone) {
                        saveExprToTbl(id, expr, Hypothesis({label:hypLabel}))
                    }
                }
                | Calculated({id,args,asrtLabel,expr}) => {
                    if (getIdxByNodeId(id)->Belt_Option.isNone) {
                        saveExprToTbl(
                            id,
                            expr, 
                            Assertion({
                                label:asrtLabel,
                                args: args->Js_array2.map(argNode => argNode->proofNodeGetId->getIdxByNodeIdExn)
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