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
    `--- ${title} ---------------------------------------------------------------------------\n`
        ++ proofTableToArrStr(ctx,tbl)->Js_array2.joinWith("\n")
        ++ `\n--------------------------------------------------------------------------------------------`
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

let createProofTableFromProof = (proofNode:proofNode):proofTable => {
    let nodeIdToIdx = Belt_HashMapInt.make(~hintSize=proofNode->proofNodeGetId)
    let tbl = []

    let getIdxByNodeId = (nodeId:int):option<int> => nodeIdToIdx->Belt_HashMapInt.get(nodeId)

    let getIdxByNodeIdExn = (nodeId:int):int => {
        switch getIdxByNodeId(nodeId) {
            | None => raise(MmException({ msg:`Could not determine idx by nodeId in createProofTableFromProof().` }))
            | Some(idx) => idx
        }
    }

    let saveExprToTbl = (nodeId:int,expr:expr,proof:exprSource):unit => {
        if (getIdxByNodeId(nodeId)->Belt_Option.isSome) {
            raise(MmException({ msg:`getIdxByNodeId(nodeId)->Belt_Option.isSome in createProofTableFromProof()` }))
        }
        let idx = tbl->Js_array2.push({expr, proof})-1
        nodeIdToIdx->Belt_HashMapInt.set(nodeId,idx)
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