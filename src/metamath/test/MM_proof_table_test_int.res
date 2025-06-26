open Expln_test
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier
open MM_int_test_utils
open Common

let mmFilePath = "./src/metamath/test/resources/set._mm"

let compareCompressedProofsAfterRenumbering = (
    ~mandHypsNum:int,
    ~expectedLabels:array<string>,
    ~expectedBlock:string,
    ~actualLabels:array<string>,
    ~actualBlock:string,
): bool => {
    let actualLabelsSet = actualLabels->Belt_HashSetString.fromArray
    let expectedLabelsSet = expectedLabels->Belt_HashSetString.fromArray
    actualLabelsSet->Belt_HashSetString.forEach(actualLabel => {
        if (!(expectedLabelsSet->Belt_HashSetString.has(actualLabel))) {
            raise(MmException({msg:`!(expectedLabelsSet->Belt_HashSetString.has(actualLabel))`}))
        }
    })
    expectedLabelsSet->Belt_HashSetString.forEach(expectedLabel => {
        if (!(actualLabelsSet->Belt_HashSetString.has(expectedLabel))) {
            raise(MmException({msg:`!(actualLabelsSet->Belt_HashSetString.has(expectedLabel))`}))
        }
    })

    let renum = actualLabels->Array.mapWithIndex((actualLabel,i) => 
        (i, (expectedLabels->Array.indexOf(actualLabel)))
    )->Belt_HashMapInt.fromArray

    let minLabelIdx = mandHypsNum + 1
    let maxLabelIdx = mandHypsNum + actualLabels->Array.length
    let newBlock = actualBlock->compressedProofBlockToArray->Array.map(oldNumStr => {
        if (oldNumStr == "Z") {
            "Z"
        } else {
            let oldNum = compressedProofStrToInt(oldNumStr)
            let newNum = if (minLabelIdx <= oldNum && oldNum <= maxLabelIdx) {
                renum->Belt_HashMapInt.get(oldNum-mandHypsNum-1)->Belt.Option.getExn+mandHypsNum+1
            } else {
                oldNum
            }
            intToCompressedProofStr(newNum)
        }
    })->Array.joinUnsafe("")

    if (expectedBlock != newBlock) {
        Console.log("-------------------------------------------------------------------------------------------")
        Console.log2("mandHypsNum", mandHypsNum)
        Console.log("actualLabels:")
        actualLabels->Array.forEachWithIndex((label,i) => Console.log(`${(i+1)->Belt_Int.toString}:${label}`))
        Console.log("actualBlock:")
        actualBlock->compressedProofBlockToArray->Array.map(numStr => {
            if (numStr == "Z") {
                "Z"
            } else {
                compressedProofStrToInt(numStr)->Belt.Int.toString
            }
        })->Array.joinUnsafe(" ")->Console.log
        Console.log("-------------------------------------------------------------------------------------------")
        false
    } else {
        true
    }
}

let proofEq = (~expectedProof:proof, ~actualProof:proof, ~getMandHypsNum:()=>int):bool => {
    switch expectedProof {
        | Uncompressed({labels:expectedLabels}) => {
            switch actualProof {
                | Uncompressed({labels:actualLabels}) => expectedLabels == actualLabels
                | Compressed(_) => false
            }
        }
        | Compressed({labels:expectedLabels, compressedProofBlock:expectedBlock}) => {
            switch actualProof {
                | Uncompressed(_) => false
                | Compressed({labels:actualLabels, compressedProofBlock:actualBlock}) => {
                    if (expectedLabels->Array.length != actualLabels->Array.length) {
                        false
                    } else if (expectedLabels == actualLabels) {
                        expectedBlock == actualBlock
                    } else {
                        compareCompressedProofsAfterRenumbering(
                            ~mandHypsNum=getMandHypsNum(),
                            ~expectedLabels,
                            ~expectedBlock,
                            ~actualLabels,
                            ~actualBlock,
                        )
                    }
                }
            }
        }
    }
}

describe("createProof", _ => {

    it("conversion compressedProof->tableProof->compressedProof creates equivalent compressedProof", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)

        let progressTracker = testProgressTrackerMake(
            ~step=0.05, 
            ~maxCnt = countFrames(ast),
        )

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            switch node {
                //when
                | Provable({label,expr:exprStr,proof:Some(expectedProof)}) => {
                    let expr = ctx->ctxSymsToIntsExn(exprStr)

                    let proofNode = verifyProof(~ctx, ~expr, ~proof=expectedProof, ~isDisjInCtx=isDisj(ctx, ...))

                    let proofTableOptimized = createProofTableFromProof(~proofNode, ~mergeSameRows=true)
                    let actualProofOptimized = createProof(
                        ctx->getMandHyps(expr), proofTableOptimized, proofTableOptimized->Array.length-1
                    )
                    verifyProof(~ctx, ~expr, ~proof=actualProofOptimized, ~isDisjInCtx=isDisj(ctx, ...))->ignore

                    let proofTable = createProofTableFromProof(~proofNode, ~mergeSameRows=false)
                    let actualProof = createProof(
                        ctx->getMandHyps(expr), proofTable, proofTable->Array.length-1
                    )
                    verifyProof(~ctx, ~expr, ~proof=actualProof, ~isDisjInCtx=isDisj(ctx, ...))->ignore

                    //then
                    if (
                        !proofEq(
                            ~expectedProof, 
                            ~actualProof, 
                            ~getMandHypsNum = () => getMandHyps(ctx, expr)->Array.length
                        )
                    ) {
                        Console.log2("expected", expectedProof)
                        Console.log2("actual", actualProof)
                        failMsg(`Proof comparison failed for ${label}`)
                    }

                    progressTracker->testProgressTrackerIncCnt
                }
                | _ => ()
            }
        })->ignore
    })
})

describe("tool: sort assertions by usage count", _ => {

    it_skip("tool: sort assertions by usage count", _ => {
        let baseDir = ""
        let mmFileText = Expln_utils_files.readStringFromFile(`${baseDir}/set.mm`)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)
        let numOfFrames = countFrames(ast)
        let progressTracker = testProgressTrackerMake(
            ~step=0.05, 
            ~maxCnt = numOfFrames,
        )

        let dirDeps: Belt_HashMapString.t<int> = Belt_HashMapString.make(~hintSize=numOfFrames)

        let ctx = loadContext(ast, ~onPreProcess = (_,node) => {
            switch node {
                | Provable({proof:Some(expectedProof)}) => {
                    switch expectedProof {
                        | Uncompressed({labels}) | Compressed({labels}) => {
                            labels->Array.forEach(label => {
                                switch dirDeps->Belt_HashMapString.get(label) {
                                    | None => dirDeps->Belt_HashMapString.set(label,1)
                                    | Some(cnt) => dirDeps->Belt_HashMapString.set(label,cnt+1)
                                }
                            })
                        }
                    }

                    progressTracker->testProgressTrackerIncCnt
                }
                | Axiom(_) => progressTracker->testProgressTrackerIncCnt
                | _ => ()
            }
        })

        dirDeps
            ->Belt_HashMapString.toArray
            ->Array.filter(((label,_)) => {
                switch ctx->getFrame(label) {
                    | None => false
                    | Some(frame) => {
                        let asrtTyp:int = frame.asrt->Array.getUnsafe(0)
                        if (ctx->ctxIntToSymExn(asrtTyp) == "|-") {
                            Array.concatMany(frame.asrt, frame.hyps->Array.map(({expr}) => expr))
                                ->Array.filter(frmInt => frmInt >= 0)
                                ->Belt_HashSetInt.fromArray
                                ->Belt_HashSetInt.toArray
                                ->Array.map(frmVarInt => frame.varTypes->Array.getUnsafe(frmVarInt)->frmIntToSymExn(ctx,frame,_))
                                ->Array.some(sym => sym != "wff")
                        } else {
                            false
                        }
                    }
                }
            })
            ->Array.toSorted(((_,cnt1), (_,cnt2)) => -. ((cnt1 - cnt2)->Float.fromInt))
            ->Array.map(((label,cnt)) => `${label} ${cnt->Int.toString}`)
            ->Array.join("\n")
            ->Expln_utils_files.writeStringToFile(`${baseDir}/asrt_usage_counts.txt`)
    })
})