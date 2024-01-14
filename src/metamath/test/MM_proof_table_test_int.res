open Expln_test
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier
open MM_int_test_utils

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

    let renum = actualLabels->Js_array2.mapi((actualLabel,i) => 
        (i, (expectedLabels->Js_array2.indexOf(actualLabel)))
    )->Belt_HashMapInt.fromArray

    let minLabelIdx = mandHypsNum + 1
    let maxLabelIdx = mandHypsNum + actualLabels->Js_array2.length
    let newBlock = actualBlock->compressedProofBlockToArray->Js.Array2.map(oldNumStr => {
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
    })->Js.Array2.joinWith("")

    if (expectedBlock != newBlock) {
        Js.Console.log("-------------------------------------------------------------------------------------------")
        Js.Console.log2("mandHypsNum", mandHypsNum)
        Js.Console.log("actualLabels:")
        actualLabels->Js.Array2.forEachi((label,i) => Js.Console.log(`${(i+1)->Belt_Int.toString}:${label}`))
        Js.Console.log("actualBlock:")
        actualBlock->compressedProofBlockToArray->Js.Array2.map(numStr => {
            if (numStr == "Z") {
                "Z"
            } else {
                compressedProofStrToInt(numStr)->Belt.Int.toString
            }
        })->Js.Array2.joinWith(" ")->Js.Console.log
        Js.Console.log("-------------------------------------------------------------------------------------------")
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
                    if (expectedLabels->Js.Array2.length != actualLabels->Js.Array2.length) {
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
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let progressTracker = testProgressTrackerMake(
            ~step=0.05, 
            ~maxCnt = countFrames(ast, ()),
        )

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            switch node {
                //when
                | Provable({label,expr:exprStr,proof:Some(expectedProof)}) => {
                    let expr = ctx->ctxSymsToIntsExn(exprStr)

                    let proofNode = verifyProof(~ctx, ~expr, ~proof=expectedProof, ~isDisjInCtx=ctx->isDisj)

                    let proofTableOptimized = createProofTableFromProof(~proofNode, ~mergeSameRows=true, ())
                    let actualProofOptimized = createProof(
                        ctx->getMandHyps(expr, ()), proofTableOptimized, proofTableOptimized->Js_array2.length-1
                    )
                    verifyProof(~ctx, ~expr, ~proof=actualProofOptimized, ~isDisjInCtx=ctx->isDisj)->ignore

                    let proofTable = createProofTableFromProof(~proofNode, ~mergeSameRows=false, ())
                    let actualProof = createProof(
                        ctx->getMandHyps(expr, ()), proofTable, proofTable->Js_array2.length-1
                    )
                    verifyProof(~ctx, ~expr, ~proof=actualProof, ~isDisjInCtx=ctx->isDisj)->ignore

                    //then
                    if (
                        !proofEq(
                            ~expectedProof, 
                            ~actualProof, 
                            ~getMandHypsNum = () => getMandHyps(ctx, expr, ())->Js.Array2.length
                        )
                    ) {
                        Js.Console.log2("expected", expectedProof)
                        Js.Console.log2("actual", actualProof)
                        failMsg(`Proof comparison failed for ${label}`)
                    }

                    progressTracker->testProgressTrackerIncCnt
                }
                | _ => ()
            }
        }, ())->ignore
    })
})