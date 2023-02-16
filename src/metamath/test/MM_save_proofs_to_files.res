open Expln_test
open MM_int_test_utils
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier
open Expln_utils_common

let mmFilePath = "/metamath/set.mm"
let dirPathToSaveProofsTo = "/generated-proofs"

let saveProofTableToFile = (
    ~dirPath, ~ctx, ~label, 
    ~proof1, ~proofTable1,
    ~proof2, ~proofTable2,
) => {
    Expln_utils_files.writeStringToFile(
        dirPath ++ "/" ++ label ++ ".txt-1",
        Expln_utils_common.stringify(proof1) ++ "\n\n\n" ++ proofTableToString(ctx, proofTable1)
    )
    Expln_utils_files.writeStringToFile(
        dirPath ++ "/" ++ label ++ ".txt-2",
        Expln_utils_common.stringify(proof2) ++ "\n\n\n" ++ proofTableToString(ctx, proofTable2)
    )
}

let comparatorInverse = (cmp:comparator<'a>):comparator<'a> => (x,y) => -cmp(x,y)

type shorterProof = {
    mathbox: option<string>,
    label: string,
    curProof: string,
    newProof: string,
    proofLengthReduction: int,
    proofLengthReduction12: int,
    shorterBySteps: int,
    shorterBySteps12: int,
}

let cmpShorterProofByMathbox = (sp1,sp2) => {
    switch sp1.mathbox {
        | None => {
            switch sp2.mathbox {
                | None => 0
                | Some(_) => -1
            }
        }
        | Some(mb1) => {
            switch sp2.mathbox {
                | None => 1
                | Some(mb2) => mb1->Js.String2.localeCompare(mb2)->Belt_Float.toInt
            }
        }
    }
}

let getProofLength = (proof):int => {
    switch proof {
        | Compressed({compressedProofBlock}) => compressedProofBlock->Js_string2.length
        | _ => raise({MmException({msg:`Unexpected format of proof.`})})
    }
}

let cmpShorterProofBySteps = Expln_utils_common.comparatorBy(sp => sp.shorterBySteps)
let cmpShorterProofByProofLengthReduction = Expln_utils_common.comparatorBy(sp => sp.proofLengthReduction)
let cmpShorterProofByLabel:comparator<shorterProof> = 
    (sp1,sp2) => sp1.label->Js_string2.localeCompare(sp2.label)->Belt.Float.toInt

let cmpShorterProof = cmpShorterProofByMathbox
                        ->Expln_utils_common.comparatorAndThen(cmpShorterProofByProofLengthReduction->comparatorInverse)
                        ->Expln_utils_common.comparatorAndThen(cmpShorterProofByLabel)

let proofToStr = proof => {
    switch proof {
        | Compressed({labels, compressedProofBlock}) => {
            "( " ++ labels->Js.Array2.joinWith(" ") ++ " )\n" ++ compressedProofBlock
        }
        | _ => raise({MmException({msg:`Unexpected format of proof.`})})
    }
}

describe("save proofs to files", _ => {
    it("print proof as plain text", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(mmFileText, ())

        let labelToPrint = "isrusgr"

        traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label,expr,proof})} if label == labelToPrint => {
                    let ctx = loadContext(ast, ~stopBefore=label, ())
                    let proofNode = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof)
                    let proofTable = createProofTableFromProof(proofNode)

                    // let proof2 = createProof(ctx, proofTable, proofTable->Js_array2.length-1)
                    // assertEqMsg(proof, proof2, label)

                    proofTablePrint(ctx, proofTable, labelToPrint)
                    Some(())
                }
                | _ => None
            }
        }, ())->ignore
    })

    it("compare proofs before and after regeneration", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(mmFileText, ())

        let cnt = ref(0)

        let shorterProofs = []
        let curMathbox = ref(None)

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            switch node {
                | Comment({text}) => {
                    if (text->Js.String2.includes("Mathbox for")) {
                        curMathbox.contents = Some(text)
                    }
                }
                | Provable({label,expr,proof:proof1}) => {
                    if (label != "isrusgr" && label != "idiVD") {
                        cnt.contents = cnt.contents + 1
                        Js.Console.log2(cnt.contents, label)

                        let proofNode1 = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof1)
                        let proofTable1 = createProofTableFromProof(proofNode1)

                        let proof2 = createProof(ctx, proofTable1, proofTable1->Js_array2.length-1)
                        let proofNode2 = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof2)
                        let proofTable2 = createProofTableFromProof(proofNode2)

                        let proof3 = createProof(ctx, proofTable2, proofTable2->Js_array2.length-1)
                        let proofNode3 = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof3)
                        let proofTable3 = createProofTableFromProof(proofNode3)

                        if (getProofLength(proof3) < getProofLength(proof1)) {
                            shorterProofs->Js.Array2.push({
                                mathbox: curMathbox.contents,
                                label,
                                curProof: proofToStr(proof1),
                                newProof: proofToStr(proof3),
                                proofLengthReduction: getProofLength(proof1) - getProofLength(proof3),
                                proofLengthReduction12: getProofLength(proof1) - getProofLength(proof2),
                                shorterBySteps: proofTable1->Js_array2.length - proofTable3->Js_array2.length,
                                shorterBySteps12: proofTable1->Js_array2.length - proofTable2->Js_array2.length,
                            })->ignore
                        }
                    }
                }
                | _ => ()
            }
        }, ())->ignore
        // Js.Console.log2("cnt", cnt.contents)
        // Js.Console.log2("unmatchCnt", unmatchCnt.contents)
        shorterProofs->Js.Array2.sortInPlaceWith(cmpShorterProof)->ignore

        let (_,strArr) = shorterProofs->Js.Array2.reduce(
            ((mb,strArr),sp) => {
                let mb = if (mb != sp.mathbox) {
                    strArr->Js_array2.push("")->ignore
                    strArr->Js_array2.push(Expln_utils_common.stringify(sp.mathbox))->ignore
                    strArr->Js_array2.push("")->ignore
                    sp.mathbox
                } else {
                    mb
                }
                strArr->Js_array2.push(sp.label)->ignore
                strArr->Js_array2.push(`current proof:`)->ignore
                strArr->Js_array2.push(sp.curProof)->ignore
                strArr->Js_array2.push(`new proof:`)->ignore
                strArr->Js_array2.push(sp.newProof)->ignore
                strArr->Js_array2.push(`proof length reduction: ${sp.proofLengthReduction->Belt.Int.toString}; proof length reduction (1 - 2): ${sp.proofLengthReduction12->Belt.Int.toString}`)->ignore
                strArr->Js_array2.push(`shorter by steps: ${sp.shorterBySteps->Belt.Int.toString}; shorter by steps (1 - 2): ${sp.shorterBySteps12->Belt.Int.toString}`)->ignore
                strArr->Js_array2.push("")->ignore
                (mb,strArr)
            },
            (None,[])
        )
        Expln_utils_files.writeStringToFile(
            dirPathToSaveProofsTo ++ "/" ++ "shorter-proofs-with-counts.txt",
            strArr->Js_array2.joinWith("\n")
        )

        let (_,strArr) = shorterProofs->Js.Array2.reduce(
            ((mb,strArr),sp) => {
                let mb = if (mb != sp.mathbox) {
                    strArr->Js_array2.push("")->ignore
                    strArr->Js_array2.push(Expln_utils_common.stringify(sp.mathbox))->ignore
                    strArr->Js_array2.push("")->ignore
                    sp.mathbox
                } else {
                    mb
                }
                strArr->Js_array2.push(sp.label)->ignore
                strArr->Js_array2.push(`current proof:`)->ignore
                strArr->Js_array2.push(sp.curProof)->ignore
                strArr->Js_array2.push(`new proof:`)->ignore
                strArr->Js_array2.push(sp.newProof)->ignore
                strArr->Js_array2.push("")->ignore
                (mb,strArr)
            },
            (None,[])
        )
        Expln_utils_files.writeStringToFile(
            dirPathToSaveProofsTo ++ "/" ++ "shorter-proofs.txt",
            strArr->Js_array2.joinWith("\n")
        )

    })
})
