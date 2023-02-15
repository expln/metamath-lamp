open Expln_test
open MM_int_test_utils
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier

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

describe("save proofs to files", _ => {
    it("print proof as plain text", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(mmFileText, ())

        let labelToPrint = "pm5.74"

        traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label,expr,proof})} if label == labelToPrint => {
                    let ctx = loadContext(ast, ~stopBefore=label, ())
                    let proofNode = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof)
                    let proofTable = createProofTableFromProof(proofNode)

                    let proof2 = createProof(ctx, proofTable, proofTable->Js_array2.length-1)
                    assertEqMsg(proof, proof2, label)

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
        let unmatchCnt = ref(0)

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            if (unmatchCnt.contents < 500000) {
                switch node {
                    | Provable({label,expr,proof:proof1}) => {
                        if (label != "isrusgr" && label != "idiVD") {
                            cnt.contents = cnt.contents + 1
                            Js.Console.log2(cnt.contents, label)

                            let proofNode1 = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof1)
                            let proofTable1 = createProofTableFromProof(proofNode1)

                            let proof2 = createProof(ctx, proofTable1, proofTable1->Js_array2.length-1)
                            let proofNode2 = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof2)
                            let proofTable2 = createProofTableFromProof(proofNode2)

                            if (proofTable1 != proofTable2) {
                                unmatchCnt.contents = unmatchCnt.contents + 1
                                
                                saveProofTableToFile(
                                    ~dirPath=dirPathToSaveProofsTo, ~ctx, ~label, 
                                    ~proof1, ~proofTable1,
                                    ~proof2, ~proofTable2,
                                )
                            }
                        }
                    }
                    | _ => ()
                }
            }
        }, ())->ignore
        Js.Console.log2("cnt", cnt.contents)
        Js.Console.log2("unmatchCnt", unmatchCnt.contents)
    })
})
