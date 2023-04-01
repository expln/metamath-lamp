open Expln_test
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier

let mmFilePath = "./src/metamath/test/resources/set.mm"

describe("verify all proofs in set.mm", _ => {
    it("verifyProof verifies all proofs in set.mm", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let cnt = ref(0)

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            switch node {
                | Provable({label,expr:exprStr,proof:Some(proof)}) => {
                    cnt.contents = cnt.contents + 1
                    // Js.Console.log2(cnt.contents, label)
                    let expr = ctx->ctxSymsToIntsExn(exprStr)
                    verifyProof(ctx, expr, proof)->ignore
                }
                | _ => ()
            }
        }, ())->ignore
    })

    it("converting proofs between compressed and table forms does not introduce errors", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let cnt = ref(0)

        loadContext(ast, ~onPreProcess = (ctx,node) => {
            switch node {
                | Provable({label,expr:exprStr,proof:Some(proof1)}) => {
                    if (label == "isrusgr" || label == "idiVD") {
                        cnt.contents = cnt.contents + 1
                        Js.Console.log2(cnt.contents, label)
                        let expr = ctx->ctxSymsToIntsExn(exprStr)

                        let proofNode1 = verifyProof(ctx, expr, proof1)
                        let proofTable1 = createProofTableFromProof(proofNode1)
                        proofTablePrint(ctx, proofTable1, label ++ " - proofTable1")

                        let proof2 = createProof(ctx, proofTable1, proofTable1->Js_array2.length-1)
                        let proofNode2 = verifyProof(ctx, expr, proof2)
                        let proofTable2 = createProofTableFromProof(proofNode2)
                    }
                }
                | _ => ()
            }
        }, ())->ignore
    })
})