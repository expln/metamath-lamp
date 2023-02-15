open Expln_test
open MM_int_test_utils
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier

let mmFilePath = "/metamath/set.mm"

describe("save proofs to files", _ => {
    it("save proofs as plain text", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(mmFileText, ())

        let labelToPrint = "pm5.74"

        traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label,expr,proof})} if label == labelToPrint => {
                    let ctx = loadContext(ast, ~stopAfter=label, ())
                    let proofNode = verifyProof(ctx, ctx->ctxSymsToIntsExn(expr), proof)
                    let proofTable = createProofTableFromProof(proofNode)
                    proofTablePrint(ctx, proofTable, labelToPrint)
                    None
                }
                | _ => None
            }
        }, ())->ignore
    })
})
