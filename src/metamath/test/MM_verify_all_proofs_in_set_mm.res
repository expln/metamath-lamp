open Expln_test
open MM_parser
open MM_proof_table
open MM_context
open MM_proof_verifier

let mmFilePath = "metamath/set.mm"

describe_skip("verify all proofs in set.mm", _ => {
    it("verify all proofs in set.mm", _ => {
        let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        let cnt = ref(0)

        loadContext(ast, ~debug=false, ~onPreProcess = (ctx,node) => {
            switch node {
                | Provable({label,expr:exprStr,proof:Some(proof)}) => {
                    if (!(label == "isrusgr" || label == "idiVD")) {
                        cnt.contents = cnt.contents + 1
                        Js.Console.log2(cnt.contents, label)
                        let expr = ctx->ctxSymsToIntsExn(exprStr)
                        verifyProof(ctx, expr, proof)->ignore
                    }
                }
                | _ => ()
            }
        }, ())->ignore
    })
})