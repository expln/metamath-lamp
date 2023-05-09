open Expln_test
open MM_parser
open MM_context
open MM_proof_verifier
open MM_int_test_utils

let mmFilePath = "./src/metamath/test/resources/set._mm"

describe("verifyProof", _ => {
    it("verifies all proofs in set.mm", _ => {
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
                | Provable({expr:exprStr,proof:Some(proof)}) => {
                    let expr = ctx->ctxSymsToIntsExn(exprStr)

                    //then
                    verifyProof(~ctx, ~expr, ~proof, ~isDisjInCtx=ctx->isDisj)->ignore

                    progressTracker->testProgressTrackerIncCnt
                }
                | _ => ()
            }
        }, ())->ignore
    })
})