open Expln_test
open MM_parser
open MM_context
open MM_proof_table
open MM_proof_verifier
open MM_proof_tree2
open MM_provers
open MM_parenCounter
open MM_substitution

let testCreateProof = (~mmFile, ~exprStr, ~expectedProofStr) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let expr = ctx->ctxStrToIntsExn(exprStr)
    let frms = prepareFrmSubsData(ctx)
    let parenCnt = parenCntMake(ctx->ctxStrToIntsExn("( ) { } [ ]"))
    let hyps = ctx->getAllHyps
    let disj = ctx->getAllDisj
    let maxVar = ctx->getNumOfVars - 1

    let proofTree = ptMake( ~frms, ~disj, ~hyps, ~parenCnt, ~exprToStr=None, ~maxVar )
    let nodeToProve = proofTree->ptMakeNode(~label=None, ~expr)

    //when
    proveFloating(proofTree, nodeToProve)

    //then
    let proofTable = pnCreateProofTable(nodeToProve)
    let actualProof = createProof(ctx, proofTable, proofTable->Js_array2.length-1)

    //then
    try {
        verifyProof(ctx, expr, actualProof)
        //let proof = verifyProof(ctx, expr, actualProof)
        //let tbl = createOrderedProofTableFromProof(proof)
        //proofTablePrint(ctx,tbl,exprStr)
    } catch {
        | MmException({msg}) => failMsg("Proof verification failed for '" ++ exprStr ++ "'\nwith msg: '" ++ msg ++ "'")
        | _ => failMsg("Proof verification failed for '" ++ exprStr ++ "'")
    }->ignore
    let actualProofStr = switch actualProof {
        | Compressed({labels, compressedProofBlock}) => {
            "( " ++ (labels->Js_array2.joinWith(" ")) ++ " ) " ++ compressedProofBlock
        }
        | p => failMsg(`Unexpected form of proof: ${Expln_utils_common.stringify(p)}`)
    }
    assertEqMsg(actualProofStr, expectedProofStr, `testCreateProof for: ${exprStr}`)
}

describe("proveFloating", _ => {
    it("finds proofs for simple wffs", _ => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testCreateProof(~mmFile=demo0, ~exprStr="wff t = t", ~expectedProofStr="( weq ) AAB")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( t = t -> t = t ) )", ~expectedProofStr="( weq wim ) AABZDDCC")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( r = r -> ( t = t -> ( r = r -> t = t ) ) ) )", ~expectedProofStr="( weq wim ) AACZBBCZEFEDDDD")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) -> ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) )", ~expectedProofStr="( weq wim ) BBCAACDZEDZFDZGD")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff ph", ~expectedProofStr="(  ) A")
        testCreateProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", ~expectedProofStr="( wb wi wn ) ABCABDBADEDEC")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff -. ( ( ( ph <-> ps ) -> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) ) -> -. ( -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) -> ( ph <-> ps ) ) )", ~expectedProofStr="( wb wi wn ) ABCZABDBADEDEZDGFDEDE")
    })
})