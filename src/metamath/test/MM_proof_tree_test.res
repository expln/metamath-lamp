open Expln_test
open MM_parser
open MM_context
open MM_proof_table
open MM_proof_verifier
open MM_proof_tree
open MM_parenCounter
open MM_substitution

let testCreateProof = (~mmFile, ~exprStr, ~expectedProof) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let expr = exprStr->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(ctx, _)
    let frms = prepareFrmSubsData(ctx)
    let parenCnt = parenCntMake(ctx->ctxSymsToIntsExn(["(", ")", "{", "}", "[", "]"]))
    let hyps = ctx->getAllHyps
    let disj = ctx->getAllDisj

    //when
    let proofTree = proofTreeProve(
        ~parenCnt,
        ~frms,
        ~ctx,
        ~stmts = [
            {
                label: None,
                expr,
                justification: None
            }
        ],
        ~syntaxProof=true,
        ()
    )
    let proofNode = proofTree.nodes->Belt_MutableMap.get(expr)->Belt_Option.getExn
    let proofTable = proofTreeCreateProofTable(proofNode)
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
    assertEqMsg(actualProofStr, expectedProof, `testCreateProof for: ${exprStr}`)
}

describe("proofTreeProve", _ => {
    it("finds proofs for simple wffs", _ => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testCreateProof(~mmFile=demo0, ~exprStr="wff t = t", ~expectedProof="( weq ) AAB")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( t = t -> t = t ) )", ~expectedProof="( weq wim ) AABZDDCC")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( r = r -> ( t = t -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) AACZBBCZEFEDDDD")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) -> ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) BBCAACDZEDZFDZGD")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff ph", ~expectedProof="(  ) A")
        testCreateProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", ~expectedProof="( wb wi wn ) ABCABDBADEDEC")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff -. ( ( ( ph <-> ps ) -> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) ) -> -. ( -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) -> ( ph <-> ps ) ) )", ~expectedProof="( wb wi wn ) ABCZABDBADEDEZDGFDEDE")
    })
})