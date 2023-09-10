open Expln_test
open MM_parser
open MM_context
open MM_proof_table
open MM_proof_verifier
open MM_proof_tree_dto
open MM_provers
open MM_parenCounter
open MM_substitution

let testCreateProof = (~mmFile, ~exprStr, ~expectedProofStr) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let expr = ctx->ctxStrToIntsExn(exprStr)
    let frms = prepareFrmSubsData(~ctx, ())

    //when
    let proofTree = proveFloatings(
        ~wrkCtx=ctx,
        ~frms,
        ~frameRestrict = { useDisc:true, useDepr:true, useTranDepr:true },
        ~floatingsToProve = [expr],
        ~parenCnt=parenCntMake(ctx->ctxStrToIntsExn(parens), ()),
    )

    //then
    let proofTreeDto = proofTreeToDto(proofTree, [expr])
    let node = proofTreeDto.nodes->Js.Array2.find(node => node.expr->exprEq(expr))->Belt.Option.getExn
    let proofTable = createProofTable(~tree=proofTreeDto, ~root=node, ())
    let actualProof = createProof(ctx->getMandHyps(node.expr), proofTable, proofTable->Js_array2.length-1)

    //then
    try {
        verifyProof(~ctx, ~expr, ~proof=actualProof, ~isDisjInCtx=ctx->isDisj)
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
        let demo0 = "./src/metamath/test/resources/demo0._mm"
        let setReduced = "./src/metamath/test/resources/set-reduced._mm"

        testCreateProof(~mmFile=demo0, ~exprStr="wff t = t", ~expectedProofStr="( weq ) AAB")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( t = t -> t = t ) )", ~expectedProofStr="( weq wim ) AABZDDCC")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( r = r -> ( t = t -> ( r = r -> t = t ) ) ) )", ~expectedProofStr="( weq wim ) AACZBBCZEFEDDDD")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) -> ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) )", ~expectedProofStr="( weq wim ) BBCAACDZEDZFDZGD")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff ph", ~expectedProofStr="(  ) A")
        testCreateProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", ~expectedProofStr="( wb wi wn ) ABCABDBADEDEC")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff -. ( ( ( ph <-> ps ) -> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) ) -> -. ( -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) -> ( ph <-> ps ) ) )", ~expectedProofStr="( wb wi wn ) ABCZABDBADEDEZDGFDEDE")
    })
})