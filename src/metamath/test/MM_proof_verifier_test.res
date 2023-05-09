open Expln_test
open MM_parser
open MM_context
open MM_proof_verifier

describe("verifyProof", _ => {
    it("successfully verifies a valid uncompressed proof", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ~stopBefore="th1", ())
        let foundTheorem = traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label:"th1",expr,proof:Some(proof)})} => Some((expr,proof))
                | _ => None
            }
        }, ())
        let (exprStr, proof) = switch foundTheorem {
            | (_,Some((exprStr,proof))) => (exprStr,proof)
            | _ => failMsg(`!!!`)
        }

        //when
        let proof = verifyProof(~ctx, ~expr=ctxSymsToIntsExn(ctx, exprStr), ~proof, ~isDisjInCtx=ctx->isDisj)

        //then
        assertEq(
            proof->proofNodeGetExpr->ctxIntsToStrExn(ctx, _),
            "|- t = t"
        )
    })

    it("shows an error for an invalid uncompressed proof", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ~stopBefore="th2", ())
        let foundTheorem = traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label:"th2",expr,proof:Some(proof)})} => Some((expr,proof))
                | _ => None
            }
        }, ())
        let (exprStr, proof) = switch foundTheorem {
            | (_,Some((exprStr,proof))) => (exprStr,proof)
            | _ => failMsg(`!!!`)
        }

        //when
        let errorMsg = ref("")
        try {
            let _ = verifyProof(~ctx, ~expr=ctxSymsToIntsExn(ctx, exprStr), ~proof, ~isDisjInCtx=ctx->isDisj)
        } catch {
            | MmException({msg}) => errorMsg.contents = msg
        }
        assertEq(errorMsg.contents, "!compareExprAfterSubstitution(ess, subs, stack->stGetExpr(baseIdx+i))")
    })

    it("successfully verifies a valid compressed proof", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/set-reduced._mm")
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
        let ctx = loadContext(ast, ~stopBefore="dfbi1ALT", ())
        let foundTheorem = traverseAst((), ast, ~process=(_,node) => {
            switch node {
                | {stmt:Provable({label:"dfbi1ALT",expr,proof:Some(proof)})} => Some((expr,proof))
                | _ => None
            }
        }, ())
        let (exprStr, proof) = switch foundTheorem {
            | (_,Some((exprStr,proof))) => (exprStr,proof)
            | _ => failMsg(`!!!`)
        }

        //when
        let proof = verifyProof(~ctx, ~expr=ctxSymsToIntsExn(ctx, exprStr), ~proof, ~isDisjInCtx=ctx->isDisj)

        //then
        assertEq(
            proof->proofNodeGetExpr->ctxIntsToStrExn(ctx, _),
            "|- ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )"
        )
    })
})

describe("compressedProofBlockToArray", _ => {
    it("converts a compressed proof block to array of strings", _ => {
        assertEq(
            compressedProofBlockToArray("YMYRYHUUBCEYMUNZYRZQ"),
            ["YM","YR","YH","UUB","C","E","YM","UN","Z","YR","Z","Q"]
        )
    })
})

describe("compressedProofStrToInt", _ => {
    it("converts compressed proof numbers to intigers", _ => {
        assertEq(compressedProofStrToInt("A"), 1)
        assertEq(compressedProofStrToInt("B"), 2)
        assertEq(compressedProofStrToInt("T"), 20)
        assertEq(compressedProofStrToInt("UA"), 21)
        assertEq(compressedProofStrToInt("UB"), 22)
        assertEq(compressedProofStrToInt("UT"), 40)
        assertEq(compressedProofStrToInt("VA"), 41)
        assertEq(compressedProofStrToInt("VB"), 42)
        assertEq(compressedProofStrToInt("YT"), 120)
        assertEq(compressedProofStrToInt("UUA"), 121)
        assertEq(compressedProofStrToInt("YYT"), 620)
        assertEq(compressedProofStrToInt("UUUA"), 621)
    })
})