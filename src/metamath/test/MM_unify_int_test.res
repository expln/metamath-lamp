open Expln_test
open MM_parser
open MM_context
open MM_proof_table
open MM_proof_verifier
open MM_proof_tree
open MM_parenCounter
open MM_substitution
open MM_wrk_ctx

type userStmt = {
    label: string,
    text: string,
    jstf: option<justification>
}

let testUnification = (
    ~pathToMmFile:string, 
    ~stopBefore: option<string>=?, 
    ~stopAfter: option<string>=?, 
    ~varsText:string="",
    ~disjText:string="",
    ~hyps: array<wrkCtxHyp>=[],
    ~stmts: array<userStmt>=[],
    ()
) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(pathToMmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore?, ~stopAfter?, ())
    let frms = prepareFrmSubsData(ctx)
    let parenCnt = parenCntMake(ctx->ctxSymsToIntsExn(["(", ")", "{", "}", "[", "]"]))

    let wrkCtx = createWrkCtx(
        ~preCtx=ctx,
        ~varsText,
        ~disjText,
        ~hyps
    )->Belt.Result.getExn

    //when
    let proofTree = proofTreeProve(
        ~parenCnt,
        ~frms,
        ~ctx=wrkCtx,
        ~stmts=stmts->Js.Array2.map(({label, text, jstf}) => {
            {
                label:Some(label), 
                expr: wrkCtx->ctxStrToIntsExn(text),
                justification: jstf
            }
        }),
        ~syntaxProof=false,
        ()
    )

    //then
    Js.Console.log2("proofTree", proofTree)
}

describe("proofTreeProve", _ => {
    it_skip("unifies few statements", _ => {
        testUnification(
            ~pathToMmFile = ".../books/metamath/set.mm",
            ~stopAfter="reccot", 
            ~varsText="var1 class class1 \n var2 class class2",
            ~stmts=[
                {
                    label: "stmt3",
                    text: "|- ( ( ( class1 e. CC /\\ class1 =/= 0 ) /\\ ( class2 e. CC /\\ class2 =/= 0 ) ) -> ( 1 / ( class1 / class2 ) ) = ( class2 / class1 ) )",
                    jstf: Some({args:[], asrt:"recdiv"})
                },
                {
                    label: "stmt2",
                    text: "|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 ) -> ( cot ` A ) = ( ( cos ` A ) / ( sin ` A ) ) )",
                    jstf: Some({args:[], asrt:"cotval"})
                },
                {
                    label: "stmt1",
                    text: "|- ( ( A e. CC /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) ) )",
                    jstf: Some({args:[], asrt:"tanval"})
                },
                {
                    label: "stmt4",
                    text: "|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) ) )",
                    jstf: None
                },
            ],
            ()
        )
    })
})