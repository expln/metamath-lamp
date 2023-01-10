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
        ~bottomUp=true,
        ~debug=true,
        ()
    )

    //then
    Js.Console.log2("proofTree", proofTree)
}

describe("proofTreeProve", _ => {
    it_skip("unifies few statements", _ => {
        testUnification(
            ~pathToMmFile = ".../metamath/set.mm",
            ~stopAfter="df-sgn", 
            ~varsText="",
            ~disjText="",
            ~stmts=[
                {
                    label: "stmt1",
                    text: "|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )",
                    jstf: None
                },
            ],
            ()
        )
    })
})