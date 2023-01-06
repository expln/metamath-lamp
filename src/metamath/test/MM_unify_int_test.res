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
            // ~stopAfter="reccot", 
            ~varsText="var1 class class1
var2 setvar setvar2
var3 class class3
var4 class class4
var5 class class5
var6 class class6
var7 class class7
var8 class class8
var9 wff wff9",
            ~disjText="setvar2,wff9,class3",
            ~stmts=[
                {
                    label: "stmt1-itgsinexplem1.11",
                    text: "|- class1 = ( setvar2 e. CC |-> ( ( sin ` setvar2 ) ^ class3 ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.21",
                    text: "|- class4 = ( setvar2 e. CC |-> -u ( cos ` setvar2 ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.31",
                    text: "|- class5 = ( setvar2 e. CC |-> ( ( class3 x. ( ( sin ` setvar2 ) ^ ( class3 - 1 ) ) ) x. ( cos ` setvar2 ) ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.41",
                    text: "|- class6 = ( setvar2 e. CC |-> ( ( ( sin ` setvar2 ) ^ class3 ) x. ( sin ` setvar2 ) ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.51",
                    text: "|- class7 = ( setvar2 e. CC |-> ( ( ( class3 x. ( ( sin ` setvar2 ) ^ ( class3 - 1 ) ) ) x. ( cos ` setvar2 ) ) x. -u ( cos ` setvar2 ) ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.61",
                    text: "|- class8 = ( setvar2 e. CC |-> ( ( ( cos ` setvar2 ) ^ 2 ) x. ( ( sin ` setvar2 ) ^ ( class3 - 1 ) ) ) )",
                    jstf: None
                },
                {
                    label: "stmt1-itgsinexplem1.71",
                    text: "|- ( wff9 -> class3 e. NN )",
                    jstf: None
                },
                {
                    label: "stmt1",
                    text: "|- ( wff9 -> S. ( 0 (,) _pi ) ( ( ( sin ` setvar2 ) ^ class3 ) x. ( sin ` setvar2 ) ) _d setvar2 = ( class3 x. S. ( 0 (,) _pi ) ( ( ( cos ` setvar2 ) ^ 2 ) x. ( ( sin ` setvar2 ) ^ ( class3 - 1 ) ) ) _d setvar2 ) )",
                    jstf: None
                },
            ],
            ()
        )
    })
})