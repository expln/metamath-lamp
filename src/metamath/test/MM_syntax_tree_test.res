open Expln_test
open MM_parser
open MM_context
open MM_parenCounter
open MM_syntax_tree
open MM_substitution
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers

type rec syntaxTreeNodeTest = {
    label:string,
    children:array<childNodeTest>,
}
and childNodeTest =
    | Subtree(syntaxTreeNodeTest)
    | Symbol(string)

let rec syntaxTreeToSyntaxTreeTest = (node:syntaxTreeNode) => {
    {
        label: node.label,
        children: node.children->Js_array2.map(c => {
            switch c {
                | Subtree(childNode) => Subtree(syntaxTreeToSyntaxTreeTest(childNode))
                | Symbol({sym}) => Symbol(sym)
            }
        })
    }
}


let testSyntaxTree = (~mmFile, ~exprStr, ~expectedSyntaxTree:syntaxTreeNodeTest) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let expr = exprStr->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(ctx, _)
    let proofTree = proveFloatings(
        ~ctx,
        ~frms=prepareFrmSubsData(ctx),
        ~stmts = [expr],
        ~parenCnt=parenCntMake(ctx->ctxStrToIntsExn(parens), ()),
        (),
    )
    let proofTreeDto = proofTreeToDto(proofTree, [expr])
    let node = proofTreeDto.nodes->Js.Array2.find(node => node.expr->exprEq(expr))->Belt.Option.getExn
    let proofTable = createProofTable(proofTreeDto, node)

    //when
    let actualSyntaxTree = buildSyntaxTree(ctx, proofTable, proofTable->Js_array2.length-1)

    //then
    //Js.Console.log2("actualSyntaxTree", actualSyntaxTree->syntaxTreeToSyntaxTreeTest->Expln_utils_common.stringify)
    assertEqMsg(actualSyntaxTree->syntaxTreeToSyntaxTreeTest, expectedSyntaxTree, `testSyntaxTree for: ${exprStr}`)
}

describe("buildSyntaxTree", _ => {
    it("builds correct syntax trees for WWFs", _ => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testSyntaxTree(~mmFile=demo0, ~exprStr="wff t = t", 
            ~expectedSyntaxTree = {
                label: "weq",
                children: [
                    Subtree({
                        label: "tt",
                        children: [
                            Symbol("t")
                        ]
                    }),
                    Symbol("="),
                    Subtree({
                        label: "tt",
                        children: [ 
                            Symbol("t") 
                        ]
                    })
                ]
            }
        )

        testSyntaxTree(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", 
            ~expectedSyntaxTree = {
                label: "wb",
                children: [
                    Symbol("("),
                    Subtree({
                        label: "wb",
                        children: [
                            Symbol("("),
                            Subtree({
                                label: "wph",
                                children: [
                                    Symbol("ph")
                                ]
                            }),
                            Symbol("<->"),
                            Subtree({
                                label: "wps",
                                children: [
                                    Symbol("ps")
                                ]
                            }),
                            Symbol(")")
                        ]
                    }),
                    Symbol("<->"),
                    Subtree({
                        label: "wn",
                        children: [
                            Symbol("-."),
                            Subtree({
                                label: "wi",
                                children: [
                                    Symbol("("),
                                    Subtree({
                                        label: "wi",
                                        children: [
                                            Symbol("("),
                                            Subtree({
                                                label: "wph",
                                                children: [
                                                    Symbol("ph")
                                                ]
                                            }),
                                            Symbol("->"),
                                            Subtree({
                                                label: "wps",
                                                children: [
                                                    Symbol("ps")
                                                ]
                                            }),
                                            Symbol(")")
                                        ]
                                    }),
                                    Symbol("->"),
                                    Subtree({
                                        label: "wn",
                                        children: [
                                            Symbol("-."),
                                            Subtree({
                                                label: "wi",
                                                children: [
                                                    Symbol("("),
                                                    Subtree({
                                                        label: "wps",
                                                        children: [
                                                            Symbol("ps")
                                                        ]
                                                    }),
                                                    Symbol("->"),
                                                    Subtree({
                                                        label: "wph",
                                                        children: [
                                                            Symbol("ph")
                                                        ]
                                                    }),
                                                    Symbol(")")
                                                ]
                                            })
                                        ]
                                    }),
                                    Symbol(")")
                                ]
                            })
                        ]
                    }),
                    Symbol(")")
                ]
            }
        )
    })
})