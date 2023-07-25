open Expln_test
open MM_parser
open MM_context
open MM_parenCounter
open MM_syntax_tree
open MM_substitution
open MM_proof_tree_dto
open MM_provers
open Common

type rec syntaxTreeNodeTest = {
    label:string,
    children:array<childNodeTest>,
    height: int
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
        }),
        height: node.height
    }
}

let buildSyntaxTreeForTest = (~mmFile, ~exprStr):syntaxTreeNode => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let expr = exprStr->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(ctx, _)
    let proofTree = proveFloatings(
        ~wrkCtx=ctx,
        ~frms=prepareFrmSubsData(~ctx, ()),
        ~floatingsToProve = [expr],
        ~parenCnt=parenCntMake(ctx->ctxStrToIntsExn(parens), ()),
    )
    let proofTreeDto = proofTreeToDto(proofTree, [expr])
    let node = proofTreeDto.nodes->Js.Array2.find(node => node.expr->exprEq(expr))->Belt.Option.getExn
    let proofTable = createProofTable(~tree=proofTreeDto, ~root=node, ())

    switch buildSyntaxTree(ctx, proofTable, proofTable->Js_array2.length-1) {
        | Error(msg) => failMsg(msg)
        | Ok(syntaxTree) => syntaxTree
    }
}

let testSyntaxTree = (~mmFile, ~exprStr, ~expectedSyntaxTree:syntaxTreeNodeTest) => {
    assertEqMsg(
        buildSyntaxTreeForTest(~mmFile, ~exprStr)->syntaxTreeToSyntaxTreeTest, 
        expectedSyntaxTree, 
        `testSyntaxTree for: ${exprStr}`
    )
}

let demo0 = "./src/metamath/test/resources/demo0._mm"
let setReduced = "./src/metamath/test/resources/set-reduced._mm"

describe("buildSyntaxTree", _ => {
    it("builds correct syntax trees for WWFs", _ => {
        testSyntaxTree(~mmFile=demo0, ~exprStr="wff t = t", 
            ~expectedSyntaxTree = {
                label: "weq",
                children: [
                    Subtree({
                        label: "tt",
                        children: [
                            Symbol("t")
                        ],
                        height: 1,
                    }),
                    Symbol("="),
                    Subtree({
                        label: "tt",
                        children: [ 
                            Symbol("t") 
                        ],
                        height: 1,
                    })
                ],
                height: 2,
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
                                ],
                                height: 1,
                            }),
                            Symbol("<->"),
                            Subtree({
                                label: "wps",
                                children: [
                                    Symbol("ps")
                                ],
                                height: 1,
                            }),
                            Symbol(")")
                        ],
                        height: 2,
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
                                                ],
                                                height: 1,
                                            }),
                                            Symbol("->"),
                                            Subtree({
                                                label: "wps",
                                                children: [
                                                    Symbol("ps")
                                                ],
                                                height: 1,
                                            }),
                                            Symbol(")")
                                        ],
                                        height: 2,
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
                                                        ],
                                                        height: 1,
                                                    }),
                                                    Symbol("->"),
                                                    Subtree({
                                                        label: "wph",
                                                        children: [
                                                            Symbol("ph")
                                                        ],
                                                        height: 1,
                                                    }),
                                                    Symbol(")")
                                                ],
                                                height: 2,
                                            })
                                        ],
                                        height: 3,
                                    }),
                                    Symbol(")")
                                ],
                                height: 4,
                            })
                        ],
                        height: 5,
                    }),
                    Symbol(")")
                ],
                height: 6,
            }
        )
    })
})

describe("unify", _ => {
    it("finds unification for two expressions", _ => {
        //given
        let a = buildSyntaxTreeForTest(~mmFile=setReduced, ~exprStr="wff ( ( ka -> ( la -> mu ) ) -> ( ( ka -> la ) -> ( ka -> mu ) ) )")
        let b = buildSyntaxTreeForTest(~mmFile=setReduced, ~exprStr="wff ( th -> ( ( ph -> ps ) -> ( ph -> ch ) ) )")
        let continue = ref(true)
        let unifSubs = []

        //when
        unify(a, b, unifSubs, continue)

        //then
        Js.Console.log2(`a`, a)
        Js.Console.log2(`b`, b)
        Js.Console.log2(`continue.contents`, continue.contents)
        Js.Console.log2(`unifSubs`, unifSubs)
    })
})