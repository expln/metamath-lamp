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

let rec syntaxTreeToJson = (node:syntaxTreeNode):Js_json.t => {
    [
        ("label", node.label->Js_json.string),
        ("children", 
            node.children->Js_array2.map(c => {
                switch c {
                    | Subtree(childNode) => syntaxTreeToJson(childNode)
                    | Symbol({sym}) => sym->Js_json.string
                }
            })->Js_json.array
        )
    ]->Js.Dict.fromArray->Js_json.object_
}

let buildSyntaxTreeForTest = (
    ~mmFile:string, 
    ~ctxUpdate:option<mmContext=>mmContext>=?,
    ~exprStr:array<string>, 
    ()
):(mmContext,array<syntaxTreeNode>) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let ctx = ctxUpdate->Belt_Option.map(update => update(ctx))->Belt.Option.getWithDefault(ctx)
    let expr = exprStr->Js_array2.map(e => e->getSpaceSeparatedValuesAsArray->ctxSymsToIntsExn(ctx, _))
    let proofTree = proveFloatings(
        ~wrkCtx=ctx,
        ~frms=prepareFrmSubsData(~ctx, ()),
        ~frameRestrict = { useDisc:true, useDepr:true, useTranDepr:true },
        ~floatingsToProve = expr,
        ~parenCnt=parenCntMake(ctx->ctxStrToIntsExn(parens), ()),
    )
    let proofTreeDto = proofTreeToDto(proofTree, expr)
    let nodes = expr->Js.Array2.map(e => {
        proofTreeDto.nodes->Js.Array2.find(node => node.expr->exprEq(e))->Belt.Option.getExn
    })
    let proofTables = nodes->Js.Array2.map(n => createProofTable(~tree=proofTreeDto, ~root=n, ()))

    (
        ctx,
        proofTables->Js.Array2.map(proofTable => {
            switch buildSyntaxTree(ctx, proofTable, proofTable->Js_array2.length-1) {
                | Error(msg) => failMsg(msg)
                | Ok(syntaxTree) => syntaxTree
            }
        })
    )
}

let testSyntaxTree = (~mmFile, ~exprStr, ~expectedSyntaxTree:syntaxTreeNodeTest) => {
    @warning("-8")
    let (_, [actualSyntaxTree]) = buildSyntaxTreeForTest(~mmFile, ~exprStr=[exprStr], ())
    assertEqMsg(
        actualSyntaxTree->syntaxTreeToSyntaxTreeTest, 
        expectedSyntaxTree, 
        `testSyntaxTree for: ${exprStr}`
    )
}

let demo0 = "./src/metamath/test/resources/demo0._mm"
let setReduced = "./src/metamath/test/resources/set-reduced._mm"

describe("buildSyntaxTree", _ => {
    it("builds correct syntax trees for WWFs", _ => {
        testSyntaxTree(~mmFile=demo0, ~exprStr="term t", 
            ~expectedSyntaxTree = {
                label: "tt",
                children: [
                    Symbol("t"),
                ],
                height: 1,
            }
        )

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
        @warning("-8")
        let (_, [a,b]) = buildSyntaxTreeForTest(
            ~mmFile=setReduced, 
            ~ctxUpdate = ctx => {
                ctx->applySingleStmt(Var({symbols:["&W1", "&W2", "&W3", "&W4"]}), ())
                ctx->applySingleStmt(Floating({label:"W1-wff", expr:["wff", "&W1"]}), ())
                ctx->applySingleStmt(Floating({label:"W2-wff", expr:["wff", "&W2"]}), ())
                ctx->applySingleStmt(Floating({label:"W3-wff", expr:["wff", "&W3"]}), ())
                ctx->applySingleStmt(Floating({label:"W4-wff", expr:["wff", "&W4"]}), ())
                ctx
            },
            ~exprStr=[
                "wff ( ( &W3 -> ( &W4 -> &W2 ) ) -> ( ( &W3 -> &W4 ) -> ( &W3 -> &W2 ) ) )",
                "wff ( &W1                       -> ( ( ph  -> ps  ) -> ( ph  -> ch  ) ) )",
            ],
            ()
        )
        let continue = ref(true)
        let foundSubs = Belt_HashMapString.make(~hintSize = 100)

        //when
        unify(a, b, ~foundSubs, ~continue, ~isMetavar=Js_string2.startsWith(_,"&"))

        //then
        // a->syntaxTreeToJson->Expln_utils_common.stringify->Expln_utils_files.writeStringToFile("/syntax-trees/a.json")
        // b->syntaxTreeToJson->Expln_utils_common.stringify->Expln_utils_files.writeStringToFile("/syntax-trees/b.json")
        let foundSubsStr = foundSubs->Belt_HashMapString.toArray->Js.Array2.map(((v,expr)) => (v,expr->Js_array2.joinWith(" ")))->Belt_HashMapString.fromArray
        assertEq(continue.contents, true)
        assertEq(foundSubsStr->Belt_HashMapString.size, 4)
        assertEq(foundSubsStr->Belt_HashMapString.get("&W3"), Some("ph"))
        assertEq(foundSubsStr->Belt_HashMapString.get("&W4"), Some("ps"))
        assertEq(foundSubsStr->Belt_HashMapString.get("&W2"), Some("ch"))
        assertEq(foundSubsStr->Belt_HashMapString.get("&W1"), Some("( ph -> ( ps -> ch ) )"))
    })
})