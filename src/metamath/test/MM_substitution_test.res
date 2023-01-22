open Expln_test
open MM_parser
open MM_context
open MM_substitution

let testIterateConstParts = (~frmExprStr:string, ~exprStr:string, ~expectedConstParts:array<(int,int)>, ~expectedMatchingConstParts:array<array<(int,int)>>) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/substitutions-test.mm")
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    ctx->applySingleStmt(Axiom({label:"test", expr: ("|- " ++ frmExprStr)->Js_string2.split(" ")}))
    let frm = switch ctx->getFrame("test") {
        | Some(frm) => frm
        | None => failMsg("Cannot find 'test' frame in testIterateConstParts.")
    }
    let frmExpr = frm.asrt->Js_array2.sliceFrom(1)
    let expr = ctx->ctxSymsToIntsExn(exprStr->Js_string2.split(" "))

    //when
    let (actualConstParts, actualMatchingConstParts) = test_iterateConstParts(~ctx, ~frmExpr, ~expr, ~parens)

    //then
    assertEqMsg(actualConstParts, expectedConstParts, "expectedConstParts")
    assertEqMsg(actualMatchingConstParts, expectedMatchingConstParts, "expectedMatchingConstParts")
}

let testIterateSubstitutions = (~frmExprStr:string, ~exprStr:string, ~expectedSubstitutions:array<array<string>>) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/substitutions-test.mm")
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    ctx->applySingleStmt(Axiom({label:"test", expr: ("|- " ++ frmExprStr)->getSpaceSeparatedValuesAsArray}))
    let frm = switch ctx->getFrame("test") {
        | Some(frm) => frm
        | None => failMsg("Cannot find 'test' frame in testIterateSubstitutions.")
    }
    let frmExpr = frm.asrt->Js_array2.sliceFrom(1)
    let expr = ctx->ctxStrToIntsExn(exprStr)

    //when
    let actualSubs = test_iterateSubstitutions(~ctx, ~frmExpr, ~expr, ~parens)

    //then
    let actualSubsStr = actualSubs
        ->Js_array2.map(exprs => {
            exprs->Js_array2.mapi((s,i) => {
                frm.frameVarToSymb->Belt_MapInt.getExn(i)
                    ++ ": "
                    ++ ctxIntsToStrExn(ctx,s)
            })
            ->Js_array2.sortInPlace
        })
    assertEq(actualSubsStr, expectedSubstitutions)
}

describe("iterateConstParts", _ => {
    it("one option", _ => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B",
            ~expectedMatchingConstParts = [
                [(1,1)],
            ]
        )
    })
    it("two options", _ => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B -> C",
            ~expectedMatchingConstParts = [
                [(1,1)],
                [(3,3)],
            ]
        )
    })
    it("three options", _ => {
        testIterateConstParts(
            ~frmExprStr = "a -> b -> c",
            ~expectedConstParts = [(1,1), (3,3)],
            ~exprStr = "A -> B -> C -> D",
            ~expectedMatchingConstParts = [
                [(1,1), (3,3)],
                [(1,1), (5,5)],
                [(3,3), (5,5)],
            ]
        )
    })
    it("assertion begins with a constant", _ => {
        testIterateConstParts(
            ~frmExprStr = "|- a -> b",
            ~expectedConstParts = [(0,0), (2,2)],
            ~exprStr = "|- A -> B -> C",
            ~expectedMatchingConstParts = [
                [(0,0), (2,2)],
                [(0,0), (4,4)],
            ]
        )
    })
    it("assertion begins with a constant and same expression is present inside of the statement", _ => {
        testIterateConstParts(
            ~frmExprStr = "|- a -> b",
            ~expectedConstParts = [(0,0), (2,2)],
            ~exprStr = "|- A -> |- B -> C",
            ~expectedMatchingConstParts = [
                [(0,0), (2,2)],
                [(0,0), (5,5)],
            ]
        )
    })
    it("there_are_sequences_of_more_than_one_constant", _ => {
        testIterateConstParts(
            ~frmExprStr = "|- ( a -> b ) -> ( a -> b )",
            ~expectedConstParts = [(0,1),(3,3),(5,7),(9,9),(11,11)],
            ~exprStr = "|- ( A -> B ) -> ( A -> B )",
            ~expectedMatchingConstParts = [
                [(0,1),(3,3),(5,7),(9,9),(11,11)]
            ]
        )
    })
    it("few_options_and_asrt_ends_with_constant", _ => {
        testIterateConstParts(
            ~frmExprStr = "a -> b ->",
            ~expectedConstParts = [(1,1),(3,3)],
            ~exprStr = "A -> B -> C ->",
            ~expectedMatchingConstParts = [
                [(1,1),(5,5)],
                [(3,3),(5,5)],
            ]
        )
    })
    it("var_and_const", _ => {
        testIterateConstParts(
            ~frmExprStr = "a ->",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B -> C ->",
            ~expectedMatchingConstParts = [
                [(5,5)]
            ]
        )
    })
    it("gaps_between_some_constant_parts_are_less_than_number_of_variables_and_asrt_starts_with_constant", _ => {
        testIterateConstParts(
            ~frmExprStr = "|- a b c -> a b d",
            ~expectedConstParts = [(0,0),(4,4)],
            ~exprStr = "|- A B C -> A B D -> C",
            ~expectedMatchingConstParts = [
                [(0,0),(4,4)]
            ]
        )
    })
    it("gaps_between_some_constant_parts_are_less_than_number_of_variables_and_asrt_starts_with_non_constant", _ => {
        testIterateConstParts(
            ~frmExprStr = "a b c -> a b d",
            ~expectedConstParts = [(3,3)],
            ~exprStr = "A -> B -> C D E",
            ~expectedMatchingConstParts = [
                [(3,3)]
            ]
        )
        testIterateConstParts(
            ~frmExprStr = "a b c -> a b d",
            ~expectedConstParts = [(3,3)],
            ~exprStr = "A -> B C D",
            ~expectedMatchingConstParts = [ ]
        )
        testIterateConstParts(
            ~frmExprStr = "( a b )",
            ~expectedConstParts = [(0,0),(3,3)],
            ~exprStr = "( a )",
            ~expectedMatchingConstParts = [ ]
        )
    })
    it("there_are_no_constants", _ => {
        testIterateConstParts(
            ~frmExprStr = "a b",
            ~expectedConstParts = [],
            ~exprStr = "A -> B",
            ~expectedMatchingConstParts = [ 
                [] 
            ]
        )
    })
    it("doesn't match when expr.length < frmExpr.length (no constants in frmExpr)", _ => {
        testIterateConstParts(
            ~frmExprStr = "a b c d",
            ~expectedConstParts = [],
            ~exprStr = "A B C",
            ~expectedMatchingConstParts = []
        )
    })
    it("doesn't fail when expr.length < frmExpr.length (expr consists of constants only and matches beginning of frmExp)", _ => {
        testIterateConstParts(
            ~frmExprStr = "( + a b -> c d )",
            ~expectedConstParts = [(0,1),(4,4),(7,7)],
            ~exprStr = "( +",
            ~expectedMatchingConstParts = []
        )
    })
})

describe("iterateSubstitutions", _ => {
    it("one_option", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- a -> b",
            ~exprStr = "|- A -> B",
            ~expectedSubstitutions = [
                [
                    "a: A",
                    "b: B",
                ],
            ]
        )
    })
    it("two_options", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- a -> b",
            ~exprStr = "|- A -> B -> C",
            ~expectedSubstitutions = [
                ["a: A", "b: B -> C"],
                ["a: A -> B", "b: C"],
            ]
        )
    })
    it("zero_options", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- a -> b",
            ~exprStr = "|- A = B",
            ~expectedSubstitutions = [ ]
        )
    })
    it("there_are_no_constants_in_assertion", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "a b",
            ~exprStr = "A = B",
            ~expectedSubstitutions = [
                ["a: A",   "b: = B"],
                ["a: A =", "b: B"],
            ]
        )
    })
    it("there_are_no_variables_in_assertion_and_assertion_matches_the_statement", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "A = B",
            ~exprStr = "A = B",
            ~expectedSubstitutions = [
                []
            ]
        )
    })
    it("there_are_no_variables_in_assertion_and_assertion_doesnt_match_the_statement", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "A -> B",
            ~exprStr = "A = B",
            ~expectedSubstitutions = [ ]
        )
    })
    it("one_variable_repeats", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- a -> b",
            ~exprStr = "|- A -> B -> A -> B",
            ~expectedSubstitutions = [
                ["a: A",            "b: B -> A -> B"],
                ["a: A -> B",       "b: A -> B"],
                ["a: A -> B -> A",  "b: B"],
            ]
        )
        testIterateSubstitutions(
            ~frmExprStr = "|- a -> a",
            ~exprStr = "|- A -> B -> A -> B",
            ~expectedSubstitutions = [
                ["a: A -> B"],
            ]
        )
    })
    it("variable is reused in another varGroup in opposite order", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "a     b     | b     a    ",
            ~exprStr =    "[ A ] [ B ] | [ B ] [ A ]",
            ~expectedSubstitutions = [
                [
                    "a: [ A ]",
                    "b: [ B ]",
                ],
            ]
        )
    })
    it("case1_from_set_mm", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- ( ( a e. f /\\ b e. f /\\ c e. f ) -> ( ( a e b ) d c ) = ( ( a d c ) + ( b d c ) ) )",
            ~exprStr = "|- ( ( A e. ( BaseSet ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) /\\ B e. ( BaseSet ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) /\\ C e. ( BaseSet ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) ) -> ( ( A ( +v ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) B ) ( .iOLD ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) C ) = ( ( A ( .iOLD ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) C ) + ( B ( .iOLD ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) ) C ) ) )",
            ~expectedSubstitutions = [
                [
                    "a: A",
                    "b: B",
                    "c: C",
                    "d: ( .iOLD ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) )",
                    "e: ( +v ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) )",
                    "f: ( BaseSet ` if ( U e. CPreHilOLD , U , <. <. + , x. >. , abs >. ) )"
                ],
            ]
        )
    })
    it("case2_from_set_mm", _ => {
        testIterateSubstitutions(
            ~frmExprStr = "|- ( ph -> ( ( ps -> ch ) -> ( ( ( th -> ps ) -> ( ch -> ta ) ) -> ( ps -> ta ) ) ) )",
            ~exprStr = "|- ( ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) -> ( ( ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) -> ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) ) -> ( ( ( ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) -> ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) ) -> ( ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) -> ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) ) ) -> ( ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) -> ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) ) ) ) ) )",
            ~expectedSubstitutions = [
                [
                    "ch: ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) )",
                    "ph: ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) )",
                    "ps: ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) )",
                    "ta: ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) )",
                    "th: ( ( ( ph -> ps ) -> ( ph -> ps ) ) -> ( ( ( ( ( ph -> ps ) -> ch ) -> ( ph -> ps ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) -> ( ( ph -> ps ) -> ( ph -> ps ) ) ) )",
                ],
            ]
        )
    })
})