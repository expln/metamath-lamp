open Expln_test
open MM_parser

let collectComments = (ast:mmAstNode):Belt_HashMapString.t<string> => {
    let lastComment = ref("########################")
    let collectedComments = Belt_HashMapString.make(~hintSize=20)
    traverseAst(
        (),
        ast,
        ~process = (_, node) => {
            @warning("-8")
            switch node {
                | {stmt:Comment({text})} => lastComment := text
                | {stmt:Axiom({label})} => {
                    collectedComments->Belt_HashMapString.set(label, lastComment.contents)
                    lastComment := "########################"
                }
                | _ => ()
            }
            None
        }
    )->ignore
    collectedComments
}

let assertCommentEq = (allComments:Belt_HashMapString.t<string>, testCaseName:string, expectedComment:string) => {
    assertEqMsg(
        allComments->Belt_HashMapString.get(testCaseName)
            ->Option.getExn(~message=`No comment found for the test case ${testCaseName}.`),
        expectedComment,
        testCaseName
    )
}

describe("parseMmFile", _ => {
    it("parses a valid mm file", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0._mm")

        //when
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)

        //then
        let actual = stmtToStrRec(ast)->Array.filter(s => !(s->String.trim->String.startsWith("$(")))
        let expected = [
                "$c 0 + = -> ( ) term wff & => |- [ { ] } <. .> $.",
                "$v t r s P Q $.",
                "tt $f term t $.",
                "tr $f term r $.",
                "ts $f term s $.",
                "wp $f wff P $.",
                "wq $f wff Q $.",
                "tze $a term 0 $.",
                "tpl $a term ( t + r ) $.",
                "weq $a wff t = r $.",
                "wim $a wff ( P -> Q ) $.",
                "a1 $a |- ( t = r -> ( t = s -> r = s ) ) $.",
                "a2 $a |- ( t + 0 ) = t $.",
                "${",
                "    min $e |- P $.",
                "    maj $e |- ( P -> Q ) $.",
                "    mp $a |- Q $.",
                "$}",
                "th1 $p |- t = t $= tt tze tpl tt weq tt tt weq tt a2 tt tze tpl tt weq tt tze tpl tt weq tt tt weq wim tt a2 tt tze tpl tt tt a1 mp mp $.",
                "th2 $p |- t = t $= tt tze tpl tt weq tt tt weq tt a2 tt tze tpl tt weq tze tt tpl tt weq tt tt weq wim tt a2 tt tze tpl tt tt a1 mp mp $.",
                "paren1 $a |- [ t ] = [ t ] $.",
                "paren2 $a |- { t } = { t } $.",
                "paren3 $a |- <. t .> = <. t .> $.",
            ]
        //assertEq(actual->Array.length, expected->Array.length)
        //for i in 0 to actual->Array.length-1 {
            //assertEq(actual[i], expected[i])
        //}
        assertEq(actual, expected)
    })

    it("does not modify comments", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/test_comment_parser._mm")
            ->String.replaceAll("TAB", "\t")
            ->String.replaceAll("NEW_LINE", "\n")
            ->String.replaceAll("RN", "\r\n")

        //when
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText)

        //then
        let allComments = collectComments(ast)
        assertCommentEq(allComments, "empty_comment_1_space", " ")
        assertCommentEq(allComments, "empty_comment_2_spaces", "  ")
        assertCommentEq(allComments, "empty_comment_3_spaces", "   ")
        assertCommentEq(allComments, "empty_comment_1_tab", "\t")
        assertCommentEq(allComments, "empty_comment_2_tabs", "\t\t")
        assertCommentEq(allComments, "empty_comment_3_tabs", "\t\t\t")
        assertCommentEq(allComments, "empty_comment_1_new_line", "\n")
        assertCommentEq(allComments, "empty_comment_2_new_lines", "\n\n")
        assertCommentEq(allComments, "empty_comment_3_new_lines", "\n\n\n")
        assertCommentEq(allComments, "empty_comment_1_rn", "\r\n")
        assertCommentEq(allComments, "empty_comment_2_rn", "\r\n\r\n")
        assertCommentEq(allComments, "empty_comment_3_rn", "\r\n\r\n\r\n")
        assertCommentEq(allComments, "non_empty_comment_1_word", " abc ")
        assertCommentEq(allComments, "non_empty_comment_2_words", " abc def ")
    })
})