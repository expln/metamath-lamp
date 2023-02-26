open Expln_test
open MM_parser

describe("parseMmFile", _ => {
    it("parses a valid mm file", _ => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/demo0.mm")

        //when
        let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ())

        //then
        let actual = stmtToStrRec(ast)->Js_array2.filter(s => !(s->Js_string2.trim->Js_string2.startsWith("$(")))
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
        //assertEq(actual->Js_array2.length, expected->Js_array2.length)
        //for i in 0 to actual->Js_array2.length-1 {
            //assertEq(actual[i], expected[i])
        //}
        assertEq(actual, expected)
    })
})