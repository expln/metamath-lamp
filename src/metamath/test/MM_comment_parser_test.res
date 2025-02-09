open Expln_test
open MM_comment_parser

describe("splitCommentIntoParts", _ => {
    it("splits text as expected", _ => {
        assertEq( splitCommentIntoParts(""), [] )
        assertEq( splitCommentIntoParts(" "), [Text(" ")] )
        assertEq( splitCommentIntoParts("a"), [Text("a")] )
        assertEq( splitCommentIntoParts("a "), [Text("a ")] )
        assertEq( splitCommentIntoParts(" a "), [Text(" a ")] )
        assertEq( splitCommentIntoParts(" a"), [Text(" a")] )
        assertEq( splitCommentIntoParts("`a`"), [Text("`a`")] ) // backticks should be surrounded by spaces
        assertEq( splitCommentIntoParts(" `a` "), [Text(" `a` ")] ) // backticks should be _surrounded_ by spaces
        assertEq( splitCommentIntoParts("~a"), [Text("~a")] ) // tildes should be surrounded by spaces
        assertEq( splitCommentIntoParts(" ~a "), [Text(" ~a ")] ) // tildes should be _surrounded_ by spaces
        assertEq( splitCommentIntoParts("``"), [Text("`")] )
        assertEq( splitCommentIntoParts("~~"), [Text("~")] )
        assertEq( splitCommentIntoParts("` a b c `"), [MathMode("a b c")] )
        assertEq( splitCommentIntoParts("` a b c ` e"), [MathMode("a b c"), Text(" e")] )
        assertEq( splitCommentIntoParts("b ` a b c ` e"), [Text("b "), MathMode("a b c"), Text(" e")] )
        assertEq( splitCommentIntoParts("b ` a b c `"), [Text("b "), MathMode("a b c")] )
        assertEq( splitCommentIntoParts("~ abc"), [LabelMode("abc")] )
        assertEq( splitCommentIntoParts("~ abc e"), [LabelMode("abc"), Text(" e")] )
        assertEq( splitCommentIntoParts("b ~ abc e"), [Text("b "), LabelMode("abc"), Text(" e")] )
        assertEq( splitCommentIntoParts("b ~ abc"), [Text("b "), LabelMode("abc")] )
        assertEq( 
            splitCommentIntoParts("123 ` a b c ` 456 ~ def 789"), 
            [Text("123 "), MathMode("a b c"), Text(" 456 "), LabelMode("def"), Text(" 789")] 
        )
        assertEq( 
            splitCommentIntoParts("1``2~~3 ` a `` b ~ c ` 4``5~~6 ~ d``e~~f 7``8~~9"), 
            [Text("1`2~3 "), MathMode("a ` b ~ c"), Text(" 4`5~6 "), LabelMode("d`e~f"), Text(" 7`8~9")] 
        )
        assertEq( splitCommentIntoParts("` a~~c `"), [MathMode("a~~c")] )
    })
})