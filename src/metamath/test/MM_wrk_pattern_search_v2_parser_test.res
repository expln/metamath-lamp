open Expln_test
open MM_wrk_pattern_search_v2_parser

let testSymSeqParser = (text:string, expectedResult:result<symSeq,()>) => {
    assertEqMsg( parseSymSeq(text), expectedResult, text )
}

let seq = (elems:seqGrp, ~flags):symSeq => { flags, elems }

let sym = (symbols:array<string>, ~flags:string=""):symSeq => seq(Symbols(symbols), ~flags)
let ord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Ordered(elems), ~flags)
let unord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Unordered(elems), ~flags)

describe("MM_wrk_pattern_search_v2_parser", _ => {
    it("parseSymSeq works as expected", _ => {
        testSymSeqParser(
            "x",
            Ok(sym(["x"]))
        )
        testSymSeqParser(
            "x y",
            Ok(sym(["x","y"]))
        )
        testSymSeqParser(
            "x $** y",
            Ok(ord([sym(["x"]), sym(["y"])]))
        )
        testSymSeqParser(
            "x $** y $**",
            Error(())
        )
        testSymSeqParser(
            "a b $** c d",
            Ok(ord([sym(["a","b"]), sym(["c","d"])]))
        )
        testSymSeqParser(
            "a b $** c d $** e f",
            Ok(ord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])]))
        )
        testSymSeqParser(
            "a b $|| c d $|| e f",
            Ok(unord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])]))
        )
        testSymSeqParser(
            "a b $** c d $|| e f",
            Ok(unord([
                ord([sym(["a","b"]), sym(["c","d"])]), 
                sym(["e","f"])
            ]))
        )
        testSymSeqParser(
            "a b $|| c d $** e f",
            Ok(unord([
                sym(["a","b"]),
                ord([sym(["c","d"]), sym(["e","f"])]),
            ]))
        )
        testSymSeqParser(
            "$[ a b $|| c d $] $** e f",
            Ok(ord([
                unord([sym(["a","b"]), sym(["c","d"])]),
                sym(["e","f"]),
            ]))
        )
        testSymSeqParser(
            "$[ a b $] $|| c d",
            Ok(unord([
                sym(["a","b"]),
                sym(["c","d"]),
            ]))
        )
        testSymSeqParser(
            "$[ a b $] $|| $[ c d",
            Error(())
        )
        testSymSeqParser(
            "$[ a b $] $|| $[ c d $]",
            Ok(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ]))
        )
        testSymSeqParser(
            "$[ $[ a b $] $|| $[ c d $] $]",
            Ok(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ]))
        )
        testSymSeqParser(
            "$[ a b $|| $[ c d $] $]",
            Ok(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ]))
        )
        testSymSeqParser(
            "$[ $[ a b $] $** $[ c d $] $] $** $[ e f $]",
            Ok(ord([
                ord([
                    sym(["a", "b"]),
                    sym(["c", "d"]),
                ]),
                sym(["e", "f"])
            ]))
        )
        testSymSeqParser(
            "
            $[ 
                a b 
                $** 
                $[ 
                    $[ c d $]
                    $||
                    $[ e f $]
                $] 
            $] 
            $|| 
            g h
            ",
            Ok(unord([
                ord([
                    sym(["a", "b"]),
                    unord([
                        sym(["c", "d"]),
                        sym(["e", "f"]),
                    ])
                ]),
                sym(["g", "h"])
            ]))
        )
        testSymSeqParser(
            "$[+ a b c $]",
            Ok(sym(["a", "b", "c"], ~flags="+"))
        )
        testSymSeqParser(
            "$[+ $[- a b c $] $]",
            Ok(sym(["a", "b", "c"], ~flags="-"))
        )
        testSymSeqParser(
            "$[+ $[ $[- a b c $] $] $]",
            Ok(sym(["a", "b", "c"], ~flags="-"))
        )
        testSymSeqParser(
            "$[+ a b c $|| d $]",
            Ok(unord([sym(["a", "b", "c"]), sym(["d"])], ~flags="+"))
        )
        testSymSeqParser(
            "$[+ $[ a b c $] $|| d $]",
            Ok(unord([sym(["a", "b", "c"]), sym(["d"])], ~flags="+"))
        )
        testSymSeqParser(
            "$[+ $[- a b c $] $|| d $]",
            Ok(unord([sym(["a", "b", "c"], ~flags="-"), sym(["d"])], ~flags="+"))
        )
        testSymSeqParser(
            "$[+ $[- a b c $] $|| $[ $[ $[ d e $] $] $] $]",
            Ok(unord([sym(["a", "b", "c"], ~flags="-"), sym(["d", "e"])], ~flags="+"))
        )
    })
})