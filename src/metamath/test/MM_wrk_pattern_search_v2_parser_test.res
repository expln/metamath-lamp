open Expln_test
open MM_wrk_pattern_search_v2_parser

let testPatternParser = (patternStr:string, expectedResult:result<symSeq,()>) => {
    assertEqMsg( parsePattern(patternStr), expectedResult, patternStr )
}

let seq = (elems:seqGrp, ~flags):symSeq => { flags, elems }

let sym = (symbols:array<string>, ~flags:string=""):symSeq => seq(Symbols(symbols), ~flags)
let ord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Ordered(elems), ~flags)
let unord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Unordered(elems), ~flags)

describe("parsePattern", _ => {
    it("works as expected", _ => {
        testPatternParser(
            "x",
            Ok(sym(["x"]))
        )
        testPatternParser(
            "x y",
            Ok(sym(["x","y"]))
        )
        testPatternParser(
            "x $** y",
            Ok(ord([sym(["x"]), sym(["y"])]))
        )
        testPatternParser(
            "x $** y $**",
            Error(())
        )
        testPatternParser(
            "a b $** c d",
            Ok(ord([sym(["a","b"]), sym(["c","d"])]))
        )
        testPatternParser(
            "a b $** c d $** e f",
            Ok(ord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])]))
        )
        testPatternParser(
            "a b $|| c d $|| e f",
            Ok(unord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])]))
        )
        testPatternParser(
            "a b $** c d $|| e f",
            Ok(unord([
                ord([sym(["a","b"]), sym(["c","d"])]), 
                sym(["e","f"])
            ]))
        )
        testPatternParser(
            "a b $|| c d $** e f",
            Ok(unord([
                sym(["a","b"]),
                ord([sym(["c","d"]), sym(["e","f"])]),
            ]))
        )
        testPatternParser(
            "$[ a b $|| c d $] $** e f",
            Ok(ord([
                unord([sym(["a","b"]), sym(["c","d"])]),
                sym(["e","f"]),
            ]))
        )
        testPatternParser(
            "$[ a b $] $|| c d",
            Ok(unord([
                sym(["a","b"]),
                sym(["c","d"]),
            ]))
        )
        testPatternParser(
            "$[ a b $] $|| $[ c d $]",
            Ok(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ]))
        )
        testPatternParser(
            "$[ $[ a b $] $|| $[ c d $] $]",
            Ok(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ]))
        )
        testPatternParser(
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
            $[ g h $]
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
    })
})