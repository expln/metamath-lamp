open Expln_test
open MM_wrk_pattern_search_v2_parser

let testPatternParser = (text:string, expectedResult:option<array<subpat>>) => {
    assertEqMsg( parsePattern(text), expectedResult, text )
}

let seq = (elems:seqGrp, ~flags):symSeq => { flags, elems }

let sym = (symbols:array<string>, ~flags:string=""):symSeq => seq(Symbols(symbols), ~flags)
let ord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Ordered(elems), ~flags)
let unord = (elems:array<symSeq>, ~flags:string=""):symSeq => seq(Unordered(elems), ~flags)
let pat = (symSeq:symSeq, ~target:subpatTarget=Frm):array<subpat> => [{target, symSeq}]

describe("MM_wrk_pattern_search_v2_parser", _ => {
    it("parsePattern works as expected", _ => {
        testPatternParser(
            "x",
            Some(pat(sym(["x"])))
        )
        testPatternParser(
            "x y",
            Some(pat(sym(["x","y"])))
        )
        testPatternParser(
            "x $** y",
            Some(pat(ord([sym(["x"]), sym(["y"])])))
        )
        testPatternParser(
            "x $|| y",
            Some(pat(unord([sym(["x"]), sym(["y"])])))
        )
        testPatternParser(
            "a b $** c d",
            Some(pat(ord([sym(["a","b"]), sym(["c","d"])])))
        )
        testPatternParser(
            "a b $** c d $** e f",
            Some(pat(ord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])])))
        )
        testPatternParser(
            "a b $|| c d $|| e f",
            Some(pat(unord([sym(["a","b"]), sym(["c","d"]), sym(["e","f"])])))
        )
        testPatternParser(
            "a b $** c d $|| e f",
            Some(pat(unord([
                ord([sym(["a","b"]), sym(["c","d"])]), 
                sym(["e","f"])
            ])))
        )
        testPatternParser(
            "a b $|| c d $** e f",
            Some(pat(unord([
                sym(["a","b"]),
                ord([sym(["c","d"]), sym(["e","f"])]),
            ])))
        )
        testPatternParser(
            "$[ a b $|| c d $] $** e f",
            Some(pat(ord([
                unord([sym(["a","b"]), sym(["c","d"])]),
                sym(["e","f"]),
            ])))
        )
        testPatternParser(
            "$[ a b $] $|| c d",
            Some(pat(unord([
                sym(["a","b"]),
                sym(["c","d"]),
            ])))
        )
        testPatternParser(
            "$[ a b $] $|| $[ c d $]",
            Some(pat(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ])))
        )
        testPatternParser(
            "$[ $[ a b $] $|| $[ c d $] $]",
            Some(pat(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ])))
        )
        testPatternParser(
            "$[ a b $|| $[ c d $] $]",
            Some(pat(unord([
                sym(["a", "b"]),
                sym(["c", "d"]),
            ])))
        )
        testPatternParser(
            "$[ $[ a b $] $** $[ c d $] $] $** $[ e f $]",
            Some(pat(ord([
                ord([
                    sym(["a", "b"]),
                    sym(["c", "d"]),
                ]),
                sym(["e", "f"])
            ])))
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
            g h
            ",
            Some(pat(unord([
                ord([
                    sym(["a", "b"]),
                    unord([
                        sym(["c", "d"]),
                        sym(["e", "f"]),
                    ])
                ]),
                sym(["g", "h"])
            ])))
        )
        testPatternParser(
            "$[+ a b c $]",
            Some(pat(sym(["a", "b", "c"], ~flags="+")))
        )
        testPatternParser(
            "$[+ $[- a b c $] $]",
            Some(pat(sym(["a", "b", "c"], ~flags="-")))
        )
        testPatternParser(
            "$[+ $[ $[- a b c $] $] $]",
            Some(pat(sym(["a", "b", "c"], ~flags="-")))
        )
        testPatternParser(
            "$[+ a b c $|| d $]",
            Some(pat(unord([sym(["a", "b", "c"]), sym(["d"])], ~flags="+")))
        )
        testPatternParser(
            "$[+ $[ a b c $] $|| d $]",
            Some(pat(unord([sym(["a", "b", "c"]), sym(["d"])], ~flags="+")))
        )
        testPatternParser(
            "$[+ $[- a b c $] $|| d $]",
            Some(pat(unord([sym(["a", "b", "c"], ~flags="-"), sym(["d"])], ~flags="+")))
        )
        testPatternParser(
            "$[+ $[- a b c $] $|| $[ $[ $[ d e $] $] $] $]",
            Some(pat(unord([sym(["a", "b", "c"], ~flags="-"), sym(["d", "e"])], ~flags="+")))
        )
    })
    
    it("doesn't parse invalid patterns", _ => {
        testPatternParser(
            "x $**",
            None
        )
        testPatternParser(
            "x $** y $**",
            None
        )
        testPatternParser(
            "$** y $**",
            None
        )
        testPatternParser(
            "$** y",
            None
        )
        testPatternParser(
            "$**",
            None
        )
        testPatternParser(
            "x $||",
            None
        )
        testPatternParser(
            "x $|| y $||",
            None
        )
        testPatternParser(
            "$|| y $||",
            None
        )
        testPatternParser(
            "$|| y",
            None
        )
        testPatternParser(
            "$||",
            None
        )
        testPatternParser(
            "$[ a b $] $|| $[ c d",
            None
        )
    })
    
    it("multiple subpatterns should begin with $", _ => {
        testPatternParser(
            "$[ a $** b $] $[ c $** d $]",
            None
        )
        
    })
})