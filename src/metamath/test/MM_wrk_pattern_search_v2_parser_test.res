open Expln_test
open MM_wrk_pattern_search_v2_parser

let testPatternParser = (patternStr:string, expectedResult:result<symSeq,()>) => {
    assertEqMsg( parsePattern(patternStr), expectedResult, patternStr )
}

let seq = (elems:seqGrp):symSeq => {
    {
        flags:"",
        elems
    }
}

describe("parsePattern", _ => {
    it("works as expected", _ => {
        testPatternParser(
            "x",
            Ok(seq(Symbols(["x"])))
        )
        testPatternParser(
            "x y",
            Ok(seq(Symbols(["x","y"])))
        )
        testPatternParser(
            "x $** y",
            Ok(seq(Ordered([seq(Symbols(["x"])), seq(Symbols(["y"]))])))
        )
        testPatternParser(
            "x $** y $**",
            Error(())
        )
        testPatternParser(
            "a b $** c d",
            Ok(seq(Ordered([seq(Symbols(["a","b"])), seq(Symbols(["c","d"]))])))
        )
        testPatternParser(
            "a b $** c d $** e f",
            Ok(seq(Ordered([seq(Symbols(["a","b"])), seq(Symbols(["c","d"])), seq(Symbols(["e","f"]))])))
        )
        testPatternParser(
            "a b $|| c d $|| e f",
            Ok(seq(Unordered([seq(Symbols(["a","b"])), seq(Symbols(["c","d"])), seq(Symbols(["e","f"]))])))
        )
        testPatternParser(
            "a b $** c d $|| e f",
            Ok(seq(Unordered([
                seq(Ordered([seq(Symbols(["a","b"])), seq(Symbols(["c","d"]))])), 
                seq(Symbols(["e","f"]))
            ])))
        )
        testPatternParser(
            "a b $|| c d $** e f",
            Ok(seq(Unordered([
                seq(Symbols(["a","b"])),
                seq(Ordered([seq(Symbols(["c","d"])), seq(Symbols(["e","f"]))])), 
            ])))
        )
    })
})