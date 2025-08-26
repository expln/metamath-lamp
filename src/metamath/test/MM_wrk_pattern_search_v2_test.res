open Expln_test
open MM_wrk_pattern_search_v2

type rec testSeqGrp = 
    | Adj(array<int>)
    | Ord(array<testSeqGrp>)
    | Unord(array<testSeqGrp>)

let makeSeq = (seq:array<int>, varTypes:array<int>): array<sym> => {
    let maxVar = seq->Array.reduce(-1, (maxVar, s) => if 0 <= s {Math.Int.max(maxVar,s)} else {maxVar})
    let variables = Belt_Array.range(0,maxVar)->Array.map(v => {
        {
            typ: varTypes->Array.getUnsafe(v),
            capVar: -1, 
            capVarIdx: -1
        }
    })
    seq->Array.map(i => {
        {
            constOrVar: if i < 0 { Const(i) } else { Var(variables->Array.getUnsafe(i)) },
            matchedIdx: -1
        }
    })
}

let rec makeSymSeq = (seq:testSeqGrp, varTypes:array<int>, minConstMismatchIdx:int): symSeq => {
    let elems = switch seq {
        | Adj(seq) => Adjacent(makeSeq(seq, varTypes))
        | Ord(childElems) => Ordered(childElems->Array.map(ch=>makeSymSeq(ch,varTypes, minConstMismatchIdx)))
        | Unord(childElems) => Unordered(childElems->Array.map(ch=>makeSymSeq(ch,varTypes, minConstMismatchIdx)))
    }
    { elems, minConstMismatchIdx, }
}

let assertMatches = (
    ~title:string, ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>, ~expectedIndices:array<int>
):unit => {
    let seq = makeSymSeq(seq, varTypes, expr->Array.length)
    assertEqMsg(exprIncludesSeq( ~expr, ~seq, ~varTypes ), true, `the pattern didn't match in '${title}'`)
    assertEqMsg(getMatchedIndices(seq), expectedIndices, `indices didn't match in '${title}'`)
}

describe("exprIncludesSeq", _ => {
    it("correctly records matched indices", _ => {
        assertMatches(
            ~title="single constant; single constant",
            ~expr=[-1],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )
    })
})