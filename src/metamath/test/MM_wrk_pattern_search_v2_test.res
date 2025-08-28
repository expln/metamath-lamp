open Expln_test
open MM_wrk_pattern_search_v2

type rec testSeqGrp = 
    | Adj(array<int>)
    | NonAdj(array<int>)
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
        | NonAdj(seq) => {
            Ordered(seq->Array.map(i => {
                {
                    elems: Adjacent(makeSeq([i], varTypes)),
                    minConstMismatchIdx,
                }
            }))
        }
        | Ord(childElems) => Ordered(childElems->Array.map(ch=>makeSymSeq(ch,varTypes, minConstMismatchIdx)))
        | Unord(childElems) => Unordered(childElems->Array.map(ch=>makeSymSeq(ch,varTypes, minConstMismatchIdx)))
    }
    { elems, minConstMismatchIdx, }
}

let assertMatches = (
    ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>, ~expectedIndices:array<int>
):unit => {
    let seq = makeSymSeq(seq, varTypes, expr->Array.length)
    assertEq(exprIncludesSeq( ~expr, ~seq, ~varTypes ), true)
    assertEq(getMatchedIndices(seq), expectedIndices)
}

let assertDoesntMatch = (
    ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>
):unit => {
    let seq = makeSymSeq(seq, varTypes, expr->Array.length)
    assertEq(exprIncludesSeq( ~expr, ~seq, ~varTypes ), false)
}

describe("exprIncludesSeq", _ => {
    it("single constant; matching single constant", _ => {
        assertMatches(
            ~expr=[-1],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )
    })
    it("single constant; non-matching single constant", _ => {
        assertDoesntMatch(
            ~expr=[-1],
            ~seq=Adj([-2]),
            ~varTypes=[],
        )
    })
    it("two constants; matching single constant on the left", _ => {
        assertMatches(
            ~expr=[-1, -2],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )
    })
    it("two constants; matching single constant on the right", _ => {
        assertMatches(
            ~expr=[-1, -2],
            ~seq=Adj([-2]),
            ~varTypes=[],
            ~expectedIndices=[1]
        )
    })
    it("two constants; non-matching single constant", _ => {
        assertDoesntMatch(
            ~expr=[-1, -2],
            ~seq=Adj([-3]),
            ~varTypes=[],
        )
    })
    it("multiple constants; matching adjacent constants on the left", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-1, -2, -3]),
            ~varTypes=[],
            ~expectedIndices=[0, 1, 2]
        )
    })
    it("multiple constants; matching adjacent constants in the middle", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-3, -4, -5]),
            ~varTypes=[],
            ~expectedIndices=[2, 3, 4]
        )
    })
    it("multiple constants; matching adjacent constants on the right", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-5, -6, -7]),
            ~varTypes=[],
            ~expectedIndices=[4, 5, 6]
        )
    })
    it("multiple constants; non-matching adjacent constants", _ => {
        assertDoesntMatch(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-6, -5, -7]),
            ~varTypes=[],
        )
    })
    it("multiple constants; matching non-adjacent constants on the left", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-1, -3, -6]),
            ~varTypes=[],
            ~expectedIndices=[0, 2, 5]
        )
    })
    it("multiple constants; matching non-adjacent constants in the middle", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -4, -6]),
            ~varTypes=[],
            ~expectedIndices=[1, 3, 5]
        )
    })
    it("multiple constants; matching non-adjacent constants on the right", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -5, -7]),
            ~varTypes=[],
            ~expectedIndices=[1, 4, 6]
        )
    })
    it("multiple constants; non-matching non-adjacent constants", _ => {
        assertDoesntMatch(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -6, -4]),
            ~varTypes=[],
        )
    })
    it("multiple constants; matching unordered single constants on the left", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-5]), Adj([-3]), Adj([-1])]),
            ~varTypes=[],
            ~expectedIndices=[0, 2, 4]
        )
    })
    it("multiple constants; matching unordered single constants in the middle", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-7]), Adj([-3]), Adj([-4])]),
            ~varTypes=[],
            ~expectedIndices=[2, 3, 6]
        )
    })
    it("multiple constants; matching unordered single constants on the right", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-9]), Adj([-5]), Adj([-2])]),
            ~varTypes=[],
            ~expectedIndices=[1, 4, 8]
        )
    })
    it("multiple constants; non-matching unordered single constants", _ => {
        assertDoesntMatch(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-9]), Adj([-5]), Adj([-9])]),
            ~varTypes=[],
        )
    })
    it("multiple constants; matching unordered groups of constants on the left", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-7,-8]), Adj([-3,-4,-5]), Adj([-1,-2])]),
            ~varTypes=[],
            ~expectedIndices=[0, 1, 2, 3, 4, 6, 7]
        )
    })
    it("multiple constants; matching unordered groups of constants in the middle", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-7, -8]), Adj([-2, -3]), Adj([-5])]),
            ~varTypes=[],
            ~expectedIndices=[1, 2, 4, 6, 7]
        )
    })
    it("multiple constants; matching unordered groups of constants on the right", _ => {
        assertMatches(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-7, -8, -9]), Adj([-5]), Adj([-2, -3])]),
            ~varTypes=[],
            ~expectedIndices=[1, 2, 4, 6, 7, 8]
        )
    })
    it("multiple constants; non-matching unordered groups of constants", _ => {
        assertDoesntMatch(
            ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
            ~seq=Unord([Adj([-7, -8]), Adj([-2, -4]), Adj([-5])]),
            ~varTypes=[],
        )
    })
    // it("multiple constants; matching combination of different groups of constants", _ => {
    //     assertMatches(
    //         ~expr=[-1, -2, -3, -4 , -5, -6, -7, -8, -9],
    //         ~seq=Unord([
    //             Ord([Adj([-7,-8]), NonAdj([])]), 
    //             Ord([Unord([NonAdj([]), Adj([])])]), 
    //             NonAdj([-1,-2])
    //         ]),
    //         ~varTypes=[],
    //         ~expectedIndices=[0, 1, 2, 3, 4, 6, 7]
    //     )
    // })
})