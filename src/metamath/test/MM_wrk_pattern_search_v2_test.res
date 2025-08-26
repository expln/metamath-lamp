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
    ~title:string, ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>, ~expectedIndices:array<int>
):unit => {
    let seq = makeSymSeq(seq, varTypes, expr->Array.length)
    assertEqMsg(exprIncludesSeq( ~expr, ~seq, ~varTypes ), true, `the pattern didn't match in '${title}'`)
    assertEqMsg(getMatchedIndices(seq), expectedIndices, `indices didn't match in '${title}'`)
}

let assertDoesntMatch = (
    ~title:string, ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>
):unit => {
    let seq = makeSymSeq(seq, varTypes, expr->Array.length)
    assertEqMsg(exprIncludesSeq( ~expr, ~seq, ~varTypes ), false, `the pattern matched in '${title}'`)
}

describe("exprIncludesSeq", _ => {
    it("correctly records matched indices", _ => {
        assertMatches(
            ~title="single constant; matching single constant",
            ~expr=[-1],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )

        assertDoesntMatch(
            ~title="single constant; non-matching single constant",
            ~expr=[-1],
            ~seq=Adj([-2]),
            ~varTypes=[],
        )

        assertMatches(
            ~title="two constants; matching single constant on the left",
            ~expr=[-1, -2],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )

        assertMatches(
            ~title="two constants; matching single constant on the right",
            ~expr=[-1, -2],
            ~seq=Adj([-2]),
            ~varTypes=[],
            ~expectedIndices=[1]
        )

        assertDoesntMatch(
            ~title="two constants; non-matching single constant",
            ~expr=[-1, -2],
            ~seq=Adj([-3]),
            ~varTypes=[],
        )

        assertMatches(
            ~title="multiple constants; matching adjacent constants on the left",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-1, -2, -3]),
            ~varTypes=[],
            ~expectedIndices=[0, 1, 2]
        )

        assertMatches(
            ~title="multiple constants; matching adjacent constants in the middle",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-3, -4, -5]),
            ~varTypes=[],
            ~expectedIndices=[2, 3, 4]
        )

        assertMatches(
            ~title="multiple constants; matching adjacent constants on the right",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-5, -6, -7]),
            ~varTypes=[],
            ~expectedIndices=[4, 5, 6]
        )

        assertDoesntMatch(
            ~title="multiple constants; non-matching adjacent constants",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=Adj([-6, -5, -7]),
            ~varTypes=[],
        )

        assertMatches(
            ~title="multiple constants; matching non-adjacent constants on the left",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-1, -3, -6]),
            ~varTypes=[],
            ~expectedIndices=[0, 2, 5]
        )

        assertMatches(
            ~title="multiple constants; matching non-adjacent constants in the middle",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -4, -6]),
            ~varTypes=[],
            ~expectedIndices=[1, 3, 5]
        )

        assertMatches(
            ~title="multiple constants; matching non-adjacent constants on the right",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -5, -7]),
            ~varTypes=[],
            ~expectedIndices=[1, 4, 6]
        )

        assertDoesntMatch(
            ~title="multiple constants; non-matching non-adjacent constants",
            ~expr=[-1, -2, -3, -4 , -5, -6, -7],
            ~seq=NonAdj([-2, -6, -4]),
            ~varTypes=[],
        )
    })
})