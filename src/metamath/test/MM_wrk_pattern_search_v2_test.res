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

let adj = (syms:array<sym>):symSeq => { elems: Adjacent(syms), minConstMismatchIdx: -1 }
let ord = (seq:array<symSeq>):symSeq => { elems: Ordered(seq), minConstMismatchIdx: -1 }
let unord = (seq:array<symSeq>):symSeq => { elems: Unordered(seq), minConstMismatchIdx: -1 }
let pat = (target:patternTarget, symSeq:symSeq):pattern => { target, symSeq, allSeq:[] }

let assertParsePattern = (
    ~pattern:string, ~syms:Belt_HashMapString.t<sym>, ~expectedResult:result<array<pattern>,string>
):unit => {
    assertEqMsg(
        parsePattern(pattern, syms)->Result.map(ps => ps->Array.map(p => {...p, allSeq:[]})), 
        expectedResult, 
        pattern
    )
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
    it("multiple constants; matching combination of different groups of constants", _ => {
        assertMatches(
            ~expr=[-100,-101,-9,-10,-11,-6,-102,-7,-103,-104,-8,-105,-106,-12,-13,-107,
                -14,-15,-1,-2,-108,-109,-3,-4,-110,-111,-5],
            ~seq=Unord([
                Ord([
                    Adj([-1,-2]), 
                    NonAdj([-3,-4,-5])
                ]), 
                Ord([
                    Unord([
                        NonAdj([-6,-7,-8]), 
                        Adj([-9,-10,-11])
                    ]),
                    Adj([-12,-13]),
                ]), 
                NonAdj([-14,-15])
            ]),
            ~varTypes=[],
            ~expectedIndices=[2,3,4,5,7,10,13,14,16,17,18,19,22,23,26]
        )
    })

    it("single var; matching single var", _ => {
        assertMatches(
            ~expr=[0],
            ~seq=Adj([0]),
            ~varTypes=[-1],
            ~expectedIndices=[0]
        )
    })
    it("single var; matching single var (another integer)", _ => {
        assertMatches(
            ~expr=[0],
            ~seq=Adj([1]),
            ~varTypes=[-1,-1],
            ~expectedIndices=[0]
        )
    })
    it("single var; non-matching single var", _ => {
        assertDoesntMatch(
            ~expr=[0],
            ~seq=Adj([1]),
            ~varTypes=[-1,-2]
        )
    })
    it("two vars; two matching vars", _ => {
        assertMatches(
            ~expr=[0,1],
            ~seq=Adj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1]
        )
    })
    it("same var in expr is assigned to different vars in pattern", _ => {
        assertMatches(
            ~expr=[0,-1,0],
            ~seq=Adj([1,-1,2]),
            ~varTypes=[-2,-2,-2],
            ~expectedIndices=[0,1,2]
        )
    })
    it("same var in pattern is assigned to same vars in expr", _ => {
        assertMatches(
            ~expr=[0,-1,0],
            ~seq=Adj([2,-1,2]),
            ~varTypes=[-2,-2,-2],
            ~expectedIndices=[0,1,2]
        )
    })
    it("same var in pattern doesn't match different vars in expr", _ => {
        assertDoesntMatch(
            ~expr=[0,-1,1],
            ~seq=Adj([2,-1,2]),
            ~varTypes=[-2,-2,-2],
        )
    })
})

describe("parsePattern", _ => {
    let a = {constOrVar:Const(-1), matchedIdx:-1}
    let b = {constOrVar:Const(-2), matchedIdx:-1}
    let c = {constOrVar:Const(-3), matchedIdx:-1}
    let d = {constOrVar:Const(-4), matchedIdx:-1}
    let syms = Belt_HashMapString.fromArray([ ("a", a), ("b", b), ("c", c), ("d", d), ])

    it("passes flags from parent to child", _ => {
        assertParsePattern(~pattern="a b $** c d", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                ord([
                    ord([adj([a]), adj([b])]),
                    ord([adj([c]), adj([d])]),
                ])
            )])
        )
        assertParsePattern(~pattern="a b $|| c d", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    ord([adj([a]), adj([b])]),
                    ord([adj([c]), adj([d])]),
                ])
            )])
        )
        assertParsePattern(~pattern="$+ a b $|| c d", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    adj([a, b]),
                    adj([c, d]),
                ])
            )])
        )
        assertParsePattern(~pattern="$+ $[- a b $] $|| c d", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    ord([adj([a]), adj([b])]),
                    adj([c, d]),
                ])
            )])
        )
        assertParsePattern(~pattern="$+ $[- a b $] $|| $[+ c d $]", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    ord([adj([a]), adj([b])]),
                    adj([c, d]),
                ])
            )])
        )
        assertParsePattern(~pattern="$[- a b $] $|| $[+ c d $]", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    ord([adj([a]), adj([b])]),
                    adj([c, d]),
                ])
            )])
        )
        assertParsePattern(~pattern="a b $|| $[+ c d $** $[- a b $] $]", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                unord([
                    ord([adj([a]), adj([b])]),
                    ord([
                        adj([c, d]), 
                        ord([adj([a]), adj([b])]),
                    ]),
                ])
            )])
        )
        assertParsePattern(~pattern="$+ a b $** $[- c d $|| $[+ a b $] $]", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                ord([
                    adj([a, b]), 
                    unord([
                        ord([adj([c]), adj([d])]),
                        adj([a, b]),
                    ]),
                ])
            )])
        )
    })
    it("sets pattern targets", _ => {
        assertParsePattern(~pattern="a b", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                ord([adj([a]), adj([b])])
            )])
        )
        assertParsePattern(~pattern="$ a b", ~syms, 
            ~expectedResult=Ok([pat(
                Frm,
                ord([adj([a]), adj([b])])
            )])
        )
        assertParsePattern(~pattern="$h a b", ~syms, 
            ~expectedResult=Ok([pat(
                Hyps,
                ord([adj([a]), adj([b])])
            )])
        )
        assertParsePattern(~pattern="$a a b", ~syms, 
            ~expectedResult=Ok([pat(
                Asrt,
                ord([adj([a]), adj([b])])
            )])
        )
        assertParsePattern(~pattern="$h+ a b", ~syms, 
            ~expectedResult=Ok([pat(
                Hyps,
                adj([a,b])
            )])
        )
        assertParsePattern(~pattern="$a+ a b", ~syms, 
            ~expectedResult=Ok([pat(
                Asrt,
                adj([a,b])
            )])
        )
    })
    it("can parse multiple patterns", _ => {
        assertParsePattern(~pattern="$ a b $h c d $a a b $+ c d $h+ a b $a+ c d", ~syms, 
            ~expectedResult=Ok([
                pat(Frm, ord([adj([a]), adj([b])])),
                pat(Hyps, ord([adj([c]), adj([d])])),
                pat(Asrt, ord([adj([a]), adj([b])])),
                pat(Frm, adj([c,d])),
                pat(Hyps, adj([a,b])),
                pat(Asrt, adj([c,d])),
            ])
        )
    })
})