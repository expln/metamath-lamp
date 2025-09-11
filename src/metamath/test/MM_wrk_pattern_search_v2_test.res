open Expln_test
open MM_wrk_pattern_search_v2

type rec testSeqGrp = 
    | Adj(array<int>)
    | NonAdj(array<int>)
    | Ord(array<testSeqGrp>)
    | Unord(array<testSeqGrp>)

let makeArrayOfSymbols = (seq:array<int>, symMap:Belt_HashMapInt.t<constOrVar>): array<sym> => {
    seq->Array.map(i => {
        {
            constOrVar: symMap->Belt_HashMapInt.get(i)
                ->Option.getExn(~message=`makeSeq: unexpected sym ${i->Int.toString}`),
            matchedIdx: -1
        }
    })
}

let rec collectAllSyms = (seq:testSeqGrp, allSyms:Belt_HashSetInt.t):unit => {
    switch seq {
        | Adj(ints) | NonAdj(ints) => ints->Array.forEach(Belt_HashSetInt.add(allSyms, _))
        | Ord(ch) | Unord(ch) => ch->Array.forEach(collectAllSyms(_, allSyms))
    }
}

let rec makeSymSeq = (
    seq:testSeqGrp, 
    minConstMismatchIdx:int, 
    symMap:Belt_HashMapInt.t<constOrVar>
): symSeq => {
    let (elems, minLen) = switch seq {
        | Adj(seq) => (Adjacent(makeArrayOfSymbols(seq, symMap)), seq->Array.length)
        | NonAdj(seq) => {
            let seqGrp = Ordered(seq->Array.map(i => {
                {
                    elems: Adjacent(makeArrayOfSymbols([i], symMap)),
                    minLen: 1,
                    minConstMismatchIdx,
                }
            }))
            (seqGrp, seq->Array.length)
        }
        | Ord(childElems) => {
            let childSeq:array<symSeq> = childElems->Array.map(ch=>makeSymSeq(ch, minConstMismatchIdx, symMap))
            (Ordered(childSeq), countMinLen(childSeq))
        }
        | Unord(childElems) => {
            let childSeq:array<symSeq> = childElems->Array.map(ch=>makeSymSeq(ch, minConstMismatchIdx, symMap))
            (Unordered(childSeq), countMinLen(childSeq))
        }
    }
    { elems, minLen, minConstMismatchIdx, }
}

let makeSymMap = (~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>):Belt_HashMapInt.t<constOrVar> => {
    let allSym = Belt_HashSetInt.make(~hintSize=expr->Array.length)
    collectAllSyms(seq, allSym)
    Belt_HashMapInt.fromArray(
        allSym->Belt_HashSetInt.toArray->Array.map(i => {
            if (i < 0) {
                (i, Const(i))
            } else {
                let var = Var({
                    typ: varTypes[i]->Option.getExn(~message=`No type is defined for var ${i->Int.toString}`),
                    capVar: -1,
                    capVarIdx: -1,
                })
                (i, var)
            }
        })
    )
}

let assertMatches = (
    ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>, ~expectedIndices:array<int>
):unit => {
    let seq = makeSymSeq(seq, expr->Array.length, makeSymMap(~expr, ~seq, ~varTypes))
    assertEq(exprIncludesSeq( ~expr, ~seq, ~varTypes ), Some(expectedIndices))
}

let assertDoesntMatch = (
    ~expr:array<int>, ~seq:testSeqGrp, ~varTypes:array<int>
):unit => {
    let seq = makeSymSeq(seq, expr->Array.length, makeSymMap(~expr, ~seq, ~varTypes))
    assertEq(exprIncludesSeq( ~expr, ~seq, ~varTypes ), None)
}

let adj = (syms:array<sym>):symSeq => { elems: Adjacent(syms), minLen:syms->Array.length, minConstMismatchIdx: -1 }
let ord = (seq:array<symSeq>):symSeq => { elems: Ordered(seq), minLen:countMinLen(seq), minConstMismatchIdx: -1 }
let unord = (seq:array<symSeq>):symSeq => { elems: Unordered(seq), minLen:countMinLen(seq), minConstMismatchIdx: -1 }
let pat = (target:patternTarget, symSeq:symSeq):pattern => { target, symSeq, allSeq:[] }

let assertParsePattern = (
    ~pattern:string, ~syms:Belt_HashMapString.t<constOrVar>, ~expectedResult:result<array<pattern>,string>
):unit => {
    assertEqMsg(
        parsePattern(pattern, ~symMap=syms)->Result.map(ps => ps->Array.map(p => {...p, allSeq:[]})), 
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
    it("var in expr matches type in pattern", _ => {
        assertMatches(
            ~expr=[-1, -2, 0, -4, -5],
            ~seq=Adj([-2, -3, -4]),
            ~varTypes=[-3],
            ~expectedIndices=[1,2,3]
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
    it("single var; matching single var represented by another integer", _ => {
        assertMatches(
            ~expr=[0],
            ~seq=Adj([1]),
            ~varTypes=[-1,-1],
            ~expectedIndices=[0]
        )
    })
    it("single var; single var with another type", _ => {
        assertDoesntMatch(
            ~expr=[0],
            ~seq=Adj([1]),
            ~varTypes=[-1,-2]
        )
    })
    it("two adj vars; two matching vars", _ => {
        assertMatches(
            ~expr=[0,1],
            ~seq=Adj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1]
        )
    })
    it("two adj vars; two matching vars on the left", _ => {
        assertMatches(
            ~expr=[0,1,-10,-11],
            ~seq=Adj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1]
        )
    })
    it("two adj vars; two matching vars in the middle", _ => {
        assertMatches(
            ~expr=[-10,-11,0,1,-10,-11],
            ~seq=Adj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,3]
        )
    })
    it("two adj vars; two matching vars on the right", _ => {
        assertMatches(
            ~expr=[-10,-11,0,1],
            ~seq=Adj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,3]
        )
    })
    it("two non-adj vars; two matching vars", _ => {
        assertMatches(
            ~expr=[0,-10,-11,1],
            ~seq=NonAdj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,3]
        )
    })
    it("two non-adj vars; two matching vars on the left", _ => {
        assertMatches(
            ~expr=[0,-10,-11,1,-12,-13],
            ~seq=NonAdj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,3]
        )
    })
    it("two non-adj vars; two matching vars in the middle", _ => {
        assertMatches(
            ~expr=[-10,-11,0,-12,-13,1,-14,-15],
            ~seq=NonAdj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,5]
        )
    })
    it("two non-adj vars; two matching vars on the right", _ => {
        assertMatches(
            ~expr=[-10,-11,0,-12,-13,1],
            ~seq=NonAdj([2,3]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,5]
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
            ~seq=Adj([1,-1,1]),
            ~varTypes=[-2,-2],
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

    it("two ordered groups of adj vars and consts", _ => {
        assertMatches(
            ~expr=[0,-10,1,-11,-12,1,-11,0],
            ~seq=Ord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1,2,5,6,7]
        )
    })
    it("two ordered groups of adj vars and consts on the left", _ => {
        assertMatches(
            ~expr=[0,-10,1,-11,-12,1,-11,0,-15,-16,0,-10,1,-11,-12,1,-11,0],
            ~seq=Ord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1,2,5,6,7]
        )
    })
    it("two ordered groups of adj vars and consts in the middle", _ => {
        assertMatches(
            ~expr=[-15,4,0,-10,1,-11,-12,1,-11,0,-17,-18],
            ~seq=Ord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2,-3],
            ~expectedIndices=[2,3,4,7,8,9]
        )
    })
    it("two ordered groups of adj vars and consts on the right", _ => {
        assertMatches(
            ~expr=[-15,4,0,-10,1,-11,-12,1,-11,0],
            ~seq=Ord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2,-3],
            ~expectedIndices=[2,3,4,7,8,9]
        )
    })
    
    it("two ordered groups of non-adj vars and consts", _ => {
        assertMatches(
            ~expr=[0,-17,-10,1,-11,-12,1,-18,-19,-11,-20,0],
            ~seq=Ord([NonAdj([2,-10,3]),NonAdj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,2,3,6,9,11]
        )
    })
    it("two ordered groups of non-adj vars and consts on the left", _ => {
        assertMatches(
            ~expr=[0,-17,-10,1,-11,-12,1,-18,-19,-11,-20,0,-21,0,-17,-10,1,-11,-12,1,-18,-19,-11,-20,0],
            ~seq=Ord([NonAdj([2,-10,3]),NonAdj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,2,3,6,9,11]
        )
    })
    it("two ordered groups of non-adj vars and consts in the middle", _ => {
        assertMatches(
            ~expr=[-21,-22,0,-17,-10,1,-11,-12,1,-18,-19,-11,-20,0,-23,-24],
            ~seq=Ord([NonAdj([2,-10,3]),NonAdj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,4,5,8,11,13]
        )
    })
    it("two ordered groups of non-adj vars and consts on the right", _ => {
        assertMatches(
            ~expr=[-21,-22,0,-17,-10,1,-11,-12,1,-18,-19,-11,-20,0],
            ~seq=Ord([NonAdj([2,-10,3]),NonAdj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[2,4,5,8,11,13]
        )
    })

    it("two unordered groups of adj vars and consts", _ => {
        assertMatches(
            ~expr=[1,-11,0,-11,-12,0,-10,1],
            ~seq=Unord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1,2,5,6,7]
        )
    })
    it("two unordered groups of adj vars and consts on the left", _ => {
        assertMatches(
            ~expr=[1,-11,0,-11,-12,0,-10,1,-15,-16,0,-10,1,-11,-12,1,-20,0],
            ~seq=Unord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2],
            ~expectedIndices=[0,1,2,5,6,7]
        )
    })
    it("two unordered groups of adj vars and consts in the middle", _ => {
        assertMatches(
            ~expr=[-15,4,1,-11,0,-11,-12,0,-10,1,-17,-18],
            ~seq=Unord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2,-3],
            ~expectedIndices=[2,3,4,7,8,9]
        )
    })
    it("two unordered groups of adj vars and consts on the right", _ => {
        assertMatches(
            ~expr=[-15,4,1,-11,0,-11,-12,0,-10,1],
            ~seq=Unord([Adj([2,-10,3]),Adj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2,-3],
            ~expectedIndices=[2,3,4,7,8,9]
        )
    })

    it("two unordered groups of non-adj vars and consts in the middle", _ => {
        assertMatches(
            ~expr=[-15,4,0,5,-10,4,1,-11,-12,1,5,-11,5,0,-17,-18],
            ~seq=Unord([NonAdj([2,-10,3]),NonAdj([3,-11,2])]),
            ~varTypes=[-1,-2,-1,-2,-3,-4],
            ~expectedIndices=[2,4,6,9,11,13]
        )
    })

    it("stops as soon as a match is found", _ => {
        assertMatches(
            ~expr=[-1,-1,-1],
            ~seq=Unord([Adj([-1]),Adj([-1])]),
            ~varTypes=[],
            ~expectedIndices=[0,1]
        )
        assertMatches(
            ~expr=[-1,-1,-1],
            ~seq=Ord([Adj([-1]),Adj([-1])]),
            ~varTypes=[],
            ~expectedIndices=[0,1]
        )
        assertMatches(
            ~expr=[-1,-1,-1],
            ~seq=Adj([-1]),
            ~varTypes=[],
            ~expectedIndices=[0]
        )

        assertMatches(
            ~expr=[0,2,1,-1,0,1,2,-1,2,1,0],
            ~seq=Ord([Adj([0,1,2]),Adj([2,1,0])]),
            ~varTypes=[-9,-9,-9],
            ~expectedIndices=[4,5,6,8,9,10]
        )
        assertMatches(
            ~expr=[0,2,1,0,1,2,2,1,0],
            ~seq=Ord([Adj([0,1,2]),Adj([2,1,0])]),
            ~varTypes=[-9,-9,-9],
            ~expectedIndices=[3,4,5,6,7,8]
        )
        
        assertMatches(
            ~expr=[0,2,1,-1,0,1,2,-1,2,1,0],
            ~seq=Unord([Adj([0,1,2]),Adj([2,1,0])]),
            ~varTypes=[-9,-9,-9],
            ~expectedIndices=[4,5,6,8,9,10]
        )
        assertMatches(
            ~expr=[0,2,1,0,1,2,2,1,0],
            ~seq=Unord([Adj([0,1,2]),Adj([2,1,0])]),
            ~varTypes=[-9,-9,-9],
            ~expectedIndices=[3,4,5,6,7,8]
        )
    })
})

describe("parsePattern", _ => {
    let a = {constOrVar:Const(-1), matchedIdx:-1}
    let b = {constOrVar:Const(-2), matchedIdx:-1}
    let c = {constOrVar:Const(-3), matchedIdx:-1}
    let d = {constOrVar:Const(-4), matchedIdx:-1}
    let syms = Belt_HashMapString.fromArray([ 
        ("a", a.constOrVar), 
        ("b", b.constOrVar), 
        ("c", c.constOrVar), 
        ("d", d.constOrVar), 
    ])

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

describe("convertMatchedIndices", () => {
    let makeFrame = (hyps:array<array<int>>, asrt:array<int>):MM_context.frame => {
        {
            ord:-1, isAxiom:false, disj: Belt_MapInt.empty,
            hyps: hyps->Array.map((expr):MM_context.hypothesis => {typ:E, label:"", expr}),
            asrt: asrt,
            label: "", frameVarToSymb: [], varTypes: [], varHyps: [], numOfVars: -1, numOfArgs: -1, descr:None, 
            descrNorm:None, proof:None, isDisc:false, isDepr:false, isTranDepr:false, dbg:None, usageCnt:-1,
        }
    }
    it("converts indices for Frm target", () => {
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [0,1,2], Frm), [[0],[0],[0]], "case 1" )
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [0], Frm), [[0],[],[]], "case 2" )
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [1], Frm), [[],[0],[]], "case 3" )
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [2], Frm), [[],[],[0]], "case 4" )
        assertEqMsg( convertMatchedIndices(makeFrame([], [0]), [0], Frm), [[0]], "case 5" )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [0,1,2,6,7,8,12,13,14,15,19], 
                Frm
            ),
            [[0,1,2],[1,2,3],[2,3,4],[0,4]],
            "case 6"
        )
    })
    it("converts indices for Hyps target", () => {
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [0,1], Hyps), [[0],[0],[]], "case 1" )
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [0], Hyps), [[0],[],[]], "case 2" )
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [1], Hyps), [[],[0],[]], "case 3" )
        assertEqMsg( convertMatchedIndices(makeFrame([], [0]), [], Hyps), [[]], "case 5" )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [0,1,2,6,7,8,12,13,14], 
                Hyps
            ),
            [[0,1,2],[1,2,3],[2,3,4],[]],
            "case 6"
        )
    })
    it("converts indices for Asrt target", () => {
        assertEqMsg( convertMatchedIndices(makeFrame([[0],[1]], [2]), [2], Asrt), [[],[],[0]], "case 4" )
        assertEqMsg( convertMatchedIndices(makeFrame([], [0]), [0], Asrt), [[0]], "case 5" )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [15,19], 
                Asrt
            ),
            [[],[],[],[0,4]],
            "case 6"
        )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [15,16,17], 
                Asrt
            ),
            [[],[],[],[0,1,2]],
            "case 7"
        )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [16,17,18], 
                Asrt
            ),
            [[],[],[],[1,2,3]],
            "case 8"
        )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [17,18,19], 
                Asrt
            ),
            [[],[],[],[2,3,4]],
            "case 9"
        )
        assertEqMsg(
            convertMatchedIndices(
                makeFrame([[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14]], [15,16,17,18,19]), 
                [16,18], 
                Asrt
            ),
            [[],[],[],[1,3]],
            "case 10"
        )
    })
})

describe("mergeMatchedIndices", () => {
    it("merges indices", () => {
        assertEqMsg( 
            mergeMatchedIndices([
                [[1,2,3],[4,5,6]],
                [[7,8,9],[10,11,12]],
            ]), 
            [[1,2,3,7,8,9],[4,5,6,10,11,12]],
            "case 1" 
        )
        assertEqMsg( 
            mergeMatchedIndices([
                [[],[4,5,6]],
                [[7,8,9],[]],
            ]), 
            [[7,8,9],[4,5,6]],
            "case 2" 
        )
    })
})