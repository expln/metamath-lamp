module P = MM_wrk_pattern_search_v2_parser

type variable = {
    typ: int,
    mutable capVar: int, //captured variable
    mutable capVarIdx: int, //index of the first occurrence of the captured variable
}

type rec sym = {
    constOrVar: constOrVar,
    mutable matchedIdx: int, //index of the matched symbol
}
and constOrVar = Const(int) | Var(variable)

type rec symSeq = {
    elems: seqGrp,
    mutable minConstMismatchIdx:int,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

type patternTarget = Frm | Hyps | Asrt

type pattern = {
    target: patternTarget,
    symSeq: symSeq,
    allSeq: array<symSeq>,
}

let getVarType = (varTypes: array<int>, var:int):int => {
    varTypes[var]->Option.getExn(~message=`No type is defiend for var ${var->Int.toString}`)
}

let exprSymMatchesSeqConst = (exprSym:int, seqConst:int, varTypes: array<int>):bool => {
    exprSym == seqConst || exprSym >= 0 && varTypes->getVarType(exprSym) == seqConst
}

let exprIncludesConstAdjSeq = (~expr:array<int>, ~startIdx:int, ~seq:array<sym>, ~varTypes: array<int>):int => {
    let begin = ref(startIdx)
    let maxBegin = expr->Array.length - seq->Array.length
    let matched = ref(false)
    let maxSeqI = seq->Array.length - 1
    while (begin.contents <= maxBegin && !matched.contents) {
        matched := true
        let exprI = ref(begin.contents)
        let seqI = ref(0)
        while (seqI.contents <= maxSeqI && matched.contents) {
            let seqSym:sym = seq->Array.getUnsafe(seqI.contents)
            let exprSym:int = expr->Array.getUnsafe(exprI.contents)
            switch seqSym.constOrVar {
                | Const(seqConst) => matched := exprSymMatchesSeqConst(exprSym, seqConst, varTypes)
                | Var({typ:seqVarType}) => matched := exprSym >= 0 && varTypes->getVarType(exprSym) == seqVarType
            }
            exprI := exprI.contents + 1
            seqI := seqI.contents + 1
        }
        if (!matched.contents) {
            begin := begin.contents + 1
        }
    }
    if (matched.contents) {
        begin.contents + maxSeqI
    } else {
        -1
    }
}

let rec exprIncludesConstSeq = (~expr:array<int>, ~startIdx:int, ~seq:symSeq, ~varTypes: array<int>):int => {
    if (expr->Array.length <= startIdx || seq.minConstMismatchIdx <= startIdx) {
        -1
    } else {
        let res = switch seq.elems {
            | Adjacent(seq) => exprIncludesConstAdjSeq(~expr, ~startIdx, ~seq, ~varTypes)
            | Ordered(childElems) => exprIncludesConstOrderedSeq(~expr, ~startIdx, ~childElems, ~varTypes)
            | Unordered(childElems) => {
                exprIncludesConstUnorderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[])
            }
        }
        if (res < 0) {
            seq.minConstMismatchIdx = startIdx
        }
        res
    }
}

and let exprIncludesConstOrderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>
):int => {
    let lastMatchedIdx = ref(startIdx-1)
    let matched = ref(true)
    let i = ref(0)
    let maxI = childElems->Array.length - 1
    while (i.contents <= maxI && matched.contents) {
        lastMatchedIdx := exprIncludesConstSeq(
            ~expr, ~startIdx=lastMatchedIdx.contents+1, ~seq=childElems->Array.getUnsafe(i.contents), ~varTypes
        )
        matched := lastMatchedIdx.contents >= 0
        i := i.contents + 1
    }
    if (matched.contents) {
        lastMatchedIdx.contents
    } else {
        -1
    }
}

and let exprIncludesConstUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, ~passedSeqIdxs:array<int>
):int => {
    if (passedSeqIdxs->Array.length == childElems->Array.length) {
        startIdx-1
    } else {
        let res = ref(-1)
        let i = ref(0)
        let maxI = childElems->Array.length - 1
        while (res.contents < 0 && i.contents <= maxI) {
            if !(passedSeqIdxs->Array.includes(i.contents)) {
                let curSeq = childElems->Array.getUnsafe(i.contents)
                if (startIdx < curSeq.minConstMismatchIdx) {
                    let lastMatchedIdx = exprIncludesConstSeq(~expr, ~startIdx, ~seq=curSeq, ~varTypes)
                    if (lastMatchedIdx >= 0) {
                        passedSeqIdxs->Array.push(i.contents)
                        res := exprIncludesConstUnorderedSeq(
                            ~expr, ~startIdx=lastMatchedIdx+1, ~childElems, ~varTypes, ~passedSeqIdxs
                        )
                        passedSeqIdxs->Array.pop->ignore
                    }
                }
            }
            i := i.contents + 1
        }
        res.contents
    }
}

let exprIncludesVarAdjSeq = (
    ~expr:array<int>, ~startIdx:int, ~seq:array<sym>, ~varTypes: array<int>,
    ~next:int=>unit
):unit => {
    let begin = ref(startIdx)
    let maxBegin = expr->Array.length - seq->Array.length
    let matched = ref(false)
    let maxSeqI = seq->Array.length - 1
    while (begin.contents <= maxBegin && !matched.contents) {
        matched := true
        let exprI = ref(begin.contents)
        let seqI = ref(0)
        while (seqI.contents <= maxSeqI && matched.contents) {
            let seqSym:sym = seq->Array.getUnsafe(seqI.contents)
            let exprSym:int = expr->Array.getUnsafe(exprI.contents)
            switch seqSym.constOrVar {
                | Const(seqConst) => matched := exprSymMatchesSeqConst(exprSym, seqConst, varTypes)
                | Var(seqVar) => {
                    if (seqVar.capVar >= 0) {
                        matched := seqVar.capVar == exprSym
                    } else if ( exprSym >= 0 && varTypes->getVarType(exprSym) == seqVar.typ ) {
                        seqVar.capVar = exprSym
                        seqVar.capVarIdx = exprI.contents
                    } else {
                        matched := false
                    }
                }
            }
            if (matched.contents) {
                seqSym.matchedIdx = exprI.contents
            }
            exprI := exprI.contents + 1
            seqI := seqI.contents + 1
        }
        if (matched.contents) {
            next(begin.contents + maxSeqI)
        }
        seqI := Math.Int.min(seqI.contents, maxSeqI)
        while (seqI.contents >= 0) {
            let seqSym = seq->Array.getUnsafe(seqI.contents)
            seqSym.matchedIdx = -1
            switch seqSym.constOrVar {
                | Const(_) => ()
                | Var(seqVar) => {
                    let exprIdx = begin.contents + seqI.contents
                    if (exprIdx == seqVar.capVarIdx) {
                        seqVar.capVar = -1
                    }
                }
            }
            seqI := seqI.contents - 1
        }
        begin := begin.contents + 1
    }
}

let rec exprIncludesVarSeq = (
    ~expr:array<int>, ~startIdx:int, ~seq:symSeq, ~varTypes:array<int>,
    ~next:int=>unit, ~stop:ref<bool>
):unit => {
    if (startIdx < expr->Array.length && startIdx < seq.minConstMismatchIdx) {
        switch seq.elems {
            | Adjacent(seq) => exprIncludesVarAdjSeq(~expr, ~startIdx, ~seq, ~varTypes, ~next)
            | Ordered(childElems) => {
                exprIncludesVarOrderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~next, ~childElemIdx=0, ~stop)
            }
            | Unordered(childElems) => {
                exprIncludesVarUnorderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[], ~next, ~stop)
            }
        }
    }
}

and let exprIncludesVarOrderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>,
    ~next:int=>unit, ~childElemIdx:int, ~stop:ref<bool>
):unit => {
    if (childElems->Array.length <= childElemIdx) {
        next(startIdx-1)
    } else {
        exprIncludesVarSeq(
            ~expr, ~startIdx, ~seq=childElems->Array.getUnsafe(childElemIdx), ~varTypes,
            ~next = lastMatchedIdx => exprIncludesVarOrderedSeq(
                ~expr, ~startIdx=lastMatchedIdx+1, ~childElems, ~varTypes,
                ~next, ~childElemIdx=childElemIdx+1, ~stop
            ), 
            ~stop
        )
    }
}

and let exprIncludesVarUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, ~passedSeqIdxs:array<int>,
    ~next:int=>unit, ~stop:ref<bool>
):unit => {
    if (passedSeqIdxs->Array.length == childElems->Array.length) {
        next(startIdx-1)
    } else {
        let i = ref(0)
        let maxI = childElems->Array.length - 1
        while (!stop.contents && i.contents <= maxI) {
            if (!(passedSeqIdxs->Array.includes(i.contents))) {
                let curSeq = childElems->Array.getUnsafe(i.contents)
                if (startIdx < curSeq.minConstMismatchIdx) {
                    exprIncludesVarSeq(
                        ~expr, ~startIdx, ~seq=curSeq, ~varTypes,
                        ~next = lastMatchedIdx => {
                            passedSeqIdxs->Array.push(i.contents)
                            exprIncludesVarUnorderedSeq(
                                ~expr, ~startIdx=lastMatchedIdx+1, ~childElems, ~varTypes, ~passedSeqIdxs, ~next, ~stop
                            )
                            passedSeqIdxs->Array.pop->ignore
                        },
                        ~stop
                    )
                }
            }
            i := i.contents + 1
        }
    }
}

let getMatchedIndices = (seq:symSeq):array<int> => {
    let indices = []
    let rec go = (seq:symSeq):unit => {
        switch seq.elems {
            | Adjacent(syms) => syms->Array.forEach(sym => indices->Array.push(sym.matchedIdx))
            | Ordered(childElems) | Unordered(childElems) => childElems->Array.forEach(go)
        }
    }
    go(seq)
    indices->Array.sort(Int.compare)
    indices
}

let exprIncludesSeq = (
    ~expr:array<int>, ~seq:symSeq, ~varTypes:array<int>
):option<array<int>> => {
    let res = ref(None)
    if (exprIncludesConstSeq(~expr, ~startIdx=0, ~seq, ~varTypes) >= 0) {
        let stop = ref(false)
        exprIncludesVarSeq(
            ~expr, ~startIdx=0, ~seq, ~varTypes, 
            ~next = lastMatchedIdx => {
                if (lastMatchedIdx >= 0) {
                    stop := true
                    switch res.contents {
                        | None => res := Some(getMatchedIndices(seq))
                        | Some(_) => Exn.raiseError("next() is called twice in exprIncludesSeq.")
                    }
                }
            },
            ~stop
        )
    }
    res.contents
}

let isAdj = (flags:P.flags):bool => {
    flags.adj->Option.getOr(false)
}

let makeSym = (symStr:string, symMap:Belt_HashMapString.t<constOrVar>):sym => {
    {
        matchedIdx: -1,
        constOrVar: 
            symMap->Belt_HashMapString.get(symStr)->Option.getExn(~message=`makeSym: unknown symbol ${symStr}`)
    }
}

let rec astToSymSeq = (ast:P.symSeq, flags:P.flags, symMap:Belt_HashMapString.t<constOrVar>):symSeq => {
    {
        elems: astToSeqGrp(ast.elems, P.passFlagsFromParentToChild(flags, ast.flags), symMap),
        minConstMismatchIdx: -1
    }
}
and astToSeqGrp = (ast:P.seqGrp, flags:P.flags, symMap:Belt_HashMapString.t<constOrVar>):seqGrp => {
    switch ast {
        | Symbols(syms) => {
            if (isAdj(flags)) {
                Adjacent(syms->Array.map(makeSym(_,symMap)))
            } else {
                Ordered(syms->Array.map(symStr => {
                    {
                        elems:Adjacent([makeSym(symStr,symMap)]),
                        minConstMismatchIdx:-1
                    }
                }))
            }
        }
        | Ordered(syms) => Ordered(syms->Array.map(astToSymSeq(_, flags, symMap)))
        | Unordered(syms) => Unordered(syms->Array.map(astToSymSeq(_, flags, symMap)))
    }
}

let rec collectAllSeq = (seq:symSeq, allSeq:array<symSeq>):unit => {
    allSeq->Array.push(seq)
    switch seq.elems {
        | Adjacent(_) => ()
        | Ordered(childSeq) | Unordered(childSeq) => childSeq->Array.forEach(collectAllSeq(_, allSeq))
        
    }
}

let astToPattern = (ast:P.pattern, symMap:Belt_HashMapString.t<constOrVar>):pattern => {
    let res = {
        target: switch ast.target {|Frm => Frm |Hyps => Hyps |Asrt => Asrt},
        symSeq: astToSymSeq(ast.symSeq, {adj:None}, symMap),
        allSeq: []
    }
    collectAllSeq(res.symSeq, res.allSeq)
    res
}

let parsePattern = (text:string, symMap:Belt_HashMapString.t<constOrVar>):result<array<pattern>,string> => {
    switch P.parsePattern(text) {
        | None => Error(`Cannot parse the pattern '${text}'`)
        | Some(ast) => Ok(ast->Array.map(astToPattern(_, symMap)))
    }
}