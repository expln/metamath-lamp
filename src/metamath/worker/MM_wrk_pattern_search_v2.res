type patTarget = Frm | Hyps | Asrt

type variable = {
    mutable capVar: int, //captured variable
    mutable capVarIdx: int, //index of the first occurrence of the captured variable
}

type sym = {
    isConst: bool,
    constOrType: int, // if isConst then constOrType == symbol else constOrType == type(variable)
    var:variable,
    mutable matchedIdx: int, //index of the matched symbol
}

type rec symSeq = {
    elems: seqGrp,
    mutable minConstMismatchIdx:int,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

type stmtPat = {
    target: patTarget,
    symSeq: symSeq,
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
            let seqSym = seq->Array.getUnsafe(seqI.contents)
            let exprSym = expr->Array.getUnsafe(exprI.contents)
            if (seqSym.isConst) {
                matched := seqSym.constOrType == exprSym
            } else {
                matched := exprSym >= 0 
                    && varTypes[exprSym]->Option.mapOr(false, exprVarType => seqSym.constOrType == exprVarType)
            }
            exprI := exprI.contents + 1
            seqI := seqI.contents + 1
        }
        begin := begin.contents + 1
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
    let i = ref(0)
    let maxI = childElems->Array.length - 1
    while (i.contents <= maxI && lastMatchedIdx.contents >= 0) {
        lastMatchedIdx := exprIncludesConstSeq(
            ~expr, ~startIdx=lastMatchedIdx.contents+1, ~seq=childElems->Array.getUnsafe(i.contents), ~varTypes
        )
        i := i.contents + 1
    }
    lastMatchedIdx.contents
}

and let exprIncludesConstUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, ~passedSeqIdxs:array<int>
):int => {
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
            let seqSym = seq->Array.getUnsafe(seqI.contents)
            let exprSym = expr->Array.getUnsafe(exprI.contents)
            if (seqSym.isConst) {
                matched := seqSym.constOrType == exprSym
            } else if (seqSym.var.capVar >= 0) {
                matched := seqSym.var.capVar == exprSym
            } else if (
                exprSym >= 0
                && varTypes[exprSym]->Option.mapOr(false, exprVarType => seqSym.constOrType == exprVarType)
            ) {
                seqSym.var.capVar = exprSym
                seqSym.var.capVarIdx = exprI.contents
            } else {
                matched := false
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
            let exprIdx = begin.contents + seqI.contents
            if (exprIdx == seqSym.var.capVarIdx) {
                seqSym.var.capVar = -1
            }
            seqI := seqI.contents - 1
        }
        begin := begin.contents + 1
    }
}

let rec exprIncludesVarSeq = (
    ~expr:array<int>, ~startIdx:int, ~seq:symSeq, ~varTypes:array<int>,
    ~next:int=>unit
):unit => {
    if (startIdx < expr->Array.length) {
        switch seq.elems {
            | Adjacent(seq) => exprIncludesVarAdjSeq(~expr, ~startIdx, ~seq, ~varTypes, ~next)
            | Ordered(childElems) => {
                exprIncludesVarOrderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~next, ~childElemIdx=0)
            }
            | Unordered(childElems) => {
                exprIncludesVarUnorderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[], ~next)
            }
        }
    }
}

and let exprIncludesVarOrderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>,
    ~next:int=>unit, ~childElemIdx:int
):unit => {
    if (childElems->Array.length <= childElemIdx) {
        next(startIdx-1)
    } else {
        exprIncludesVarSeq(
            ~expr, ~startIdx, ~seq=childElems->Array.getUnsafe(childElemIdx), ~varTypes,
            ~next = lastMatchedIdx => exprIncludesVarOrderedSeq(
                ~expr, ~startIdx=lastMatchedIdx+1, ~childElems, ~varTypes,
                ~next, ~childElemIdx=childElemIdx+1
            )
        )
    }
}

and let exprIncludesVarUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, ~passedSeqIdxs:array<int>,
    ~next:int=>unit
):unit => {
    if (passedSeqIdxs->Array.length == childElems->Array.length) {
        next(startIdx-1)
    } else {
        let res = ref(-1)
        let i = ref(0)
        let maxI = childElems->Array.length - 1
        while (res.contents < 0 && i.contents <= maxI) {
            if (!(passedSeqIdxs->Array.includes(i.contents))) {
                let curSeq = childElems->Array.getUnsafe(i.contents)
                exprIncludesVarSeq(
                    ~expr, ~startIdx, ~seq=curSeq, ~varTypes,
                    ~next = lastMatchedIdx => {
                        passedSeqIdxs->Array.push(i.contents)
                        exprIncludesVarUnorderedSeq(
                            ~expr, ~startIdx=lastMatchedIdx+1, ~childElems, ~varTypes, ~passedSeqIdxs, ~next
                        )
                        passedSeqIdxs->Array.pop->ignore
                    }
                )
            }
            i := i.contents + 1
        }
    }
}

let exprIncludesSeq = (
    ~expr:array<int>, ~seq:symSeq, ~varTypes:array<int>
):bool => {
    let res = ref(false)
    if (exprIncludesConstSeq(~expr, ~startIdx=0, ~seq, ~varTypes) >= 0) {
        exprIncludesVarSeq(~expr, ~startIdx=0, ~seq, ~varTypes, ~next = lastMatchedIdx => res := lastMatchedIdx >= 0)
    }
    res.contents
}