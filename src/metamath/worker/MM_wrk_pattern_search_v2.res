type patTarget = Frm | Hyps | Asrt

type sym = {
    isConst: bool,
    const: int, // if isConst then const == symbol else const == type(variable)
    var: int,
    mutable capVar: int, //captured variable
    mutable capVarIdx: int, //index of the first occurance of the captured variable
}

type either<'a,'b> = 
    | Left('a)
    | Right('b)

type rec symSeq = {
    ordered: bool,
    elems: either<array<sym>,array<symSeq>>,
    mutable minConstMismatchIdx:int,
}

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
            let expectedConst = (seq->Array.getUnsafe(seqI.contents)).const
            let exprSym = expr->Array.getUnsafe(exprI.contents)
            if (exprSym < 0) {
                matched := expectedConst == exprSym
            } else {
                matched := expectedConst == varTypes->Array.getUnsafe(exprSym)
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
            | Left(adjSyms) => exprIncludesConstAdjSeq(~expr, ~startIdx, ~seq=adjSyms, ~varTypes)
            | Right(childElems) => {
                if seq.ordered {
                    exprIncludesConstOrderedSeq(~expr, ~startIdx, ~childElems, ~varTypes)
                } else {
                    exprIncludesConstUnorderedSeq(~expr, ~startIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[])
                }
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
            if startIdx < curSeq.minConstMismatchIdx {
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