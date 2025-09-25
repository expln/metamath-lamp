module P = MM_wrk_pattern_search_v2_parser
module MC = MM_context

type matchResult = 
    | Matched(option<array<array<int>>>) 
    | NotMatched

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
    target: patternTarget,
    singleStmt: bool,
    minLen:int,
    mutable minConstMismatchIdx:int,
    minConstMismatchIdxs:Belt_HashMapInt.t<int>,
}
and seqGrp = 
    | Adjacent(array<sym>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)
and patternTarget = Frm | Hyps | Asrt

type pattern = {
    symSeq: symSeq,
    allSeq: array<symSeq>,
}

type subSeqMatchRes = {
    matchBegin:int,
    matchEnd:int
}

let getVarType = (varTypes: array<int>, var:int):int => {
    varTypes[var]->Option.getExn(~message=`No type is defiend for var ${var->Int.toString}`)
}

let exprSymMatchesSeqConst = (~exprSym:int, ~seqConst:int, ~varTypes: array<int>):bool => {
    exprSym == seqConst || exprSym >= 0 && varTypes->getVarType(exprSym) == seqConst
}

let countMinLen = (seq:array<symSeq>):int => seq->Array.reduce(0,(sum,seq)=>sum+seq.minLen)

let exprIncludesConstAdjSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:array<sym>, ~varTypes: array<int>
):int => {
    let begin = ref(startIdx)
    let maxBegin = maxIdx + 1 - seq->Array.length
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
                | Const(seqConst) => matched := exprSymMatchesSeqConst(~exprSym, ~seqConst, ~varTypes)
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

let getMinIdxForSingleStmt = (
    ~stmtI:int, ~exprLen:int, ~target:patternTarget, ~frmData:MC.patternSearchData
):int => {
    switch target {
        | Frm => frmData.stmtBnds->Array.getUnsafe(stmtI)
        | Hyps => {
            if (stmtI == frmData.numOfHyps) {
                exprLen
            } else {
                frmData.stmtBnds->Array.getUnsafe(stmtI)
            }
        }
        | Asrt => {
            if (stmtI == frmData.numOfHyps) {
                frmData.stmtBnds->Array.getUnsafe(stmtI)
            } else {
                exprLen
            }
        }
    }
}

let getMaxIdxForSingleStmt = (
    ~stmtI:int, ~exprLen:int, ~target:patternTarget, ~frmData:MC.patternSearchData
):int => {
    switch target {
        | Frm => frmData.stmtBnds[stmtI+1]->Option.mapOr(exprLen-1, nextStmtStart => nextStmtStart - 1)
        | Hyps => {
            if (stmtI == frmData.numOfHyps) {
                -1
            } else {
                frmData.stmtBnds->Array.getUnsafe(stmtI+1) - 1
            }
        }
        | Asrt => {
            if (stmtI == frmData.numOfHyps) {
                exprLen-1
            } else {
                -1
            }
        }
    }
}

let getMinIdxForNonSingleStmt = (
    ~exprLen:int, ~target:patternTarget, ~frmData:MC.patternSearchData
):int => {
    switch target {
        | Frm => 0
        | Hyps => if (frmData.numOfHyps > 0) { 0 } else { exprLen }
        | Asrt => frmData.stmtBnds->Array.getUnsafe(frmData.numOfHyps)
    }
}

let getMaxIdxForNonSingleStmt = (
    ~exprLen:int, ~target:patternTarget, ~frmData:MC.patternSearchData
):int => {
    switch target {
        | Frm => exprLen - 1
        | Hyps => if (frmData.numOfHyps > 0) { frmData.stmtBnds->Array.getUnsafe(frmData.numOfHyps) - 1 } else { -1 }
        | Asrt => exprLen - 1
    }
}

let rec exprIncludesConstSeqWithTarget = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:symSeq, ~varTypes: array<int>, ~frmData:MC.patternSearchData
):int => {
    let exprLen = expr->Array.length
    if (seq.singleStmt) {
        let stmtI = ref(0)
        let lastMatchedIdx = ref(-1)
        while (stmtI.contents <= frmData.numOfHyps && lastMatchedIdx.contents < 0) {
            let newStartIdx = Math.Int.max(
                startIdx,
                getMinIdxForSingleStmt(~stmtI=stmtI.contents, ~exprLen, ~target=seq.target, ~frmData)
            )
            let newMaxIdx = Math.Int.min(
                maxIdx,
                getMaxIdxForSingleStmt(~stmtI=stmtI.contents, ~exprLen, ~target=seq.target, ~frmData)
            )
            if (newStartIdx <= newMaxIdx) {
                seq.minConstMismatchIdx = seq.minConstMismatchIdxs->Belt_HashMapInt.get(stmtI.contents)
                    ->Option.getOr(exprLen)
                lastMatchedIdx := exprIncludesConstSeq(
                    ~expr, ~startIdx=newStartIdx, ~maxIdx=newMaxIdx, ~seq, ~varTypes, ~frmData
                )
                seq.minConstMismatchIdxs->Belt_HashMapInt.set(stmtI.contents, seq.minConstMismatchIdx)
            }
            stmtI := stmtI.contents + 1
        }
        lastMatchedIdx.contents
    } else {
        let newStartIdx = Math.Int.max(
            startIdx,
            getMinIdxForNonSingleStmt(~exprLen, ~target=seq.target, ~frmData)
        )
        let newMaxIdx = Math.Int.min(
            maxIdx,
            getMaxIdxForNonSingleStmt(~exprLen, ~target=seq.target, ~frmData)
        )
        if (newStartIdx <= newMaxIdx) {
            exprIncludesConstSeq(
                ~expr, ~startIdx=newStartIdx, ~maxIdx=newMaxIdx, ~seq, ~varTypes, ~frmData
            )
        } else {
            -1
        }
    }
}

and exprIncludesConstSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:symSeq, ~varTypes: array<int>, ~frmData:MC.patternSearchData
):int => {
    if (maxIdx < startIdx || seq.minConstMismatchIdx <= startIdx) {
        -1
    } else {
        let res = switch seq.elems {
            | Adjacent(seq) => exprIncludesConstAdjSeq(~expr, ~startIdx, ~maxIdx, ~seq, ~varTypes)
            | Ordered(childElems) => {
                exprIncludesConstOrderedSeq(~expr, ~startIdx, ~maxIdx, ~childElems, ~varTypes, ~frmData)
            }
            | Unordered(childElems) => {
                exprIncludesConstUnorderedSeq(
                    ~expr, ~startIdx, ~maxIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[], ~frmData
                )
            }
        }
        if (res < 0) {
            seq.minConstMismatchIdx = startIdx
        }
        res
    }
}

and let exprIncludesConstOrderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, 
    ~frmData:MC.patternSearchData
):int => {
    let lastMatchedIdx = ref(startIdx-1)
    let matched = ref(true)
    let i = ref(0)
    let maxI = childElems->Array.length - 1
    while (i.contents <= maxI && matched.contents) {
        lastMatchedIdx := exprIncludesConstSeqWithTarget(
            ~expr, ~startIdx=lastMatchedIdx.contents+1, ~maxIdx, 
            ~seq=childElems->Array.getUnsafe(i.contents), ~varTypes, ~frmData
        )
        matched := 0 <= lastMatchedIdx.contents && lastMatchedIdx.contents <= maxIdx
        i := i.contents + 1
    }
    if (matched.contents) {
        lastMatchedIdx.contents
    } else {
        -1
    }
}

and let exprIncludesConstUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>, 
    ~passedSeqIdxs:array<int>, ~frmData:MC.patternSearchData
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
                    let lastMatchedIdx = exprIncludesConstSeqWithTarget(
                        ~expr, ~startIdx, ~maxIdx, ~seq=curSeq, ~varTypes, ~frmData
                    )
                    if (0 <= lastMatchedIdx && lastMatchedIdx <= maxIdx) {
                        passedSeqIdxs->Array.push(i.contents)
                        res := exprIncludesConstUnorderedSeq(
                            ~expr, ~startIdx=lastMatchedIdx+1, ~maxIdx, ~childElems, ~varTypes, ~passedSeqIdxs, ~frmData
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
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:array<sym>, ~varTypes: array<int>,
    ~next:subSeqMatchRes=>unit
):unit => {
    let begin = ref(startIdx)
    let maxBegin = maxIdx + 1 - seq->Array.length
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
                | Const(seqConst) => matched := exprSymMatchesSeqConst(~exprSym, ~seqConst, ~varTypes)
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
            next({matchBegin:begin.contents, matchEnd:begin.contents + maxSeqI})
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

let rec exprIncludesVarSeqWithTarget = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:symSeq, ~varTypes: array<int>, ~frmData:MC.patternSearchData,
    ~next:subSeqMatchRes=>unit, ~stop:ref<bool>
):unit => {
    let exprLen = expr->Array.length
    if (seq.singleStmt) {
        let stmtI = ref(0)
        while (stmtI.contents <= frmData.numOfHyps && !stop.contents) {
            let newStartIdx = Math.Int.max(
                startIdx,
                getMinIdxForSingleStmt(~stmtI=stmtI.contents, ~exprLen, ~target=seq.target, ~frmData)
            )
            let newMaxIdx = Math.Int.min(
                maxIdx,
                getMaxIdxForSingleStmt(~stmtI=stmtI.contents, ~exprLen, ~target=seq.target, ~frmData)
            )
            if (newStartIdx <= newMaxIdx) {
                seq.minConstMismatchIdx = seq.minConstMismatchIdxs->Belt_HashMapInt.get(stmtI.contents)
                    ->Option.getOr(exprLen)
                exprIncludesVarSeq(
                    ~expr, ~startIdx=newStartIdx, ~maxIdx=newMaxIdx, ~seq, ~varTypes, ~next, ~stop, ~frmData
                )
                seq.minConstMismatchIdxs->Belt_HashMapInt.set(stmtI.contents, seq.minConstMismatchIdx)
            }
            stmtI := stmtI.contents + 1
        }
    } else {
        let newStartIdx = Math.Int.max(
            startIdx,
            getMinIdxForNonSingleStmt(~exprLen, ~target=seq.target, ~frmData)
        )
        let newMaxIdx = Math.Int.min(
            maxIdx,
            getMaxIdxForNonSingleStmt(~exprLen, ~target=seq.target, ~frmData)
        )
        if (newStartIdx <= newMaxIdx) {
            exprIncludesVarSeq(
                ~expr, ~startIdx=newStartIdx, ~maxIdx=newMaxIdx, ~seq, ~varTypes, ~next, ~stop, ~frmData
            )
        }
    }
}

and let exprIncludesVarSeq = ( 
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~seq:symSeq, ~varTypes:array<int>,
    ~next:subSeqMatchRes=>unit, ~stop:ref<bool>, ~frmData:MC.patternSearchData,
):unit => {
    if (startIdx <= maxIdx && startIdx < seq.minConstMismatchIdx) {
        switch seq.elems {
            | Adjacent(seq) => exprIncludesVarAdjSeq(~expr, ~startIdx, ~maxIdx, ~seq, ~varTypes, ~next)
            | Ordered(childElems) => {
                exprIncludesVarOrderedSeq(
                    ~expr, ~startIdx, ~maxIdx, ~childElems, ~varTypes, ~next, ~res=None, ~childElemIdx=0, ~stop, 
                    ~frmData
                )
            }
            | Unordered(childElems) => {
                exprIncludesVarUnorderedSeq(
                    ~expr, ~startIdx, ~maxIdx, ~childElems, ~varTypes, ~passedSeqIdxs=[], ~next, ~res=None, ~stop, 
                    ~frmData
                )
            }
        }
    }
}

and let exprIncludesVarOrderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>,
    ~next:subSeqMatchRes=>unit, ~res:option<subSeqMatchRes>, ~childElemIdx:int, ~stop:ref<bool>, 
    ~frmData:MC.patternSearchData
):unit => {
    if (childElems->Array.length <= childElemIdx) {
        switch res {
            | None => Exn.raiseError("exprIncludesVarOrderedSeq: res is None")
            | Some(res) => next({...res, matchEnd:startIdx-1})
        }
    } else if (startIdx <= maxIdx) {
        let curSeq = childElems->Array.getUnsafe(childElemIdx)
        let begin = ref(startIdx)
        let beginMax = maxIdx + 1 - curSeq.minLen
        while (!stop.contents && begin.contents <= beginMax && begin.contents < curSeq.minConstMismatchIdx) {
            let beginCopy = begin.contents
            begin := beginMax + 1 // this ends the while loop unless begin is changed in the next()
            exprIncludesVarSeqWithTarget(
                ~expr, ~startIdx=beginCopy, ~maxIdx, ~seq=curSeq, ~varTypes,
                ~next = curSeqRes => {
                    if (curSeqRes.matchEnd <= maxIdx) {
                        exprIncludesVarOrderedSeq(
                            ~expr, ~startIdx=curSeqRes.matchEnd+1, ~maxIdx, ~childElems, ~varTypes, ~next,
                            ~res=switch res {|Some(_)=>res |None=>Some({matchBegin:beginCopy, matchEnd:-1})},
                            ~childElemIdx=childElemIdx+1, ~stop, ~frmData
                        )
                        begin := curSeqRes.matchBegin + 1
                    }
                }, 
                ~stop, ~frmData
            )
        }
    }
}

and let exprIncludesVarUnorderedSeq = (
    ~expr:array<int>, ~startIdx:int, ~maxIdx:int, ~childElems:array<symSeq>, ~varTypes: array<int>,
    ~passedSeqIdxs:array<int>, ~next:subSeqMatchRes=>unit, ~res:option<subSeqMatchRes>, ~stop:ref<bool>, 
    ~frmData:MC.patternSearchData
):unit => {
    if (passedSeqIdxs->Array.length == childElems->Array.length) {
        switch res {
            | None => Exn.raiseError("exprIncludesVarUnorderedSeq: res is None")
            | Some(res) => next({...res, matchEnd:startIdx-1})
        }
    } else if (startIdx <= maxIdx) {
        let i = ref(0)
        let maxI = childElems->Array.length - 1
        while (!stop.contents && i.contents <= maxI) {
            if (!(passedSeqIdxs->Array.includes(i.contents))) {
                let curSeq = childElems->Array.getUnsafe(i.contents)
                let begin = ref(startIdx)
                let beginMax = maxIdx + 1 - curSeq.minLen
                while (!stop.contents && begin.contents <= beginMax && begin.contents < curSeq.minConstMismatchIdx) {
                    let beginCopy = begin.contents
                    begin := beginMax + 1 // this ends the while loop unless begin is changed in the next()
                    exprIncludesVarSeqWithTarget(
                        ~expr, ~startIdx=beginCopy, ~maxIdx, ~seq=curSeq, ~varTypes,
                        ~next = curSeqRes => {
                            if (curSeqRes.matchEnd <= maxIdx) {
                                passedSeqIdxs->Array.push(i.contents)
                                exprIncludesVarUnorderedSeq(
                                    ~expr, ~startIdx=curSeqRes.matchEnd+1, ~maxIdx, 
                                    ~childElems, ~varTypes, ~passedSeqIdxs, ~next,
                                    ~res=switch res {|Some(_)=>res |None=>Some({matchBegin:beginCopy, matchEnd:-1})},
                                    ~stop, ~frmData
                                )
                                passedSeqIdxs->Array.pop->ignore
                                begin := curSeqRes.matchBegin + 1
                            }
                        },
                        ~stop, ~frmData
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
    ~expr:array<int>, ~seq:symSeq, ~varTypes:array<int>, ~frmData:MC.patternSearchData
):option<array<int>> => {
    let res = ref(None)
    let maxIdx = expr->Array.length-1
    if (exprIncludesConstSeqWithTarget(~expr, ~startIdx=0, ~maxIdx, ~seq, ~varTypes, ~frmData) >= 0) {
        let stop = ref(false)
        exprIncludesVarSeqWithTarget(
            ~expr, ~startIdx=0, ~maxIdx, ~seq, ~varTypes,
            ~next = _ => {
                stop := true
                switch res.contents {
                    | None => res := Some(getMatchedIndices(seq))
                    | Some(_) => Exn.raiseError("next() is called twice in exprIncludesSeq.")
                }
            },
            ~stop, ~frmData
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

let rec astToSymSeq = (ast:P.symSeq, parentFlags:P.flags, symMap:Belt_HashMapString.t<constOrVar>):symSeq => {
    let elems = astToSeqGrp(ast.elems, P.passFlagsFromParentToChild(parentFlags, ast.flags), symMap)
    let minLen = switch elems {
        | Adjacent(syms) => syms->Array.length
        | Ordered(symSeq) | Unordered(symSeq) => countMinLen(symSeq)
    }
    { 
        elems, 
        minLen, 
        minConstMismatchIdx: -1, 
        minConstMismatchIdxs: Belt_HashMapInt.make(~hintSize=20),
        target: switch ast.flags.target {|Frm=>Frm |Hyps=>Hyps |Asrt=>Asrt},
        singleStmt: ast.flags.singleStmt
    }
}
and astToSeqGrp = (ast:P.seqGrp, flags:P.flags, symMap:Belt_HashMapString.t<constOrVar>):seqGrp => {
    switch ast {
        | Symbols(syms) => {
            if (isAdj(flags)) {
                Adjacent(syms->Array.map(makeSym(_,symMap)))
            } else {
                let target = switch flags.target {|Frm=>Frm |Hyps=>Hyps |Asrt=>Asrt}
                let singleStmt = flags.singleStmt
                Ordered(syms->Array.map(symStr => {
                    {
                        elems:Adjacent([makeSym(symStr,symMap)]),
                        minLen:1,
                        minConstMismatchIdx:-1,
                        minConstMismatchIdxs: Belt_HashMapInt.make(~hintSize=20),
                        target,
                        singleStmt,
                    }
                }))
            }
        }
        | Ordered(syms) => Ordered(syms->Array.map(astToSymSeq(_, flags, symMap)))
        | Unordered(syms) => Unordered(syms->Array.map(astToSymSeq(_, flags, symMap)))
    }
}

let rec traverseAst = (
    seq:P.symSeq,
    ~onSymSeq:P.symSeq=>unit=_=>(),
    ~onSeqGrp:P.seqGrp=>unit=_=>(),
    ~onSym:string=>unit=_=>(),
):unit => {
    onSymSeq(seq)
    onSeqGrp(seq.elems)
    switch seq.elems {
        | Symbols(syms) => syms->Array.forEach(onSym)
        | Ordered(childSeq) | Unordered(childSeq) => {
            childSeq->Array.forEach(traverseAst(_, ~onSymSeq, ~onSeqGrp, ~onSym))
        }
    }
}

let rec traversePattern = (
    seq:symSeq,
    ~onSymSeq:symSeq=>unit=_=>(),
    ~onSeqGrp:seqGrp=>unit=_=>(),
    ~onSym:sym=>unit=_=>(),
):unit => {
    onSymSeq(seq)
    onSeqGrp(seq.elems)
    switch seq.elems {
        | Adjacent(syms) => syms->Array.forEach(onSym)
        | Ordered(childSeq) | Unordered(childSeq) => {
            childSeq->Array.forEach(traversePattern(_, ~onSymSeq, ~onSeqGrp, ~onSym))
        }
    }
}

let collectAllSeq = (seq:symSeq, allSeq:array<symSeq>):unit => {
    traversePattern(seq, ~onSymSeq=s=>allSeq->Array.push(s))
}

let astToPattern = (ast:P.pattern, symMap:Belt_HashMapString.t<constOrVar>):pattern => {
    let res = {
        symSeq: astToSymSeq(ast.symSeq, P.parseFlags(""), symMap),
        allSeq: []
    }
    collectAllSeq(res.symSeq, res.allSeq)
    res
}

let makeSymMap = (ast:P.pattern, ctx:MC.mmContext):result<Belt_HashMapString.t<constOrVar>, string> => {
    let symMap = Belt_HashMapString.make(~hintSize=20)
    let errors:array<string> = []
    traverseAst(ast.symSeq, ~onSym=sym => {
        switch ctx->MC.getTokenType(sym) {
            | Some(C) => {
                if (!(symMap->Belt_HashMapString.has(sym))) {
                    symMap->Belt_HashMapString.set(sym, Const(ctx->MC.ctxSymToIntExn(sym)))
                }
            }
            | Some(V) => {
                if (!(symMap->Belt_HashMapString.has(sym))) {
                    symMap->Belt_HashMapString.set(sym, Var({
                        typ: ctx->MC.getTypeOfVarExn(ctx->MC.ctxSymToIntExn(sym)),
                        capVar: -1,
                        capVarIdx: -1,
                    }))
                }
            }
            | Some(F) | Some(E) | Some(A) | Some(P) | None => errors->Array.push(`'${sym}' is not a math symbol`)
        }
    })
    if (errors->Array.length == 0) {
        Ok(symMap)
    } else {
        Error(errors->Array.join("; "))
    }
}

let checkControlToken = (tok:string, errors:array<string>):unit => {
    if (!(tok->String.startsWith("$"))) {
        errors->Array.push(`'${tok}' - all control tokens must start with '$'`)
    } else if (!(
        tok == P.operatorOrdered || tok == P.operatorUnordered || tok == P.openParenthesis || tok == P.closeParenthesis
    )) {
        let flags = tok->String.substringToEnd(~start=tok->String.startsWith(P.openParenthesis)?2:1)
        if (flags->String.length > 0) {
            let flagH = ref(false)
            let flagA = ref(false)
            let flagP = ref(false)
            let flagM = ref(false)
            let flagS = ref(false)
            for i in 0 to flags->String.length-1 {
                let flag = flags->String.charAt(i)
                if (flag == "h") {flagH := true}
                else if (flag == "a") {flagA := true}
                else if (flag == "+") {flagP := true}
                else if (flag == "-") {flagM := true}
                else if (flag == "s") {flagS := true}
                else {
                    errors->Array.push(`'${tok}' - invalid flag '${flag}'`)
                }
            }
            if (flagH.contents && flagA.contents) {
                errors->Array.push(`'${tok}' - flags 'h' and 'a' cannot be used together`)
            }
            if (flagP.contents && flagM.contents) {
                errors->Array.push(`'${tok}' - flags '+' and '-' cannot be used together`)
            }
        }
    }
}

let checkParenthesesMatch = (tokens:array<string>):bool => {
    let parenCnt = ref(0)
    let i = ref(0)
    let maxI = tokens->Array.length-1
    let mismatchFound = ref(false)
    while (i.contents <= maxI && !mismatchFound.contents) {
        let tok = tokens->Array.getUnsafe(i.contents)
        if (tok->String.startsWith(P.openParenthesis)) {
            parenCnt := parenCnt.contents + 1
        } else if (tok == P.closeParenthesis) {
            parenCnt := parenCnt.contents - 1
            if (parenCnt.contents < 0) {
                mismatchFound := true
            }
        }
        i := i.contents + 1
    }
    !mismatchFound.contents && parenCnt.contents == 0
}

let validatePattern = (
    ~text:string, 
    ~ctx:MC.mmContext,
):option<string> => {
    let tokens = text->String.trim->Common.getSpaceSeparatedValuesAsArray
    let errors = []
    tokens->Array.forEach(tok => {
        if (tok->String.includes("$")) {
            checkControlToken(tok, errors)
        } else {
            switch ctx->MC.getTokenType(tok) {
                | Some(C) | Some(V) => ()
                | Some(F) | Some(E) | Some(A) | Some(P) | None => {
                    errors->Array.push(`'${tok}' - is not a constant or variable`)
                }
            }
        }
    })
    if (!checkParenthesesMatch(tokens)) {
        errors->Array.push(`parentheses mismatch`)
    }
    if (tokens->Array.length > 0) {
        switch P.isPatternBegin(tokens->Array.getUnsafe(0)) {
            | Some(_) => ()
            | None => {
                tokens->Array.reduceWithIndex(None, (found,tok,i) => {
                    switch found {
                        | Some(_) => found
                        | None => {
                            if (i == 0) {found} else {P.isPatternBegin(tok)}
                        }
                    }
                })->Option.forEach(_=>{
                    errors->Array.push(`when multiple patterns are specified, each pattern must begin with $`)
                })
            }
        }
    }
    if (errors->Array.length == 0) {
        None
    } else {
        Some(errors->Array.join("\n"))
    }
}

let parsePattern = (
    text:string, 
    ~symMap:option<Belt_HashMapString.t<constOrVar>>=?, //symMap may be passed as a parameter only for testing
    ~ctx:option<MC.mmContext>=?,
):result<array<pattern>,string> => {
    let text = text->String.trim
    if (text->String.length == 0) {
        Ok([])
    } else {
        switch ctx->Option.flatMap(ctx => validatePattern(~text, ~ctx)) {
            | Some(errors) => Error(errors)
            | None => {
                switch P.parsePattern(text) {
                    | None => Error(`Cannot parse the pattern '${text}'`)
                    | Some(asts) => {
                        let subpatterns:array<result<pattern,string>> = asts->Array.map(ast => {
                            let symMap:result<Belt_HashMapString.t<constOrVar>, string> = switch symMap {
                                | Some(symMap) => Ok(symMap)
                                | None => {
                                    makeSymMap(
                                        ast, 
                                        ctx->Option.getExn(
                                            ~message="parsePattern: either symMap or ctx must be provided."
                                        )
                                    )
                                }
                            }
                            symMap->Result.map(astToPattern(ast, _))
                        })
                        subpatterns->Array.reduce(Ok([]), (acc, subpatRes) => {
                            switch acc {
                                | Ok(subpatArr) => {
                                    switch subpatRes {
                                        | Ok(subpat) => {
                                            subpatArr->Array.push(subpat)
                                            Ok(subpatArr)
                                        }
                                        | Error(msg) => Error(msg)
                                    }
                                }
                                | Error(prevMsg) => {
                                    switch subpatRes {
                                        | Ok(_) => Error(prevMsg)
                                        | Error(msg) => Error(prevMsg ++ "; " ++ msg)
                                    }
                                }
                            }
                        })
                    }
                }
            }
        }
    }
}

let makeEmptyMatchedIdxs = (numOfStmts:int):array<array<int>> => {
    Array.fromInitializer(~length=numOfStmts, _=>[])
}

let convertMatchedIndices = (frm:MC.frame, idxs:array<int>):array<array<int>> => {
    let hyps = frm.hyps->Array.filter(hyp => hyp.typ == E)
    let numOfHyps = hyps->Array.length
    let res = makeEmptyMatchedIdxs(numOfHyps+1)
    let idxI = ref(0)
    let maxIdxI = idxs->Array.length-1
    let hypI = ref(0)
    let maxHypI = numOfHyps-1
    let hypLenSum = ref(0)
    while (hypI.contents <= maxHypI) {
        let curHypLen = (hyps->Array.getUnsafe(hypI.contents)).expr->Array.length
        let maxIdx = hypLenSum.contents + curHypLen - 1
        let curResArr = res->Array.getUnsafe(hypI.contents)
        let curIdx = ref(idxs[idxI.contents])
        while (curIdx.contents->Option.mapOr(false, curIdx => curIdx <= maxIdx) && idxI.contents <= maxIdxI) {
            curResArr->Array.push(curIdx.contents->Option.getExn - hypLenSum.contents)
            idxI := idxI.contents + 1
            curIdx := idxs[idxI.contents]
        }
        hypLenSum := hypLenSum.contents + curHypLen
        hypI := hypI.contents + 1
    }
    let curResArr = res->Array.getUnsafe(hypI.contents)
    while (idxI.contents <= maxIdxI) {
        curResArr->Array.push(idxs->Array.getUnsafe(idxI.contents) - hypLenSum.contents)
        idxI := idxI.contents + 1
    }
    res
}

let mergeMatchedIndices = (idxs:array<array<array<int>>>):array<array<int>> => {
    let numOfStmts = idxs[0]->Option.getExn(~message="mergeMatchedIndices: error 1")->Array.length
    let res:array<array<int>> = makeEmptyMatchedIdxs(numOfStmts)
    idxs->Array.forEach((patIdxs:array<array<int>>) => {
        patIdxs->Array.forEachWithIndex((stmtIdxs:array<int>, stmtI) => {
            res[stmtI]->Option.getExn(~message="mergeMatchedIndices: error 2")->Array.pushMany(stmtIdxs)
        })
    })
    res->Array.forEach(stmtIdxs => Array.sort(stmtIdxs, Int.compare))
    res
}

let frameMatchesPattern = (frm:MC.frame, pattern:pattern):option<array<array<int>>> => {
    let frmData = MC.frmGetPatternSearchData(frm)
    let expr = frmData.allHypsAsrt
    let exprLen = expr->Array.length
    pattern.allSeq->Array.forEach(seq => {
        seq.minConstMismatchIdx = exprLen
        seq.minConstMismatchIdxs->Belt_HashMapInt.clear
    })
    exprIncludesSeq(~expr, ~seq=pattern.symSeq, ~varTypes=frm.varTypes, ~frmData)
        ->Option.map(convertMatchedIndices(frm, _))
}

let frameMatchesPatterns = (frm:MC.frame, patterns:array<pattern>):matchResult => {
    if (patterns->Array.length == 0) {
        Matched(None)
    } else {
        switch patterns->Array.reduce(
            Some([]),
            (idxs,pattern) => {
                switch idxs {
                    | None => None
                    | Some(idxs) => {
                        switch frameMatchesPattern(frm, pattern) {
                            | None => None
                            | Some(patIdxs) => {
                                idxs->Array.push(patIdxs)
                                Some(idxs)
                            }
                        }
                    }
                }
            }
        )->Option.map(mergeMatchedIndices) {
            | Some(idxs) => Matched(Some(idxs))
            | None => NotMatched
        }
    }
}