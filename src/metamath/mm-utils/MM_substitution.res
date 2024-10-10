open Expln_utils_common
open MM_parenCounter
open MM_context
open MM_unification_debug

type contunieInstruction = Continue | Stop

type constParts = {
    length: int,
    begins: array<int>,
    ends: array<int>,
    remainingMinLength: array<int>,
}

type varGroup = {
    leftConstPartIdx:int,
    frmExpr:expr,
    varsBeginIdx:int,
    numOfVars:int,
    mutable exprBeginIdx:int,
    mutable exprEndIdx:int
}

type workVars = {
    maxVar:int,
    newVars: array<int>,
    newVarTypes: array<int>,
}

type subs = {
    size: int,
    begins: array<int>,
    ends: array<int>,
    exprs: array<expr>,
    isDefined: array<bool>,
}

type frmSubsData = {
    frame: frame,
    hypsE: array<hypothesis>,
    numOfHypsE:int,
    frmConstParts:array<constParts>,
    constParts:array<constParts>,
    varGroups:array<array<varGroup>>,
    subs:subs,
}

type frms = {
    all: array<frmSubsData>,
    byType:Belt_HashMapInt.t<array<frmSubsData>>,
    byLabel:Belt_HashMapString.t<frmSubsData>,
}

let subsClone = subs => {
    {
        size: subs.size,
        begins: subs.begins->Js.Array2.copy,
        ends: subs.ends->Js.Array2.copy,
        exprs: subs.exprs->Js.Array2.copy,
        isDefined: subs.isDefined->Js.Array2.copy,
    }
}

let subsEq = (a:subs, b:subs):bool => {
    if (a.size == b.size && a.isDefined->Js_array2.every(b => b) && b.isDefined->Js_array2.every(b => b)) {
        let res = ref(true)
        let v = ref(0)
        while (res.contents && v.contents < a.size) {
            let aExpr = a.exprs->Array.getUnsafe(v.contents)
            let aBegin = a.begins->Array.getUnsafe(v.contents)
            let aEnd = a.ends->Array.getUnsafe(v.contents)
            let aExprLen = aEnd - aBegin + 1
            let bExpr = b.exprs->Array.getUnsafe(v.contents)
            let bBegin = b.begins->Array.getUnsafe(v.contents)
            let bEnd = b.ends->Array.getUnsafe(v.contents)
            let bExprLen = bEnd - bBegin + 1
            if (aExprLen == bExprLen) {
                let ai = ref(aBegin)
                let bi = ref(bBegin)
                while (res.contents && ai.contents <= aEnd) {
                    res.contents = aExpr->Array.getUnsafe(ai.contents) == bExpr->Array.getUnsafe(bi.contents)
                    ai.contents = ai.contents + 1
                    bi.contents = bi.contents + 1
                }
            } else {
                res.contents = false
            }
            v.contents = v.contents + 1
        }
        res.contents
    } else {
        false
    }
}

let subsHash = (subs:subs):int => {
    if (subs.isDefined->Js_array2.every(b => b)) {
        let hash = ref(0)
        let v = ref(0)
        while (v.contents < subs.size) {
            hash.contents = Expln_utils_common.hash2(
                hash.contents,
                Expln_utils_common.hashArrIntFromTo(
                    subs.exprs->Array.getUnsafe(v.contents),
                    subs.begins->Array.getUnsafe(v.contents),
                    subs.ends->Array.getUnsafe(v.contents),
                )
            )
            v.contents = v.contents + 1
        }
        hash.contents
    } else {
        0
    }
}

let lengthOfGap = (leftConstPartIdx:int, constParts:array<array<int>>, exprLength:int):int => {
    if (leftConstPartIdx < 0) {
        constParts->Array.getUnsafe(0)->Array.getUnsafe(0)
    } else if (leftConstPartIdx < constParts->Js_array2.length - 1) {
        constParts->Array.getUnsafe(leftConstPartIdx+1)->Array.getUnsafe(0) - constParts->Array.getUnsafe(leftConstPartIdx)->Array.getUnsafe(1) - 1
    } else {
        exprLength - constParts->Array.getUnsafe(leftConstPartIdx)->Array.getUnsafe(1) - 1
    }
}

let lengthOfGap2 = (leftConstPartIdx:int, constParts:constParts, exprLength:int):int => {
    if (leftConstPartIdx < 0) {
        constParts.begins->Array.getUnsafe(0)
    } else if (leftConstPartIdx < constParts.length - 1) {
        constParts.begins->Array.getUnsafe(leftConstPartIdx+1) - constParts.ends->Array.getUnsafe(leftConstPartIdx) - 1
    } else {
        exprLength - constParts.ends->Array.getUnsafe(leftConstPartIdx) - 1
    }
}

let createConstParts = expr => {
    let constParts = []
    for i in 0 to expr->Js_array2.length-1 {
        let constPartsLength = constParts->Js_array2.length
        if (expr->Array.getUnsafe(i) < 0) {
            if (constPartsLength == 0 || constParts->Array.getUnsafe(constPartsLength-1)->Array.getUnsafe(1) >= 0) {
                constParts->Array.push([i,-1])
            }
        } else if (constPartsLength > 0 && constParts->Array.getUnsafe(constPartsLength-1)->Array.getUnsafe(1) < 0) {
            (constParts->Array.getUnsafe(constPartsLength-1))[1] = i-1
        }
    }
    let constPartsLength = constParts->Js_array2.length
    let exprLength = expr->Js_array2.length
    if (constPartsLength > 0 && constParts->Array.getUnsafe(constPartsLength-1)->Array.getUnsafe(1) < 0) {
        (constParts->Array.getUnsafe(constPartsLength-1))[1] = exprLength-1
    }
    let result = {
        length: constPartsLength,
        begins: createArray(constPartsLength),
        ends: createArray(constPartsLength),
        remainingMinLength: createArray(constPartsLength)
    }
    let remainingMinLength = ref(0)
    for i in constPartsLength-1 downto 0 {
        result.begins[i] = constParts->Array.getUnsafe(i)->Array.getUnsafe(0)
        result.ends[i] = constParts->Array.getUnsafe(i)->Array.getUnsafe(1)
        remainingMinLength.contents = remainingMinLength.contents + (result.ends->Array.getUnsafe(i) - result.begins->Array.getUnsafe(i) + 1) + lengthOfGap(i, constParts, exprLength)
        result.remainingMinLength[i] = remainingMinLength.contents
    }
    result
}

let createMatchingConstParts = constParts => {
    {
        length: constParts.length,
        begins: createArray(constParts.length),
        ends: createArray(constParts.length),
        remainingMinLength: []
    }
}

let rec iterateConstParts = (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~idxToMatch:int, 
    ~parenCnt:parenCnt,
    ~consumer:constParts => contunieInstruction
):contunieInstruction => {
    let invokeNext = ():contunieInstruction => {
        iterateConstParts(
            ~frmExpr, 
            ~expr, 
            ~frmConstParts, 
            ~constParts, 
            ~idxToMatch=idxToMatch+1, 
            ~parenCnt,
            ~consumer
        )
    }

    let exprLen = expr->Js_array2.length
    let frmExprLen = frmExpr->Js_array2.length

    if (exprLen < frmExprLen) {
        Continue
    } else if (idxToMatch == frmConstParts.length) {
        if (frmConstParts.length > 0) {
            if (constParts.ends->Array.getUnsafe(idxToMatch-1) != exprLen-1) {
                if (frmConstParts.ends->Array.getUnsafe(idxToMatch-1) == frmExprLen-1) {
                    Continue
                } else {
                    let frmRemainingGapLength = lengthOfGap2(idxToMatch-1, frmConstParts, frmExprLen)
                    let remainingGapLength = lengthOfGap2(idxToMatch-1, constParts, exprLen)
                    if (remainingGapLength < frmRemainingGapLength) {
                        Continue
                    } else {
                        if (
                            !(parenCnt->parenCntCanBeFirst(expr->Array.getUnsafe(constParts.ends->Array.getUnsafe(idxToMatch-1)+1)))
                            || !(parenCnt->parenCntCanBeLast(expr->Array.getUnsafe(exprLen-1)))
                        ) {
                            Continue
                        } else {
                            parenCnt->parenCntReset
                            let pState = ref(Balanced)
                            let i = ref(constParts.ends->Array.getUnsafe(idxToMatch-1)+1)
                            while (i.contents < exprLen && pState.contents != Failed) {
                                pState.contents = parenCnt->parenCntPut(expr->Array.getUnsafe(i.contents))
                                i.contents = i.contents + 1
                            }
                            if (pState.contents == Balanced) {
                                consumer(constParts)
                            } else {
                                Continue
                            }
                        }
                    }
                }
            } else {
                consumer(constParts)
            }
        } else if (parenCnt->parenCntCanBeFirst(expr->Array.getUnsafe(0)) && parenCnt->parenCntCanBeLast(expr->Array.getUnsafe(exprLen-1))) {
            consumer(constParts)
        } else {
            Continue
        }
    } else if (idxToMatch == 0 && frmConstParts.begins->Array.getUnsafe(0) == 0) {
        if (exprLen-1 < frmConstParts.ends->Array.getUnsafe(0)) {
            Continue
        } else {
            let res = ref(None)
            let maxI = frmConstParts.ends->Array.getUnsafe(0)
            let i = ref(0)
            while (res.contents->Belt_Option.isNone && i.contents <= maxI) {
                if (frmExpr->Array.getUnsafe(i.contents) != expr->Array.getUnsafe(i.contents)) {
                    res.contents = Some(Continue)
                }
                i.contents = i.contents + 1
            }
            switch res.contents {
                | Some(instr) => instr
                | None => {
                    if (maxI == exprLen-1 || parenCnt->parenCntCanBeFirst(expr->Array.getUnsafe(maxI+1))) {
                        constParts.begins[0] = 0
                        constParts.ends[0] = maxI
                        invokeNext()
                    } else {
                        Continue
                    }
                }
            }
        }
    } else {
        let begin = ref(if (idxToMatch == 0) {0} else {constParts.ends->Array.getUnsafe(idxToMatch-1)+1})
        let maxBegin = exprLen - frmConstParts.remainingMinLength->Array.getUnsafe(idxToMatch)
        parenCnt->parenCntReset
        let pState = ref(Balanced)
        let numOfVars = lengthOfGap2(idxToMatch-1,frmConstParts,frmExprLen)
        for _ in 1 to numOfVars {
            pState.contents = parenCnt->parenCntPut(expr->Array.getUnsafe(begin.contents))
            begin.contents = begin.contents + 1
        }
        let partLen = frmConstParts.ends->Array.getUnsafe(idxToMatch) - frmConstParts.begins->Array.getUnsafe(idxToMatch) + 1
        let instr = ref(Continue)
        while (begin.contents <= maxBegin && pState.contents != Failed && instr.contents == Continue) {
            if (pState.contents == Balanced) {
                let matchedLen = ref(0)
                let cmpRes = ref(true)
                while (matchedLen.contents < partLen && cmpRes.contents) {
                    cmpRes.contents = frmExpr->Array.getUnsafe(frmConstParts.begins->Array.getUnsafe(idxToMatch)+matchedLen.contents) == expr->Array.getUnsafe(begin.contents+matchedLen.contents)
                    matchedLen.contents = matchedLen.contents + 1
                }
                let end = begin.contents+partLen-1
                if (
                    matchedLen.contents == partLen && cmpRes.contents
                    && parenCnt->parenCntCanBeLast(expr->Array.getUnsafe(begin.contents-1))
                    && (end == exprLen-1 || parenCnt->parenCntCanBeFirst(expr->Array.getUnsafe(end+1)))
                ) {
                    constParts.begins[idxToMatch] = begin.contents
                    constParts.ends[idxToMatch] = end
                    instr.contents = invokeNext()
                    parenCnt->parenCntReset
                }
            }
            pState.contents = parenCnt->parenCntPut(expr->Array.getUnsafe(begin.contents))
            begin.contents = begin.contents + 1
        }
        instr.contents
    }
}

let createVarGroups = (~frmExpr:expr, ~frmConstParts:constParts): array<varGroup> => {
    let frmExprLen = frmExpr->Js_array2.length
    if (frmConstParts.length == 0) {
        [{
            leftConstPartIdx: -1,
            frmExpr:frmExpr,
            varsBeginIdx: 0,
            numOfVars: frmExprLen,
            exprBeginIdx: 0,
            exprEndIdx: 0
        }]
    } else {
        let res = []
        if (frmConstParts.begins->Array.getUnsafe(0) != 0) {
            res->Array.push({
                leftConstPartIdx: -1,
                frmExpr:frmExpr,
                varsBeginIdx:0,
                numOfVars:frmConstParts.begins->Array.getUnsafe(0),
                exprBeginIdx: 0,
                exprEndIdx: 0
            })
        }
        for i in 0 to frmConstParts.length-2 {
            res->Array.push({
                leftConstPartIdx: i,
                frmExpr:frmExpr,
                varsBeginIdx: frmConstParts.ends->Array.getUnsafe(i)+1,
                numOfVars: lengthOfGap2(i, frmConstParts, frmExprLen),
                exprBeginIdx: 0,
                exprEndIdx: 0
            })
        }
        let lastConstPartIdx = frmConstParts.length-1
        if (frmConstParts.ends->Array.getUnsafe(lastConstPartIdx) != frmExprLen-1) {
            res->Array.push({
                leftConstPartIdx: lastConstPartIdx,
                frmExpr:frmExpr,
                varsBeginIdx: frmConstParts.ends->Array.getUnsafe(lastConstPartIdx)+1,
                numOfVars: lengthOfGap2(lastConstPartIdx, frmConstParts, frmExprLen),
                exprBeginIdx: 0,
                exprEndIdx: 0
            })
        }
        res
    }
}

let initVarGroups = (~varGroups:array<varGroup>, ~constParts:constParts, ~expr:expr) => {
    let exprLen = expr->Js_array2.length
    if (constParts.length == 0) {
        (varGroups->Array.getUnsafe(0)).exprBeginIdx = 0
        (varGroups->Array.getUnsafe(0)).exprEndIdx = exprLen-1
    } else {
        varGroups->Js_array2.forEach(grp => {
            if (grp.leftConstPartIdx == -1) {
                grp.exprBeginIdx = 0
                grp.exprEndIdx = constParts.begins->Array.getUnsafe(0) - 1
            } else if (grp.leftConstPartIdx == constParts.length-1) {
                grp.exprBeginIdx = constParts.ends->Array.getUnsafe(grp.leftConstPartIdx)+1
                grp.exprEndIdx = exprLen-1
            } else {
                grp.exprBeginIdx = constParts.ends->Array.getUnsafe(grp.leftConstPartIdx)+1
                grp.exprEndIdx = constParts.begins->Array.getUnsafe(grp.leftConstPartIdx+1)-1
            }
        })
        varGroups->Expln_utils_common.sortInPlaceWith((g1,g2) => Belt_Float.fromInt(g1.numOfVars - g2.numOfVars))->ignore
    }
}

let rec iterateVarGroups = (
    ~expr:expr,
    ~subs:subs,
    ~varGroups:array<varGroup>,
    ~curGrpIdx:int,
    ~curVarIdx:int,
    ~subExprBeginIdx:int,
    ~parenCnt:parenCnt,
    ~consumer: subs=>contunieInstruction
): contunieInstruction => {
    let grp = varGroups->Array.getUnsafe(curGrpIdx)
    let frmVar = grp.frmExpr->Array.getUnsafe(grp.varsBeginIdx+curVarIdx)
    let maxSubExprLength = grp.exprEndIdx - subExprBeginIdx + 1 - (grp.numOfVars - curVarIdx - 1)
    
    let invokeNext = (subExprLength:int):contunieInstruction => {
        if (curVarIdx < grp.numOfVars - 1) {
            iterateVarGroups(
                ~expr,
                ~subs,
                ~varGroups,
                ~curGrpIdx,
                ~curVarIdx = curVarIdx+1,
                ~subExprBeginIdx = subExprBeginIdx+subExprLength,
                ~parenCnt,
                ~consumer
            )
        } else if (curGrpIdx < varGroups->Js_array2.length-1) {
            iterateVarGroups(
                ~expr,
                ~subs,
                ~varGroups,
                ~curGrpIdx = curGrpIdx+1,
                ~curVarIdx = 0,
                ~subExprBeginIdx = (varGroups->Array.getUnsafe(curGrpIdx+1)).exprBeginIdx,
                ~parenCnt,
                ~consumer
            )
        } else {
            consumer(subs)
        }
    }

    let continueInstr = ref(Continue)
    if (!(subs.isDefined->Array.getUnsafe(frmVar))) {
        subs.isDefined[frmVar] = true
        subs.exprs[frmVar] = expr
        subs.begins[frmVar] = subExprBeginIdx
        if (curVarIdx == grp.numOfVars-1) {
            subs.ends[frmVar] = grp.exprEndIdx
            continueInstr.contents = invokeNext(maxSubExprLength)
        } else if (parenCnt->parenCntCanBeFirst(expr->Array.getUnsafe(subExprBeginIdx))) {
            let subExprLength = ref(1)
            let end = ref(subExprBeginIdx)
            parenCnt->parenCntReset
            let pStatus = ref(Balanced)
            while (subExprLength.contents <= maxSubExprLength && continueInstr.contents == Continue && pStatus.contents != Failed) {
                subs.ends[frmVar] = end.contents
                pStatus.contents = parenCnt->parenCntPut(expr->Array.getUnsafe(end.contents))
                if (pStatus.contents == Balanced && parenCnt->parenCntCanBeLast(expr->Array.getUnsafe(end.contents))) {
                    continueInstr.contents = invokeNext(subExprLength.contents)
                    parenCnt->parenCntReset
                }
                subExprLength.contents = subExprLength.contents + 1
                end.contents = end.contents + 1
            }
        }
        subs.isDefined[frmVar] = false
    } else {
        let existingExpr = subs.exprs->Array.getUnsafe(frmVar)
        let existingExprBeginIdx = subs.begins->Array.getUnsafe(frmVar)
        let existingExprLen = subs.ends->Array.getUnsafe(frmVar) - existingExprBeginIdx + 1
        if (existingExprLen <= maxSubExprLength && (curVarIdx < grp.numOfVars-1 || existingExprLen == maxSubExprLength)) {
            let checkedLen = ref(0)
            while (checkedLen.contents < existingExprLen 
                    && existingExpr->Array.getUnsafe(existingExprBeginIdx+checkedLen.contents) == expr->Array.getUnsafe(subExprBeginIdx+checkedLen.contents)) {
                        checkedLen.contents = checkedLen.contents + 1
            }
            if (checkedLen.contents == existingExprLen) {
                continueInstr.contents = invokeNext(existingExprLen)
            }
        }
    }
    continueInstr.contents
}

let iterateSubstitutions = (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~varGroups:array<varGroup>,
    ~subs:subs,
    ~parenCnt:parenCnt,
    ~consumer: subs => contunieInstruction
):contunieInstruction => {
    if (subs.size == 0) {
        if (frmExpr->exprEq(expr)) {
            consumer(subs)
        } else {
            Continue
        }
    } else {
        let exprLen = expr->Js_array2.length
        let frmExprLen = frmExpr->Js_array2.length
        if (exprLen < frmExprLen || exprLen == 0 || frmExprLen == 0) {
            Continue
        } else {
            let frmExprEnd = frmExpr->Js_array2.unsafe_get(frmExprLen-1)
            if (frmExprEnd < 0 && frmExprEnd != expr->Js_array2.unsafe_get(exprLen-1)) {
                Continue
            } else {
                let frmExprBegin = frmExpr->Js_array2.unsafe_get(0)
                if (frmExprBegin < 0 && frmExprBegin != expr->Js_array2.unsafe_get(0)) {
                    Continue
                } else {
                    iterateConstParts(
                        ~frmExpr, 
                        ~expr, 
                        ~frmConstParts, 
                        ~constParts, 
                        ~idxToMatch=0,
                        ~parenCnt,
                        ~consumer = constParts => {
                            if (varGroups->Js.Array2.length > 0) {
                                initVarGroups(~varGroups, ~constParts, ~expr)
                                iterateVarGroups(
                                    ~expr,
                                    ~subs,
                                    ~varGroups,
                                    ~curGrpIdx = 0,
                                    ~curVarIdx = 0,
                                    ~subExprBeginIdx = (varGroups->Array.getUnsafe(0)).exprBeginIdx,
                                    ~parenCnt,
                                    ~consumer
                                )
                            } else {
                                consumer(subs)
                            }
                        }
                    )
                }
            }
        }
    }
}

let createSubs = (~numOfVars:int) => {
    {
        size: numOfVars,
        begins: Belt_Array.make(numOfVars, 0),
        ends: Belt_Array.make(numOfVars, 0),
        exprs: Belt_Array.make(numOfVars, []),
        isDefined: Belt_Array.make(numOfVars, false),
    }
}

let prepareFrmSubsDataForFrame = (frame:frame):frmSubsData => {
    let hypsE = frame.hyps->Js.Array2.filter(hyp => hyp.typ == E)

    let frmConstPartsArr:array<constParts> = []
    let constPartsArr:array<constParts> = []
    let varGroupsArr:array<array<varGroup>> = []

    hypsE->Js_array2.forEach(hyp => {
        let frmConstParts = createConstParts(hyp.expr)
        let constParts = createMatchingConstParts(frmConstParts)
        let varGroups = createVarGroups(~frmExpr=hyp.expr, ~frmConstParts)
        frmConstPartsArr->Array.push(frmConstParts)
        constPartsArr->Array.push(constParts)
        varGroupsArr->Array.push(varGroups)
    })

    let frmConstParts = createConstParts(frame.asrt)
    let constParts = createMatchingConstParts(frmConstParts)
    let varGroups = createVarGroups(~frmExpr=frame.asrt, ~frmConstParts)
    frmConstPartsArr->Array.push(frmConstParts)
    constPartsArr->Array.push(constParts)
    varGroupsArr->Array.push(varGroups)

    let subs = createSubs(~numOfVars=frame.numOfVars)
    {
        frame:frame,
        hypsE,
        numOfHypsE: hypsE->Js.Array2.length,
        frmConstParts:frmConstPartsArr,
        constParts:constPartsArr,
        varGroups:varGroupsArr,
        subs,
    }
}

let prepareFrmSubsData = (
    ~ctx:mmContext,
    ()
):frms => {
    let frmCmp = comparatorBy(frm => frm.hypsE->Js_array2.length)
        ->comparatorAndThen(comparatorBy(frm => frm.frame.ord))
    let all = ctx->getAllFramesArr->Js.Array2.map(prepareFrmSubsDataForFrame)->Expln_utils_common.sortInPlaceWith(frmCmp)
    let byLabel = Belt_HashMapString.make(~hintSize=1000)
    let byType = Belt_HashMapInt.make(~hintSize=16)
    all->Js_array2.forEach(frm => {
        byLabel->Belt_HashMapString.set(frm.frame.label, frm)
        let typ = frm.frame.asrt->Array.getUnsafe(0)
        switch byType->Belt_HashMapInt.get(typ) {
            | None => byType->Belt_HashMapInt.set(typ,[frm])
            | Some(arr) => arr->Array.push(frm)
        }
    })
    {
        all,
        byLabel,
        byType,
    }
}

let sortFrames = (frms:frms, frames:array<string>):array<string> => {
    let framesSet = frames->Belt_HashSetString.fromArray
    frms.all
        ->Js.Array2.filter(frm => framesSet->Belt_HashSetString.has(frm.frame.label))
        ->Js.Array2.map(frm => frm.frame.label)
}

let applySubs = (~frmExpr:expr, ~subs:subs, ~createWorkVar:int=>int): expr => {
    let resultSize = ref(0)
    frmExpr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else if (subs.isDefined->Array.getUnsafe(s)) {
            resultSize.contents = resultSize.contents + (subs.ends->Array.getUnsafe(s)-subs.begins->Array.getUnsafe(s)+1)
        } else {
            resultSize.contents = resultSize.contents + 1
        }
    })
    let res = Expln_utils_common.createArray(resultSize.contents)
    let e = ref(0)
    let r = ref(0)
    while (r.contents < resultSize.contents) {
        let s = frmExpr->Array.getUnsafe(e.contents)
        if (s < 0) {
            res[r.contents] = s
            r.contents = r.contents + 1
        } else if (subs.isDefined->Array.getUnsafe(s)) {
            let subExpr = subs.exprs->Array.getUnsafe(s)
            let len = (subs.ends->Array.getUnsafe(s)-subs.begins->Array.getUnsafe(s)+1)
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=subs.begins->Array.getUnsafe(s), ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        } else {
            res[r.contents] = createWorkVar(s)
            r.contents = r.contents + 1
        }
        e.contents = e.contents + 1
    }
    res
}

let verifyDisjoints = (
    ~frmDisj:Belt_MapInt.t<Belt_SetInt.t>, 
    ~subs:subs, 
    ~isDisjInCtx:(int,int)=>bool,
    ~debugLevel:int,
):option<unifErr> => {
    let res = ref(None)
    frmDisj->Belt_MapInt.forEach((n,ms) => {
        if (res.contents->Belt.Option.isNone) {
            ms->Belt_SetInt.forEach(m => {
                if (res.contents->Belt.Option.isNone) {
                    let nExpr = subs.exprs->Array.getUnsafe(n)
                    let nExprBegin = subs.begins->Array.getUnsafe(n)
                    let nExprEnd = subs.ends->Array.getUnsafe(n)
                    let mExpr = subs.exprs->Array.getUnsafe(m)
                    let mExprBegin = subs.begins->Array.getUnsafe(m)
                    let mExprEnd = subs.ends->Array.getUnsafe(m)
                    for nExprI in nExprBegin to nExprEnd {
                        if (res.contents->Belt.Option.isNone) {
                            let nExprSym = nExpr->Array.getUnsafe(nExprI)
                            if (nExprSym >= 0) {
                                for mExprI in mExprBegin to mExprEnd {
                                    if (res.contents->Belt.Option.isNone) {
                                        let mExprSym = mExpr->Array.getUnsafe(mExprI)
                                        if (mExprSym >= 0) {
                                            if (nExprSym == mExprSym) {
                                                if (debugLevel == 0) {
                                                    res.contents = Some(UnifErr)
                                                } else {
                                                    res.contents = Some(DisjCommonVar({
                                                        frmVar1:n, 
                                                        expr1:nExpr->Js_array2.slice(~start=nExprBegin, ~end_=nExprEnd+1),
                                                        frmVar2:m, 
                                                        expr2:mExpr->Js_array2.slice(~start=mExprBegin, ~end_=mExprEnd+1),
                                                        commonVar:nExprSym,
                                                    }))
                                                }
                                            } else if (!isDisjInCtx(nExprSym, mExprSym)) {
                                                if (debugLevel == 0) {
                                                    res.contents = Some(UnifErr)
                                                } else {
                                                    res.contents = Some(Disj({
                                                        frmVar1:n, 
                                                        expr1:nExpr->Js_array2.slice(~start=nExprBegin, ~end_=nExprEnd+1),
                                                        var1:nExprSym,
                                                        frmVar2:m, 
                                                        expr2:mExpr->Js_array2.slice(~start=mExprBegin, ~end_=mExprEnd+1),
                                                        var2:mExprSym,
                                                    }))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            })
        }
    })
    res.contents
}

let frmsEmpty = ():frms => {
    {
        all: [],
        byType: Belt_HashMapInt.make(~hintSize=0),
        byLabel:Belt_HashMapString.make(~hintSize=0),
    }
}
let frmsSize = frms => frms.all->Js_array2.length
let frmsForEach = (frms:frms, ~typ:option<int>=?, consumer:frmSubsData=>unit):unit => {
    switch typ {
        | None => frms.all->Js_array2.forEach(consumer)
        | Some(typ) => frms.byType->Belt_HashMapInt.get(typ)->Belt.Option.forEach(Js_array2.forEach(_, consumer))
    }
}
let frmsSelect = (frms:frms, ~typ:option<int>=?, ~label:option<string>=?, ()):array<frmSubsData> => {
    switch typ {
        | None => {
            switch label {
                | None => frms.all->Js.Array2.copy
                | Some(label) => {
                    switch frms.byLabel ->Belt_HashMapString.get(label) {
                        | None => []
                        | Some(frm) => [frm]
                    }
                }
            }
        }
        | Some(typ) => {
            switch label {
                | None => {
                    switch frms.byType->Belt_HashMapInt.get(typ) {
                        | None => []
                        | Some(arr) => arr->Js_array2.copy
                    }
                }
                | Some(label) => {
                    switch frms.byLabel->Belt_HashMapString.get(label)->Belt.Option.keep(frm=>frm.frame.asrt->Array.getUnsafe(0)==typ) {
                        | None => []
                        | Some(frm) => [frm]
                    }
                }
            }
        }
    }
}
let frmsGetAll = (frms:frms):array<frmSubsData> => frms.all
let frmsGetByLabel = (frms:frms, label:string):option<frmSubsData> => {
    frms.byLabel->Belt_HashMapString.get(label)
}
let frmsGetByType = (frms:frms, typ:int):option<array<frmSubsData>> => {
    frms.byType->Belt_HashMapInt.get(typ)
}
let frmsGetAllTypes = (frms):array<int> => frms.byType->Belt_HashMapInt.keysToArray
let frmsGetAllGroupedByLabel = (frms:frms):Belt_HashMapString.t<frmSubsData> => frms.byLabel

//------------------------- TEST ---------------------------

let test_iterateConstParts = (~frmExpr:expr, ~expr:expr, ~parenCnt:parenCnt):(array<(int,int)>, array<array<(int,int)>>) => {
    let constPartsToArr = (constParts:constParts) => {
        constParts.begins->Js_array2.mapi((b,i)=>(b,constParts.ends->Array.getUnsafe(i)))
    }
    let frmConstParts = createConstParts(frmExpr)
    let constParts = createMatchingConstParts(frmConstParts)
    let matchingConstParts = []
    iterateConstParts(
        ~frmExpr, 
        ~expr, 
        ~frmConstParts, 
        ~constParts, 
        ~idxToMatch=0,
        ~parenCnt,
        ~consumer = constParts => {
            matchingConstParts->Array.push(
                constPartsToArr(constParts)
            )
            Continue
        }
    )->ignore
    (
        constPartsToArr(frmConstParts),
        matchingConstParts
    )
}

let test_iterateSubstitutions = (~frmExpr:expr, ~expr:expr, ~parenCnt:parenCnt):array<array<expr>> => {
    let frmConstParts = createConstParts(frmExpr)
    let constParts = createMatchingConstParts(frmConstParts)
    let varGroups = createVarGroups(~frmExpr, ~frmConstParts)
    let numOfVars = frmExpr
        ->Js_array2.filter(i => i >= 0)
        ->Belt_SetInt.fromArray
        ->Belt_SetInt.size
    let subs = createSubs(~numOfVars)
    let result = []
    iterateSubstitutions(
        ~frmExpr, 
        ~expr, 
        ~frmConstParts, 
        ~constParts, 
        ~varGroups,
        ~subs,
        ~parenCnt,
        ~consumer = subs => {
            let res = []
            for i in 0 to numOfVars-1 {
                res->Array.push(
                    subs.exprs->Array.getUnsafe(i)->Js_array2.slice(~start=subs.begins->Array.getUnsafe(i), ~end_=subs.ends->Array.getUnsafe(i)+1)
                )
            }
            result->Array.push(res)
            Continue
        }
    )->ignore
    result
}