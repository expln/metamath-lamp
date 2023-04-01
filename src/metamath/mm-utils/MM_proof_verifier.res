open MM_parser
open MM_context

type proofNodeDbg = {
    exprStr: string,
}

type rec proofNode =
    | Hypothesis({hypLabel:string, expr:expr, dbg:option<proofNodeDbg>})
    | Calculated({args:array<proofNode>, asrtLabel:string, expr:expr, height:int, dbg:option<proofNodeDbg>})

let proofNodeGetHeight = (node:proofNode):int => {
    switch node {
        | Hypothesis(_) => 0
        | Calculated({height}) => height
    }
}

let getExprFromNode = (node:proofNode):expr => {
    switch node {
        | Hypothesis({expr}) | Calculated({expr}) => expr
    }
}
let getExprFromStack = (stack:array<proofNode>, i:int):expr => getExprFromNode(stack[i])

let compareSubArrays = (~src:array<'t>, ~srcFromIdx:int, ~dst:array<'t>, ~dstFromIdx:int, ~len:int): bool => {
    let s = ref(srcFromIdx)
    let d = ref(dstFromIdx)
    let srcLen = src->Js_array2.length
    let dstLen = dst->Js_array2.length
    if (srcLen < srcFromIdx+len || dstLen < dstFromIdx+len) {
        false
    } else {
        let sMax = srcFromIdx+len-1
        while (s.contents <= sMax && src[s.contents] == dst[d.contents]) {
            d.contents = d.contents + 1
            s.contents = s.contents + 1
        }
        s.contents > sMax
    }
}

let compareExprAfterSubstitution = (expr:expr, subs, eqTo:expr): bool => {
    let e = ref(0)
    let t = ref(0)
    let eq = ref(true)
    let eLen = expr->Js_array2.length
    let tLen = eqTo->Js_array2.length
    while (eq.contents && e.contents < eLen && t.contents < tLen) {
        let s = expr[e.contents]
        if (s < 0) {
            eq.contents = s == eqTo[t.contents]
            t.contents = t.contents + 1
        } else {
            let subExpr = subs[s]
            let len = subExpr->Js_array2.length-1
            eq.contents = compareSubArrays(~src=subExpr, ~srcFromIdx=1, ~dst=eqTo, ~dstFromIdx=t.contents, ~len)
            t.contents = t.contents + len
        }
        e.contents = e.contents + 1
    }
    eq.contents && e.contents == eLen && t.contents == tLen
}

let applySubs = (expr, subs): expr => {
    let resultSize = ref(0)
    expr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            resultSize.contents = resultSize.contents + (subs[s]->Js_array2.length) - 1
        }
    })
    let res = Expln_utils_common.createArray(resultSize.contents)
    let e = ref(0)
    let r = ref(0)
    while (r.contents < resultSize.contents) {
        let s = expr[e.contents]
        if (s < 0) {
            res[r.contents] = s
            r.contents = r.contents + 1
        } else {
            let subExpr = subs[s]
            let len = subExpr->Js_array2.length-1
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=1, ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let extractSubstitution = (stack:array<proofNode>, stackLength, frame):array<expr> => {
    let subs = Expln_utils_common.createArray(frame.numOfVars)
    let subsLock = Belt_Array.make(frame.numOfVars, false)
    let baseIdx = stackLength - frame.numOfArgs
    frame.hyps->Js_array2.forEachi((hyp,i) => {
        if (hyp.typ == F) {
            let t = hyp.expr[0]
            let v = hyp.expr[1]
            if (subsLock[v]) {
                raise(MmException({msg:`subsLock[v]`}))
            } else {
                let subsExpr = stack->getExprFromStack(baseIdx+i)
                if (subsExpr->Js_array2.length < 2) {
                    raise(MmException({msg:`subsExpr->Js_array2.length < 2`}))
                } else if (subsExpr[0] != t) {
                    raise(MmException({msg:`subsExpr[0] != t`}))
                } else {
                    subsLock[v] = true
                    subs[v] = subsExpr
                }
            }
        }
    })
    if (subsLock->Js_array2.some(lock => !lock)) {
        raise(MmException({msg:`subsLock->Js_array2.some(lock => !lock)`}))
    } else {
        subs
    }
}

let validateTopOfStackMatchesFrame = (stack:array<proofNode>, stackLength, frame, subs:array<expr>):unit => {
    let baseIdx = stackLength - frame.numOfArgs
    frame.hyps->Js_array2.forEachi((hyp,i) => {
        if (hyp.typ == E && !compareExprAfterSubstitution(hyp.expr, subs, stack->getExprFromStack(baseIdx+i))) {
            raise(MmException({msg:`!compareExprAfterSubstitution(ess, subs, stack->getExprFromStack(baseIdx+i))`}))
        }
    })
}

let charCode = (str:string,pos:int):int => str->Js.String2.codePointAt(pos)->Belt_Option.getExn
let charToInt = ch => charCode(ch, 0)
let zCode = charToInt("Z")
let aCode = charToInt("A")
let aCodePrev = aCode-1
let tCode = charToInt("T")
let uCode = charToInt("U")
let uCodePrev = uCode-1

let compressedProofBlockToArray = (str:string):array<string> => {
    let len = str->Js_string2.length
    let res = []
    let b = ref(0)
    let e = ref(0)
    while (e.contents < len) {
        let c = charCode(str,e.contents)
        if (c == zCode) {
            res->Js_array2.push("Z")->ignore
            e.contents=e.contents+1
            b.contents=e.contents
        } else if (aCode <= c && c <= tCode) {
            res->Js_array2.push(str->Js_string2.substring(~from=b.contents, ~to_=e.contents+1))->ignore
            e.contents=e.contents+1
            b.contents=e.contents
        } else {
            e.contents=e.contents+1
        }
    }
    res
}

let compressedProofCharCodeToInt = (code:int):int => 
    if (code <= tCode) { code - aCodePrev } else { code - uCodePrev }

let compressedProofStrToInt = (str:string):int => {
    let res = ref(0)
    let base = ref(1)
    let len = str->Js_string2.length
    for i in len-2 downto 0 {
        res.contents = res.contents + base.contents*compressedProofCharCodeToInt(charCode(str,i))
        base.contents = base.contents*5
    }
    res.contents*20 + compressedProofCharCodeToInt(charCode(str, len-1))
}

let intToCompressedProofStr: int => string = i => {
    if (i < 1) {
        raise(MmException({msg:`intToCompressedProofStr: i < 1`}))
    } else {
        let res = [Js_string2.fromCharCode(aCode + mod(i-1,20))]
        let i = ref((i-1)/20)
        while (i.contents > 0) {
            res->Js_array2.push(Js_string2.fromCharCode(uCode + mod(i.contents-1, 5)))->ignore
            i.contents = (i.contents-1) / 5
        }
        res->Js_array2.reverseInPlace->Js_array2.joinWith("")
    }
}

let applyAsrt = (stack:array<proofNode>, frame, ctx):unit => {
    let stackLength = stack->Js_array2.length
    if (stackLength < frame.numOfArgs) {
        raise(MmException({msg:`stackLength < numOfArgs`}))
    } else {
        let subs = extractSubstitution(stack, stackLength, frame)
        validateTopOfStackMatchesFrame(stack, stackLength, frame, subs)
        let args = stack->Js_array2.sliceFrom(stackLength - frame.numOfArgs)
        let expr = applySubs(frame.asrt, subs)
        let newNode = Calculated({
            asrtLabel: frame.label,
            args,
            expr,
            height: args->Js.Array2.map(proofNodeGetHeight)->Js.Array2.reduce(Js_math.max_int, 0),
            dbg:
                if (ctx->isDebug) {
                    Some({ exprStr: ctx->ctxIntsToStrExn(expr) })
                } else {
                    None
                }
        })
        for _ in 1 to frame.numOfArgs {
            stack->Js_array2.pop->ignore
        }
        stack->Js_array2.push(newNode)->ignore
    }
}

let applyUncompressedProof = (ctx, stack, proofLabels) => {
    proofLabels->Js_array2.forEach(step => {
        switch ctx->getHypothesis(step) {
            | Some(hyp) => {
                stack->Js_array2.push(Hypothesis({
                    hypLabel:hyp.label, 
                    expr:hyp.expr,
                    dbg:
                        if (ctx->isDebug) {
                            Some({ exprStr: ctx->ctxIntsToStrExn(hyp.expr) })
                        } else {
                            None
                        }
                }))->ignore
            }
            | None => {
                switch ctx->getFrame(step) {
                    | Some(frame) => applyAsrt(stack, frame, ctx)
                    | None => raise(MmException({msg:`The proof step '${step}' doesn't refer to a hypothesis or assertion (in uncompressed proof).`}))
                }
            }
        }
    })
}

let applyCompressedProof = (ctx, expr, stack, labels, compressedProofBlock):unit => {
    let pushHypToStack = (hyp:hypothesis) => {
        stack->Js_array2.push(Hypothesis({
            hypLabel:hyp.label, 
            expr:hyp.expr,
            dbg:
                if (ctx->isDebug) {
                    Some({ exprStr: ctx->ctxIntsToStrExn(hyp.expr) })
                } else {
                    None
                }
        }))->ignore
    }

    let steps = compressedProofBlockToArray(compressedProofBlock)
    let hyps = getMandHyps(ctx, expr)
    let hypLen = hyps->Js_array2.length
    let hypLenPlusLabelsLen = hypLen + labels->Js_array2.length
    let savedNodes = []
    steps->Belt_Array.forEach(step => {
        if (step == "Z") {
            let stackLen = stack->Js_array2.length
            if (stackLen == 0) {
                raise(MmException({msg:`Cannot execute 'Z' command because the stack is empty.`}))
            } else {
                savedNodes->Js_array2.push(stack[stackLen-1])->ignore
            }
        } else {
            let i = compressedProofStrToInt(step)
            if (i < 1) {
                raise(MmException({msg:`Unexpected condition when applying compressed proof: i < 1.`}))
            } else if (i <= hypLen) {
                pushHypToStack(hyps[i-1])
            } else if (i <= hypLenPlusLabelsLen) {
                let labelToApply = labels[i-hypLen-1]
                switch ctx->getHypothesis(labelToApply) {
                    | Some(hyp) => pushHypToStack(hyp)
                    | None => {
                        switch ctx->getFrame(labelToApply) {
                            | Some(frame) => applyAsrt(stack, frame, ctx)
                            | None => raise(MmException({msg:`The proof step '${labelToApply}' doesn't refer to a hypothesis or assertion (in compressed proof).`}))
                        }
                    }
                }
            } else {
                switch savedNodes->Belt_Array.get(i-hypLenPlusLabelsLen-1) {
                    | None => raise(MmException({msg:`Compressed proof refers to a saved step by the index which is out of bounds.`}))
                    | Some(node) => stack->Js_array2.push(node)->ignore
                }
            }
        }
    })
}

let verifyProof = (ctx:mmContext, expr:expr, proof:proof):proofNode => {
    let stack = []
    switch proof {
        | Compressed({labels, compressedProofBlock}) => applyCompressedProof(ctx, expr, stack, labels, compressedProofBlock)
        | Uncompressed({labels}) => applyUncompressedProof(ctx, stack, labels)
    }
    if (stack->Js_array2.length != 1) {
        raise(MmException({msg:`stack->Js_array2.length is ${stack->Js_array2.length->Belt_Int.toString} but must be 1.`}))
    } else if (stack->getExprFromStack(0) != expr) {
        raise(MmException({msg:
            `stack[0] != expr` 
                ++ `\nstack[0] is '${stack->getExprFromStack(0)->ctxIntsToStrExn(ctx,_)}'`
                ++ `\nexpr is     '${expr->ctxIntsToStrExn(ctx,_)}'`
        }))
    } else {
        stack[0]
    }
}


