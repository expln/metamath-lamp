open MM_context
open MM_substitution
open MM_parenCounter
open MM_proof_tree

type hypMatcherType = Label(string) | Idx(int)

type applyAsrtResultMatcher = {
    frm:frmSubsData,
    matchAsrt:bool,
    hypMatchers:array<hypMatcherType>,
}

let getArgToMatchPriv = (
    ~frame:frame,
    ~args:array<proofNode>,
    ~hypLabelToMatch:option<string>,
    ~hypIdxToMatch:option<int>,
):option<expr> => {
    let res = ref(None)
    let i = ref(0)
    let eHypI = ref(0)
    let maxI = frame.hyps->Js_array2.length - 1
    while (i.contents <= maxI && res.contents->Belt_Option.isNone) {
        let hyp = frame.hyps->Array.getUnsafe(i.contents)
        if (hyp.typ == E) {
            if (
                hypLabelToMatch->Belt_Option.map(label => label == hyp.label)->Belt_Option.getWithDefault(false)
                || hypIdxToMatch->Belt_Option.map(idx => idx == eHypI.contents)->Belt_Option.getWithDefault(false)
            ) {
                res := Some(args->Array.getUnsafe(i.contents)->pnGetExpr)
            } else {
                eHypI := eHypI.contents + 1
            }
        }
        i := i.contents + 1
    }
    res.contents
}

let getArgToMatch = (
    ~matcher:applyAsrtResultMatcher,
    ~idx:int,
    ~src:exprSrc,
):option<expr> => {
    switch src {
        | VarType | Hypothesis(_) | AssertionWithErr(_) => None
        | Assertion({args, frame}) => {
            switch matcher.hypMatchers->Array.getUnsafe(idx) {
                | Label(label) => getArgToMatchPriv(~frame, ~args, ~hypLabelToMatch=Some(label), ~hypIdxToMatch=None)
                | Idx(idx) => getArgToMatchPriv(~frame, ~args, ~hypLabelToMatch=None, ~hypIdxToMatch=Some(idx))
            }
        }
    }
}

let rec exprSrcMatchesPriv = (
    ~expr:expr,
    ~src:exprSrc, 
    ~matcher:applyAsrtResultMatcher,
    ~idx:int,
    ~parenCnt:parenCnt,
):bool => {
    if (idx == matcher.frm.numOfHypsE) {
        true
    } else if (idx == -1) {
        let res = ref(false)
        let frm = matcher.frm
        iterateSubstitutions(
            ~frmExpr = frm.frame.asrt,
            ~expr,
            ~frmConstParts = frm.frmConstParts->Array.getUnsafe(frm.numOfHypsE), 
            ~constParts = frm.constParts->Array.getUnsafe(frm.numOfHypsE), 
            ~varGroups = frm.varGroups->Array.getUnsafe(frm.numOfHypsE),
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                let remainingMatches = exprSrcMatchesPriv( ~expr, ~src, ~matcher, ~idx=idx+1, ~parenCnt, )
                if (remainingMatches) {
                    res := true
                    Stop
                } else {
                    Continue
                }
            }
        )->ignore
        res.contents
    } else {
        switch getArgToMatch(~matcher,~idx,~src) {
            | None => false
            | Some(argToMatch) => {
                let res = ref(false)
                let frm = matcher.frm
                iterateSubstitutions(
                    ~frmExpr = frm.hypsE->Array.getUnsafe(idx).expr,
                    ~expr=argToMatch,
                    ~frmConstParts = frm.frmConstParts->Array.getUnsafe(idx), 
                    ~constParts = frm.constParts->Array.getUnsafe(idx), 
                    ~varGroups = frm.varGroups->Array.getUnsafe(idx),
                    ~subs = frm.subs,
                    ~parenCnt,
                    ~consumer = _ => {
                        let remainingMatches = exprSrcMatchesPriv( ~expr, ~src, ~matcher, ~idx=idx+1, ~parenCnt, )
                        if (remainingMatches) {
                            res := true
                            Stop
                        } else {
                            Continue
                        }
                    }
                )->ignore
                res.contents
            }
        }
    }
}

let exprSrcMatches = (
    ~expr:expr,
    ~src:exprSrc, 
    ~matcher:applyAsrtResultMatcher,
    ~parenCnt:parenCnt,
):bool => {
    if (matcher.matchAsrt) {
        exprSrcMatchesPriv( ~expr, ~src, ~matcher, ~idx=-1, ~parenCnt, )
    } else {
        exprSrcMatchesPriv( ~expr, ~src, ~matcher, ~idx=0, ~parenCnt, )
    }
}