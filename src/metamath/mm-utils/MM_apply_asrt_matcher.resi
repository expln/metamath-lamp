open MM_context
open MM_substitution
open MM_parenCounter
open MM_proof_tree

type hypMatcherType = Label(string) | Idx(int)

type applyAsrtResultMatcher = {
    frm: frmSubsData,
    matchAsrt: bool,
    hypMatchers: array<hypMatcherType>,
}

let exprSrcMatches: (
    ~expr: expr,
    ~src: exprSrc,
    ~matcher: applyAsrtResultMatcher,
    ~parenCnt: parenCnt,
) => bool
