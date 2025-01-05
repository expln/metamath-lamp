open MM_context
open MM_parenCounter
open MM_proof_tree
open MM_apply_asrt_matcher_type

let exprSrcMatches: (
    ~expr: expr,
    ~src: exprSrc,
    ~matcher: applyAsrtResultMatcher,
    ~parenCnt: parenCnt,
) => bool
