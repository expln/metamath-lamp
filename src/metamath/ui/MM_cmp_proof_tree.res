open MM_proof_tree_dto
open MM_proof_tree
open MM_context

type state = {
    ctxMaxVar: int,
    exprToLabel: Belt_HashMap.t<expr,string,ExprHash.identity>
}

let makeInitialState = (~wrkCtx:mmContext, ~rootStmts: array<rootStmt>,) => {
    {
        ctxMaxVar: wrkCtx->getNumOfVars - 1,
        exprToLabel: Belt_HashMap.fromArray(
            rootStmts->Js_array2.map(stmt => (stmt.expr, stmt.label)), 
            ~id=module(ExprHash)
        ),
    }
}

@react.component
let rec make = (
    ~tree: proofTreeDto,
    ~rootExpr: expr,
    ~wrkCtx:mmContext,
    ~rootStmts: array<rootStmt>,
) => {
    let (state, setState) = React.useState(() => makeInitialState(~wrkCtx, ~rootStmts))

    let nodeIdxToLabel = idx => {
        switch state.exprToLabel->Belt_HashMap.get(tree.nodes[idx].expr) {
            | Some(label) => label
            | None => "@" ++ idx->Belt_Int.toString
        }
    }

    let intToSym = i => {
        if (i <= state.ctxMaxVar) {
            wrkCtx->ctxIntToSymExn(i)
        } else {
            "&" ++ i->Belt.Int.toString
        }
    }

    let exprToStr = expr => {
        expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" ")
    }

    let exprToReElem = expr => {
        <span> {exprToStr(expr)->React.string} </span>
    }

    switch tree.nodes->Js.Array2.findIndex(node => node.expr->exprEq(rootExpr)) {
        | -1 => React.string(`The proof tree doesn't contain expression [${exprToStr(rootExpr)}]`)
        | nodeIdx => {
            <MM_cmp_proof_node 
                tree
                nodeIdx
                nodeIdxToLabel
                exprToReElem
            />
        }
    }
}