open MM_proof_tree_dto
open MM_proof_tree
open MM_context
open MM_wrk_settings
open Common
open Expln_React_Mui
open Expln_React_common
open Local_storage_utils

type state = {
    ctxMaxVar: int,
    exprToLabel: Belt_HashMap.t<expr,string,ExprHash.identity>,
}

let makeInitialState = (~wrkCtx:mmContext, ~rootStmts: array<rootStmt>):state => {
    {
        ctxMaxVar: wrkCtx->getNumOfVars - 1,
        exprToLabel: Belt_HashMap.fromArray(
            rootStmts->Array.map(stmt => (stmt.expr, stmt.label)), 
            ~id=module(ExprHash)
        ),
    }
}

@react.component
let make = (
    ~tree: proofTreeDto,
    ~rootExpr: expr,
    ~settings:settings,
    ~wrkCtx:mmContext,
    ~rootStmts: array<rootStmt>,
) => {
    let (state, _) = React.useState(() => makeInitialState(~wrkCtx, ~rootStmts))
    let (showUnprovedOnly, setShowUnprovedOnly) = useStateFromLocalStorageBool(
        ~key="bottom-up-proof-tree-showUnprovedOnly", ~default=false,
    )

    let actSetShowUnprovedOnly = (newShowUnprovedOnly:bool):unit => {
        setShowUnprovedOnly(_ => newShowUnprovedOnly)
    }

    let nodeIdxToLabel = idx => {
        switch state.exprToLabel->Belt_HashMap.get((tree.nodes->Array.getUnsafe(idx)).expr) {
            | Some(label) => label
            | None => idx->Belt_Int.toString
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
        expr->Array.map(intToSym)->Array.joinUnsafe(" ")
    }

    let exprToReElem = expr => {
        <span> {exprToStr(expr)->React.string} </span>
    }

    let frmExprToStr = (asrtLabel,expr) => {
        switch wrkCtx->getFrame(asrtLabel) {
            | None => raise(MmException({msg:`Cannot find an assertion by label '${asrtLabel}'`}))
            | Some(frame) => wrkCtx->frmIntsToStrExn(frame, expr)
        }
    }

    let isRootStmt = idx => state.exprToLabel->Belt_HashMap.has((tree.nodes->Array.getUnsafe(idx)).expr)

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch wrkCtx->getFrame(label) {
            | None => None
            | Some(frame) => MM_react_common.getFrmLabelBkgColor(frame, settings)
        }
    }

    let rndRootExpr = () => {
        switch tree.nodes->Array.findIndex(node => node.expr->exprEq(rootExpr)) {
            | -1 => React.string(`The proof tree doesn't contain expression [${exprToStr(rootExpr)}]`)
            | nodeIdx => {
                <MM_cmp_proof_node 
                    tree
                    nodeIdx
                    isRootStmt
                    nodeIdxToLabel
                    exprToStr
                    exprToReElem
                    frmExprToStr
                    getFrmLabelBkgColor
                    showUnprovedOnly
                />
            }
        }
    }

    let rndControls = () => {
        <Row>
            <FormControlLabel
                control={
                    <Checkbox
                        checked=showUnprovedOnly
                        onChange=evt2bool(actSetShowUnprovedOnly)
                    />
                }
                label="Show unproved only"
                style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () )
            />
        </Row>
    }

    <Col>
        {rndControls()}
        {rndRootExpr()}
    </Col>

}