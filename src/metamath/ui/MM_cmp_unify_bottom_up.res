open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open MM_parser
open Expln_React_Modal
open MM_statements_dto
open MM_provers
open MM_proof_tree

type resultRendered = {
    elem: reElem,
    numOfNewVars: int,
    numOfUnprovedStmts: int,
}

type state = {
    availableLabels: array<string>,
    label:option<string>,
    depth: int,
    lengthRestrict: lengthRestrict,

    results: option<array<newStmtsDto>>,
    resultsRendered: option<array<resultRendered>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultIdx: option<int>,
}

let makeInititalState = (~preCtx) => {
    {
        availableLabels: preCtx->getAllFrames->Belt_MapString.keysToArray,
        label: None,
        depth: 3,
        lengthRestrict: No,

        results: None,
        resultsRendered: None,
        resultsPerPage: 20,
        resultsMaxPage: 0,
        resultsPage: 0,
        checkedResultIdx: None,
    }
}

@react.component
let make = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~framesToSkip:array<string>,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~stmts: array<rootStmt>,
) => {
    let (state, setState) = React.useState(() => makeInititalState(~preCtx))

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        {"Unifying bottom up ..."->React.string}
    </Paper>
}