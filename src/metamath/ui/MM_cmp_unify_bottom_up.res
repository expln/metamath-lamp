open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_wrk_ctx_data
open MM_wrk_editor
open MM_context
open MM_substitution
open MM_parser
open Expln_React_Modal
open MM_statements_dto
open MM_provers
open MM_proof_tree
open MM_proof_tree_dto
open MM_wrk_unify
open MM_parenCounter
open MM_wrk_settings
open MM_cmp_root_stmts

type sortBy = UnprovedStmtsNum | NumOfNewVars | AsrtLabel

type resultRendered = {
    idx:int,
    elem: reElem,
    isProved: bool,
    asrtLabel:string,
    numOfNewVars: int,
    numOfUnprovedStmts: int,
    numOfStmts: int,
}

type state = {
    rootStmts: array<userStmt>,
    rootStmtsRendered: array<rootStmtRendered>,
    exprToProve:expr,
    title: reElem,
    rootProvables: array<rootStmt>,

    args0: array<bool>,
    args1: array<bool>,
    availableLabels: array<string>,
    label:option<string>,
    depth: int,
    depthStr: string,
    lengthRestrict: lengthRestrict,
    allowNewStmts: bool,
    allowNewVars: bool,
    debug:bool,
    maxNumberOfBranchesStr:string,

    tree: option<proofTreeDto>,
    results: option<array<stmtsDto>>,
    resultsRendered: option<array<resultRendered>>,
    sortBy: sortBy,
    resultsSorted: option<array<resultRendered>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultIdx: option<int>,
}

let getProofStatus = (stmt:rootStmtRendered):option<proofStatus> => {
    if (stmt.isHyp) {
        Some(Ready)
    } else {
        stmt.proofStatus
    }
}

let exprMayMatchAsrt = (
    ~expr:expr,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
):bool => {
    if (expr[0] != frm.frame.asrt[0]) {
        false
    } else {
        let res = ref(false)
        iterateSubstitutions(
            ~frmExpr = frm.frame.asrt,
            ~expr,
            ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
            ~constParts = frm.constParts[frm.numOfHypsE], 
            ~varGroups = frm.varGroups[frm.numOfHypsE],
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                res.contents = true
                Stop
            }
        )->ignore
        res.contents
    }
}

let getAvailableAsrtLabels = (
    ~frms: Belt_MapString.t<frmSubsData>, 
    ~parenCnt: parenCnt, 
    ~exprToProve:expr,
) => {
    let availableAsrtLabels = []
    frms->Belt_MapString.forEach((label,frm) => {
        if ( exprMayMatchAsrt(~expr=exprToProve, ~frm, ~parenCnt) ) {
            availableAsrtLabels->Js_array2.push(label)->ignore
        }
    })
    availableAsrtLabels
}

let makeInitialState = (
    ~rootStmts: array<userStmt>,
    ~rootProvables: array<rootStmt>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~initialLabel: option<string>,
) => {
    let rootStmtsLen = rootStmts->Js_array2.length
    let maxRootStmtIdx = rootStmtsLen-1
    let exprToProve = switch rootStmts[maxRootStmtIdx].expr {
        | None => raise(MmException({msg:`expr must be set on the statement to prove.`}))
        | Some(expr) => expr
    }
    {
        rootStmts,
        rootStmtsRendered: rootStmts->Js_array2.filteri((_,i) => i < maxRootStmtIdx)->Js.Array2.map(stmt => {
            {
                id: stmt.id,
                expr: switch stmt.expr {
                    | None => raise(MmException({msg:`expr must be set on a root statement.`}))
                    | Some(expr) => expr
                },
                isHyp:stmt.typ == E,
                label:stmt.label,
                proofStatus:stmt.proofStatus,
                exprReElem: <span> {MM_cmp_user_stmt.rndContText(stmt.cont)} </span>
            }
        })
        ,
        exprToProve,
        title:
            <span>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {"Proving bottom-up "->React.string}
                </span>
                { MM_cmp_user_stmt.rndContText(rootStmts[maxRootStmtIdx].cont) }
            </span>,
        rootProvables,
        
        args0: Belt_Array.make(rootStmtsLen-1, true),
        args1: Belt_Array.make(rootStmtsLen-1, false),
        availableLabels: getAvailableAsrtLabels( ~frms, ~parenCnt, ~exprToProve, ),
        label: initialLabel,
        depthStr: "4",
        depth: 4,
        lengthRestrict: Less,
        allowNewStmts: true,
        allowNewVars: true,
        debug: false,
        maxNumberOfBranchesStr: "",

        tree: None,
        results: None,
        resultsRendered: None,
        sortBy: UnprovedStmtsNum,
        resultsSorted: None,
        resultsPerPage: 20,
        resultsMaxPage: 0,
        resultsPage: 0,
        checkedResultIdx: None,
    }
}

let setLabel = (st,label) => {
    {
        ...st,
        label
    }
}

let setDepthStr = (st,depthStr) => {
    {
        ...st,
        depthStr
    }
}

let setLengthRestrict = (st,lengthRestrict) => {
    {
        ...st,
        lengthRestrict
    }
}

let toggleAllowNewStmts = (st) => {
    {
        ...st,
        allowNewStmts: !st.allowNewStmts
    }
}

let toggleAllowNewVars = (st) => {
    {
        ...st,
        allowNewVars: !st.allowNewVars
    }
}

let toggleDebug = (st) => {
    {
        ...st,
        debug: !st.debug
    }
}

let nonDigitPattern = %re("/\D/g")

let setMaxNumberOfBranchesStr = (st, maxNumberOfBranchesStr) => {
    {
        ...st,
        maxNumberOfBranchesStr: maxNumberOfBranchesStr->Js.String2.replaceByRe(nonDigitPattern, "")
    }
}

let selectAllArgs = args => args->Js_array2.map(_ => true)
let unselectAllArgs = args => args->Js_array2.map(_ => false)

let updateArgs0 = (st, args0) => { ...st, args0 }
let updateArgs1 = (st, args1) => { ...st, args1 }

let isStmtToShow = (
    ~stmt:stmtDto, 
    ~rootJstfs:Belt_HashMap.t<expr,option<jstf>,ExprHash.identity>
):bool => {
    if (!stmt.isProved) {
        true
    } else {
        stmt.jstf != switch rootJstfs->Belt_HashMap.get(stmt.expr) {
            | None => None
            | Some(jstf) => jstf
        }
    }
}

let stmtsDtoToResultRendered = (
    stmtsDto:stmtsDto, 
    idx:int,
    isStmtToShow:stmtDto=>bool
):resultRendered => {
    let elem = 
        <Col>
            {
                stmtsDto.newDisjStr
                    ->Js.Array2.map(disjStr => {
                        <span key=disjStr>
                            {disjStr->React.string}
                        </span>
                    })
                    ->React.array
            }
            <table>
                <tbody>
                    {
                        let maxI = stmtsDto.stmts->Js.Array2.length - 1
                        stmtsDto.stmts
                            ->Js.Array2.filteri((stmt,i) => i == maxI || isStmtToShow(stmt))
                            ->Js.Array2.map(stmt => {
                                <tr key=stmt.exprStr>
                                    <td>
                                        { React.string(stmt.label) }
                                    </td>
                                    <td style=ReactDOM.Style.make(~textAlign="right", ())>
                                        {
                                            switch stmt.jstf {
                                                | None => React.null
                                                | Some({args, label}) => {
                                                    React.string(
                                                        "[" ++ args->Js_array2.joinWith(" ") ++ " : " ++ label ++ " ]"
                                                    )
                                                }
                                            }
                                        }
                                    </td>
                                    <td style=ReactDOM.Style.make(~color= if (stmt.isProved) {"green"} else {"#565656"},
                                                ~fontWeight="bold", ())>
                                        {React.string( if (stmt.isProved) {"\u2713"} else {"?"} )}
                                    </td>
                                    <td> 
                                        {React.string(stmt.exprStr)}
                                    </td>
                                </tr>
                            })
                            ->React.array
                    }
                </tbody>
            </table>
        </Col>
    {
        idx,
        elem,
        asrtLabel:
            stmtsDto.stmts[stmtsDto.stmts->Js.Array2.length-1].jstf
                ->Belt_Option.map(jstf => jstf.label)
                ->Belt_Option.getWithDefault(""),
        numOfNewVars: stmtsDto.newVars->Js.Array2.length,
        numOfUnprovedStmts: stmtsDto.stmts->Js.Array2.reduce(
            (cnt,stmt) => cnt + if (stmt.isProved) {0} else {1},
            0
        ),
        isProved: stmtsDto.stmts[stmtsDto.stmts->Js.Array2.length-1].isProved,
        numOfStmts: stmtsDto.stmts->Js.Array2.length,
    }
}

let compareByIsProved = Expln_utils_common.comparatorBy((res:resultRendered) => if (res.isProved) {0} else {1})
let compareByNumberOfStmts = Expln_utils_common.comparatorBy(res => res.numOfStmts)

let compareByNumOfUnprovedStmts = Expln_utils_common.comparatorBy(res => res.numOfUnprovedStmts)
let compareByNumOfNewVars = Expln_utils_common.comparatorBy(res => res.numOfNewVars)
let compareByAsrtLabel = (a,b) => a.asrtLabel->Js.String2.localeCompare(b.asrtLabel)->Belt.Float.toInt

let createComparator = (sortBy):Expln_utils_common.comparator<resultRendered> => {
    open Expln_utils_common
    let mainCmp = switch sortBy {
        | UnprovedStmtsNum => compareByNumOfUnprovedStmts
        | NumOfNewVars => compareByNumOfNewVars
        | AsrtLabel => compareByAsrtLabel
    }
    compareByIsProved
        ->comparatorAndThen(mainCmp)
        ->comparatorAndThen(compareByNumberOfStmts)
        ->comparatorAndThen(compareByAsrtLabel)
}

let sortResultsRendered = (resultsRendered, sortBy) => {
    switch resultsRendered {
        | None => None
        | Some(resultsRendered) => {
            Some(resultsRendered->Js_array2.copy->Js_array2.sortInPlaceWith(createComparator(sortBy)))
        }
    }
}

let setResults = (st,tree,results) => {
    switch results {
        | None => {
            {
                ...st,
                tree: None,
                results,
                resultsRendered: None,
                resultsSorted: None,
                resultsMaxPage: 0,
                resultsPage: 0,
                checkedResultIdx: None,
            }
        }
        | Some(results) => {
            let rootJstfs = st.rootProvables
                ->Js_array2.map(stmt => (stmt.expr, stmt.jstf))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let isStmtToShow = stmt => isStmtToShow(~stmt, ~rootJstfs)
            let resultsRendered = Some(results->Js_array2.mapi((dto,i) => stmtsDtoToResultRendered(dto,i,isStmtToShow)))
            {
                ...st,
                tree,
                results:Some(results),
                resultsRendered,
                resultsSorted: sortResultsRendered(resultsRendered, st.sortBy),
                resultsMaxPage: Js.Math.ceil_int(
                    results->Js_array2.length->Belt_Int.toFloat /. st.resultsPerPage->Belt_Int.toFloat
                ),
                resultsPage: 1,
                checkedResultIdx: None,
            }
        }
    }
    
}

let setSortBy = (st,sortBy) => {
    {
        ...st,
        sortBy,
        resultsSorted: sortResultsRendered(st.resultsRendered, sortBy),
        resultsPage: 1,
        checkedResultIdx: None,
    }
}

let setPage = (st,page) => {
    {
        ...st,
        resultsPage: page,
    }
}

let toggleResultChecked = (st,idx) => {
    switch st.checkedResultIdx {
        | None => {
            {
                ...st,
                checkedResultIdx: Some(idx)
            }
        }
        | Some(checkedIdx) => {
            if (checkedIdx != idx) {
                {
                    ...st,
                    checkedResultIdx: Some(idx)
                }
            } else {
                {
                    ...st,
                    checkedResultIdx: None
                }
            }
        }
    }
}

let lengthRestrictToStr = (len:lengthRestrict) => {
    switch len {
        | No => "No"
        | LessEq => "LessEq"
        | Less => "Less"
    }
}
let lengthRestrictFromStr = str => {
    switch str {
        | "No" => No
        | "LessEq" => LessEq
        | "Less" => Less
        | _ => raise(MmException({msg:`Cannot convert '${str}' to lengthRestrict.`}))
    }
}

let sortByToStr = sortBy => {
    switch sortBy {
        | UnprovedStmtsNum => "UnprovedStmtsNum"
        | NumOfNewVars => "NumOfNewVars"
        | AsrtLabel => "AsrtLabel"
    }
}

let sortByFromStr = str => {
    switch str {
        | "UnprovedStmtsNum" => UnprovedStmtsNum
        | "NumOfNewVars" => NumOfNewVars
        | "AsrtLabel" => AsrtLabel
        | _ => raise(MmException({msg:`Cannot convert value of '${str}' to a sortBy.`}))
    }
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~wrkCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~rootProvables: array<rootStmt>,
    ~rootStmts: array<userStmt>,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~initialLabel: option<string>,
    ~onResultSelected:stmtsDto=>unit,
    ~onCancel:unit=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState( 
        ~rootStmts, ~rootProvables, ~frms, ~parenCnt, ~initialLabel
    ))

    let onlyOneResultIsAvailable = switch state.results {
        | None => false
        | Some(results) => results->Js_array2.length == 1
    }

    let actLabelUpdated = label => {
        setState(setLabel(_,label))
    }

    let actDepthUpdated = depthStr => {
        setState(setDepthStr(_,depthStr->Js.String2.replaceByRe(nonDigitPattern, "")))
    }

    let actLengthRestrictUpdated = lengthRestrict => {
        setState(setLengthRestrict(_,lengthRestrict))
    }

    let actToggleAllowNewStmts = () => {
        setState(toggleAllowNewStmts)
    }

    let actToggleAllowNewVars = () => {
        setState(toggleAllowNewVars)
    }

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let actOnResultsReady = (treeDto) => {
        let results = proofTreeDtoToNewStmtsDto(
            ~treeDto, 
            ~rootStmts = rootStmts->Js_array2.map(userStmtToRootStmt),
            ~ctx = wrkCtx,
            ~typeToPrefix,
        )
        setState(st => setResults(st, if (st.debug) {Some(treeDto)} else {None}, Some(results)))
    }

    let actProve = () => {
        setState(st => {
            let depthStr = st.depthStr->Js_string2.trim
            let st = if (depthStr->Js_string2.length == 0) {
                {...st, depth:1, depthStr:"1"}
            } else {
                let depth = Js.Math.max_int(depthStr->Belt_Int.fromString->Belt_Option.getWithDefault(1), 1)
                let depthStr = depth->Belt_Int.toString
                {...st, depth, depthStr}
            }

            openModal(modalRef, () => rndProgress(~text="Proving bottom-up", ~pct=0., ()))->promiseMap(modalId => {
                updateModal( 
                    modalRef, modalId, () => rndProgress(
                        ~text="Proving bottom-up", ~pct=0., ~onTerminate=makeActTerminate(modalId), ()
                    )
                )
                unify(
                    ~settingsVer, ~settings, ~preCtxVer, ~preCtx, ~varsText, ~disjText, ~hyps, ~rootProvables,
                    ~bottomUpProverParams=Some({
                        asrtLabel: st.label,
                        maxSearchDepth: st.depth,
                        lengthRestriction: st.lengthRestrict,
                        allowNewStmts: st.allowNewStmts,
                        allowNewVars: st.allowNewVars,
                        args0: 
                            st.rootStmtsRendered
                                ->Js_array2.filteri((_,i) => st.args0[i])
                                ->Js_array2.map(stmt => stmt.expr),
                        args1:
                            st.rootStmtsRendered
                                ->Js_array2.filteri((_,i) => st.args1[i])
                                ->Js_array2.map(stmt => stmt.expr),
                        maxNumberOfBranches: 
                            if (!state.debug || state.maxNumberOfBranchesStr == "") {
                                None
                            } else {
                                state.maxNumberOfBranchesStr->Belt_Int.fromString
                            },
                    }),
                    ~debugLevel = if (st.debug) {1} else {0},
                    ~onProgress = msg => updateModal( 
                        modalRef, modalId, () => rndProgress(
                            ~text=msg, ~onTerminate=makeActTerminate(modalId), ()
                        )
                    )
                )->promiseMap(proofTreeDto => {
                    closeModal(modalRef, modalId)
                    actOnResultsReady(proofTreeDto)
                })
            })->ignore

            st
        })
    }

    let actSortByChange = sortBy => {
        setState(setSortBy(_, sortBy))
    }

    let actPageChange = page => {
        setState(setPage(_, page))
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = () => {
        switch state.results {
            | None => ()
            | Some(results) => {
                if (onlyOneResultIsAvailable) {
                    onResultSelected(results[0])
                } else {
                    switch state.checkedResultIdx {
                        | None => ()
                        | Some(checkedResultIdx) => onResultSelected(results[checkedResultIdx])
                    }
                }
            }
        }
    }

    let actShowProofTree = () => {
        switch state.tree {
            | None => ()
            | Some(tree) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <Col spacing=1. style=ReactDOM.Style.make(~margin="5px", ())>
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                                {React.string("Close")}
                            </Button>
                            <MM_cmp_proof_tree
                                tree
                                rootExpr=state.exprToProve
                                wrkCtx
                                rootStmts={rootStmts->Js_array2.map(userStmtToRootStmt)}
                            />
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                                {React.string("Close")}
                            </Button>
                        </Col>
                    })
                })->ignore
            }
        }
    }

    let actToggleDebug = () => {
        setState(toggleDebug)
    }

    let actMaxNumberOfBranchesStrUpdated = maxNumberOfBranchesStr => {
        setState(setMaxNumberOfBranchesStr(_, maxNumberOfBranchesStr))
    }

    let rndLengthRestrictSelector = (value:lengthRestrict) => {
        <FormControl size=#small>
            <InputLabel id="length-restrict-select-label">"Length restriction"</InputLabel>
            <Select
                sx={"width": 130}
                labelId="length-restrict-select-label"
                value={lengthRestrictToStr(value)}
                label="Length restriction"
                onChange=evt2str(str => actLengthRestrictUpdated(lengthRestrictFromStr(str)))
            >
                <MenuItem value="No">{React.string("No")}</MenuItem>
                <MenuItem value="LessEq">{React.string("LessEq")}</MenuItem>
                <MenuItem value="Less">{React.string("Less")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndSortBySelector = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                if (results->Js_array2.length < 2) {
                    React.null
                } else {
                    <FormControl size=#small>
                        <InputLabel id="sortBy-select-label">"Sort results by"</InputLabel>
                        <Select 
                            labelId="sortBy-select-label"
                            value=sortByToStr(state.sortBy)
                            label="Sort results by"
                            onChange=evt2str(str => actSortByChange(sortByFromStr(str)))
                        >
                            <MenuItem value="UnprovedStmtsNum">{React.string("Number of unproved statements")}</MenuItem>
                            <MenuItem value="NumOfNewVars">{React.string("Number of new variables")}</MenuItem>
                            <MenuItem value="AsrtLabel">{React.string("Assertion label")}</MenuItem>
                        </Select>
                    </FormControl>
                }
            }
        }
    }

    let rndParams = () => {
        if (state.availableLabels->Js.Array2.length == 0) {
            <Col>
                {
                    React.string("The statement to prove doesn't match any existing assertion. " 
                        ++ "Bottom-up proving is not available for such statements.")
                }
                <Button onClick={_=>onCancel()}> {React.string("Ok")} </Button>
            </Col>
        } else {
            <Col>
                <Row>
                    <AutocompleteVirtualized value=state.label options=state.availableLabels size=#small width=200
                        onChange=actLabelUpdated
                    />
                    <TextField 
                        label="Search depth"
                        size=#small
                        style=ReactDOM.Style.make(~width="100px", ())
                        autoFocus=true
                        value=state.depthStr
                        onChange=evt2str(actDepthUpdated)
                    />
                    {rndLengthRestrictSelector(state.lengthRestrict)}
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewStmts
                                onChange={_ => actToggleAllowNewStmts()}
                            />
                        }
                        label="Allow new statements"
                        style=ReactDOM.Style.make(
                            ~border="solid 1px lightgrey", 
                            ~borderRadius="7px", 
                            ~paddingRight="10px",
                            ~marginTop="-2px",
                            ~marginLeft="2px",
                            ()
                        )
                    />
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewVars
                                onChange={_ => actToggleAllowNewVars()}
                            />
                        }
                        label="Allow new variables"
                        disabled={!state.allowNewStmts}
                        style=ReactDOM.Style.make(
                            ~border="solid 1px lightgrey", 
                            ~borderRadius="7px", 
                            ~paddingRight="10px",
                            ~marginTop="-2px",
                            ()
                        )
                    />
                </Row>
                <Row>
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.debug
                                onChange={_ => actToggleDebug()}
                            />
                        }
                        label="Debug"
                    />
                    <TextField 
                        label="Max num of branches"
                        size=#small
                        disabled={!state.debug}
                        style=ReactDOM.Style.make(~width="200px", ())
                        value=state.maxNumberOfBranchesStr
                        onChange=evt2str(actMaxNumberOfBranchesStrUpdated)
                    />
                </Row>
                <Row>
                    <Button onClick={_=>actProve()} variant=#outlined >
                        {React.string("Prove")}
                    </Button>
                    <Button onClick={_=>onCancel()}> {React.string("Cancel")} </Button>
                </Row>
            </Col>
        }
    }

    let rndPagination = totalNumOfResults => {
        if (state.resultsPerPage < totalNumOfResults) {
            <Pagination count=state.resultsMaxPage page=state.resultsPage onChange={(_,newPage) => actPageChange(newPage)} />
        } else {
            React.null
        }
    }

    let rndApplyButton = () => {
        <Button onClick={_=>actChooseSelected()} variant=#contained 
                disabled={!onlyOneResultIsAvailable && state.checkedResultIdx->Belt.Option.isNone}>
            {React.string(if onlyOneResultIsAvailable {"Apply"} else {"Apply selected"})}
        </Button>
    }

    let rndShowProofTreeBtn = () => {
        switch state.tree {
            | None => React.null
            | Some(_) => {
                <Button onClick={_=>actShowProofTree()} variant=#outlined>
                    {React.string("Show proof tree")}
                </Button>
            }
        }
    }

    let rndResults = () => {
        switch state.resultsSorted {
            | None => React.null
            | Some(resultsSorted) => {
                let totalNumOfResults = resultsSorted->Js.Array2.length
                if (totalNumOfResults == 0) {
                    <Row>
                        <Divider/>
                        {React.string("Nothing found.")}
                        {rndShowProofTreeBtn()}
                    </Row>
                } else {
                    let start = (state.resultsPage - 1) * state.resultsPerPage
                    let items = resultsSorted->Js_array2.slice( ~start, ~end_ = start + state.resultsPerPage )
                    <Col>
                        <Divider/>
                        <Row alignItems=#center>
                            {rndApplyButton()}
                            {rndSortBySelector()}
                            {rndPagination(totalNumOfResults)}
                            {rndShowProofTreeBtn()}
                        </Row>
                        {
                            items->Js_array2.map(item => {
                                <table key={item.idx->Belt_Int.toString}>
                                    <tbody>
                                        <tr>
                                            {
                                                if (onlyOneResultIsAvailable) {
                                                    React.null
                                                } else {
                                                    <td>
                                                        <Checkbox
                                                            checked={
                                                                state.checkedResultIdx
                                                                    ->Belt_Option.map(idx => idx == item.idx)
                                                                    ->Belt.Option.getWithDefault(false)
                                                            }
                                                            onChange={_ => actToggleResultChecked(item.idx)}
                                                        />
                                                    </td>
                                                }
                                            }
                                            <td>
                                                <Paper>
                                                    item.elem
                                                </Paper>
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            })->React.array
                        }
                        {rndPagination(totalNumOfResults)}
                        {rndApplyButton()}
                    </Col>
                }
            }
        }
    }

    let rndTitle = () => {
        state.title
    }

    let actOpenRootStmtsDialog = (
        ~title:string,
        ~getFlags: state => array<bool>,
        ~setFlags: array<bool> => unit,
    ) => {

        let proofStatusesAreAvailable = state.rootStmtsRendered->Js.Array2.every(stmt => 
            getProofStatus(stmt)->Belt_Option.isSome
        )

        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_root_stmts 
                    title
                    rootStmtsRendered=state.rootStmtsRendered
                    proofStatusesAreAvailable
                    flags=getFlags(state)
                    onClose={newFlags => {
                        setFlags(newFlags)
                        closeModal(modalRef, modalId)
                    }}
                />
            })
        })->ignore
    }

    let rndRootStmtsForLevelShort = (
        ~title: string, 
        ~dialogTitle: string,
        ~getFlags: state => array<bool>,
        ~setFlags: array<bool> => unit,
    ) => {
        let flags = state->getFlags
        let allSelected = flags->Js_array2.every(b => b)
        let noneSelected = flags->Js_array2.every(b => !b)
        let numberOfSelected = if (allSelected) {
            "All"
        } else if (noneSelected) {
            "None"
        } else {
            let numSelected = flags->Js_array2.reduce((cnt,b) => if (b) {cnt+1} else {cnt}, 0)
            let numAll = flags->Js_array2.length
            numSelected->Belt_Int.toString ++ "/" ++ numAll->Belt_Int.toString
        }
        let (selectUnselectText, selectUnselectAct) = if (noneSelected) {
            ("select all", () => setFlags(flags->selectAllArgs))
        } else {
            ("select none", () => setFlags(flags->unselectAllArgs))
        }
        <Row style=ReactDOM.Style.make(~border="solid lightgrey 1px", ~borderRadius="6px", ~margin="2px", ())>
            {React.string(title)}
            <span
                onClick={_=> { actOpenRootStmtsDialog( ~title = dialogTitle, ~getFlags, ~setFlags, ) }}
                style=ReactDOM.Style.make(~cursor="pointer", ~color="blue", ())
            >
                {React.string(numberOfSelected)}
            </span>
            <span
                onClick={_=> selectUnselectAct() }
                style=ReactDOM.Style.make(
                    ~cursor="pointer", 
                    ~border="solid lightgrey 0px", ~borderRadius="10px", ~backgroundColor="rgb(240, 240, 240)", 
                    ~paddingLeft="5px", ~paddingRight="5px", 
                    ()
                )
            >
                {React.string(selectUnselectText)}
            </span>
        </Row>
    }

    let rndRootStmts = () => {
        if (state.rootStmtsRendered->Js_array2.length == 0) {
            React.null
        } else {
            <Row alignItems=#center>
                {React.string("Root statements: ")}
                {
                    rndRootStmtsForLevelShort(
                        ~title = "first level", 
                        ~dialogTitle = "Select statements to derive from on level 0", 
                        ~getFlags = state => state.args0,
                        ~setFlags = newFlags => setState(updateArgs0(_, newFlags)),
                    )
                }
                {
                    rndRootStmtsForLevelShort(
                        ~title = "other levels", 
                        ~dialogTitle = "Select statements to derive from on other levels", 
                        ~getFlags = state => state.args1,
                        ~setFlags = newFlags => setState(updateArgs1(_, newFlags)),
                    )
                }
            </Row>
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.5>
            {rndTitle()}
            {rndRootStmts()}
            {rndParams()}
            {rndResults()}
        </Col>
    </Paper>
}