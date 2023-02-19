open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_wrk_ctx
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

type sortBy = UnprovedStmtsNum | NumOfNewVars | AsrtLabel

type resultRendered = {
    idx:int,
    elem: reElem,
    isProved: bool,
    asrtLabel:string,
    numOfNewVars: int,
    numOfNewUnprovedStmts: int,
    numOfStmts: int,
}

type state = {
    exprToProve:expr,
    title: reElem,

    availableLabels: array<string>,
    label:option<string>,
    depth: int,
    depthStr: string,
    lengthRestrict: lengthRestrict,
    allowNewVars: bool,
    useRootStmtsAsArgs: bool,

    tree: option<proofTreeDto>,
    results: option<array<newStmtsDto>>,
    resultsRendered: option<array<resultRendered>>,
    sortBy: sortBy,
    resultsSorted: option<array<resultRendered>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultIdx: option<int>,
}

let stmtMayMatchAsrt = (
    ~stmt:expr,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
):bool => {
    if (stmt[0] != frm.frame.asrt[0]) {
        false
    } else {
        let res = ref(false)
        iterateSubstitutions(
            ~frmExpr = frm.frame.asrt,
            ~expr = stmt,
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
    ~stmtToProve:expr,
) => {
    let availableAsrtLabels = []
    frms->Belt_MapString.forEach((label,frm) => {
        if ( stmtMayMatchAsrt(~stmt=stmtToProve, ~frm, ~parenCnt) ) {
            availableAsrtLabels->Js_array2.push(label)->ignore
        }
    })
    availableAsrtLabels
}

let makeInitialState = (
    ~userStmtToProve:userStmt,
    ~stmts: array<rootStmt>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
) => {
    let stmtToProve = stmts[stmts->Js_array2.length-1].expr
    {
        exprToProve: stmtToProve,
        title:
            <span>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {"Proving bottom-up "->React.string}
                </span>
                { MM_cmp_user_stmt.rndContText(userStmtToProve.cont) }
            </span>,
        
        availableLabels: getAvailableAsrtLabels( ~frms, ~parenCnt, ~stmtToProve, ),
        label: None,
        depthStr: "4",
        depth: 4,
        lengthRestrict: Less,
        allowNewVars: true,
        useRootStmtsAsArgs: false,

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

let toggleAllowNewVars = (st) => {
    {
        ...st,
        allowNewVars: !st.allowNewVars
    }
}

let toggleUseRootStmtsAsArgs = (st) => {
    {
        ...st,
        useRootStmtsAsArgs: !st.useRootStmtsAsArgs
    }
}

let newStmtsDtoToResultRendered = (newStmtsDto:newStmtsDto, idx:int):resultRendered => {
    let elem = 
        <Col>
            {
                newStmtsDto.newDisjStr
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
                        newStmtsDto.stmts
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
            newStmtsDto.stmts[newStmtsDto.stmts->Js.Array2.length-1].jstf
                ->Belt_Option.map(jstf => jstf.label)
                ->Belt_Option.getWithDefault(""),
        numOfNewVars: newStmtsDto.newVars->Js.Array2.length,
        numOfNewUnprovedStmts: newStmtsDto.stmts->Js.Array2.reduce(
            (cnt,stmt) => cnt + if (stmt.isProved) {0} else {1},
            0
        ),
        isProved: newStmtsDto.stmts[newStmtsDto.stmts->Js.Array2.length-1].isProved,
        numOfStmts: newStmtsDto.stmts->Js.Array2.length,
    }
}

let compareByIsProved = Expln_utils_common.comparatorBy(res => if (res.isProved) {0} else {1})
let compareByNumberOfStmts = Expln_utils_common.comparatorBy(res => res.numOfStmts)

let compareByNumOfNewUnprovedStmts = Expln_utils_common.comparatorBy(res => res.numOfNewUnprovedStmts)
let compareByNumOfNewVars = Expln_utils_common.comparatorBy(res => res.numOfNewVars)
let compareByAsrtLabel = (a,b) => a.asrtLabel->Js.String2.localeCompare(b.asrtLabel)->Belt.Float.toInt

let createComparator = (sortBy):Expln_utils_common.comparator<resultRendered> => {
    open Expln_utils_common
    let mainCmp = switch sortBy {
        | UnprovedStmtsNum => compareByNumOfNewUnprovedStmts
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
            let resultsRendered = Some(results->Js_array2.mapi((dto,i) => newStmtsDtoToResultRendered(dto,i)))
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

let notDigitPattern = %re("/\D/g")

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
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~wrkCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~userStmtToProve:userStmt,
    ~stmts: array<rootStmt>,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~onResultSelected:newStmtsDto=>unit,
    ~onCancel:unit=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState( 
        ~userStmtToProve, ~stmts, ~frms, ~parenCnt,
    ))

    let onlyOneResultIsAvailable = switch state.results {
        | None => false
        | Some(results) => results->Js_array2.length == 1
    }

    let actLabelUpdated = label => {
        setState(setLabel(_,label))
    }

    let actDepthUpdated = depthStr => {
        setState(setDepthStr(_,depthStr->Js.String2.replaceByRe(notDigitPattern, "")))
    }

    let actLengthRestrictUpdated = lengthRestrict => {
        setState(setLengthRestrict(_,lengthRestrict))
    }

    let actToggleAllowNewVars = () => {
        setState(toggleAllowNewVars)
    }

    let actToggleUseRootStmtsAsArgs = () => {
        setState(toggleUseRootStmtsAsArgs)
    }

    let rndTitle = () => {
        state.title
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
            ~rootStmts = stmts,
            ~ctx = wrkCtx,
            ~typeToPrefix,
        )
        setState(setResults(_, Some(treeDto), Some(results)))
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
                    ~preCtxVer, ~preCtx, ~parenStr, ~varsText, ~disjText, ~hyps, ~stmts,
                    ~bottomUpProverParams=Some({
                        asrtLabel: st.label,
                        maxSearchDepth: st.depth,
                        lengthRestriction: st.lengthRestrict,
                        allowNewVars: st.allowNewVars,
                        useRootStmtsAsArgs: st.useRootStmtsAsArgs,
                    }),
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
                        <Col>
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                                {React.string("Close")}
                            </Button>
                            <MM_cmp_proof_tree
                                tree
                                rootExpr=state.exprToProve
                                wrkCtx
                                rootStmts=stmts
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
                            <MenuItem value="UnprovedStmtsNum">{React.string("Number of new unproved statements")}</MenuItem>
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
                    <Button onClick={_=>actProve()} variant=#outlined>
                        {React.string("Prove")}
                    </Button>
                    <Button onClick={_=>onCancel()}> {React.string("Cancel")} </Button>
                </Row>
                <Row>
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewVars
                                onChange={_ => actToggleAllowNewVars()}
                            />
                        }
                        label="Allow new variables"
                    />
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.useRootStmtsAsArgs
                                onChange={_ => actToggleUseRootStmtsAsArgs()}
                            />
                        }
                        label="Derive from root statements"
                    />
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
            | Some(tree) => {
                <Button onClick={_=>()} variant=#outlined>
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
                        {React.string("Nothing found.")}
                        {rndShowProofTreeBtn()}
                    </Row>
                } else {
                    let start = (state.resultsPage - 1) * state.resultsPerPage
                    let items = resultsSorted->Js_array2.slice( ~start, ~end_ = start + state.resultsPerPage )
                    <Col>
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

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.5>
            {rndTitle()}
            {rndParams()}
            {rndResults()}
        </Col>
    </Paper>
}