open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open Expln_React_Modal
open MM_statements_dto
open MM_wrk_settings
open Common

type resultForRender = React.element

type state = {
    label:string,
    allTypes: array<int>,
    typ: int,
    patternStr: string,
    patternErr: option<string>,
    results: option<array<stmtsDto>>,
    resultsForRender: option<array<resultForRender>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultsIdx: array<int>
}

let makeInitialState = (frms, initialTyp:option<int>) => {
    if (frms->frmsSize == 0) {
        raise(MmException({msg:`Cannot search assertions when frms are empty.`}))
    }
    let allTypes = frms->frmsGetAllTypes
    {
        label: "",
        allTypes,
        typ: initialTyp
                ->Belt_Option.map(iniTyp => if (allTypes->Array.includes(iniTyp)) {iniTyp} else {allTypes->Array.getUnsafe(0)})
                ->Belt_Option.getWithDefault(allTypes->Array.getUnsafe(0)),
        patternStr: "",
        patternErr: None,
        results: None,
        resultsForRender: None,
        resultsPerPage:10,
        resultsMaxPage:1,
        resultsPage:1,
        checkedResultsIdx: [],
    }
}

let setResults = (
    st,
    ~results: array<stmtsDto>,
    ~getFrmLabelBkgColor: string=>option<string>,
):state => {
    let maxPage = Math.Int.ceil(results->Array.length->Belt_Int.toFloat /. st.resultsPerPage->Belt_Int.toFloat)
    {
        ...st,
        results:Some(results),
        resultsForRender:Some(
            results->Array.map(result => {
                let numOfStmt = result.stmts->Array.length
                let lastStmtIdx = numOfStmt - 1
                <Paper style=ReactDOM.Style.make(~padding="3px", ())>
                    <Col>
                        {React.array(
                            result.newDisjStr->Array.mapWithIndex((disjStr,i) => {
                                <React.Fragment key={"disj-" ++ i->Belt_Int.toString} >
                                    {React.string("$d " ++ disjStr ++ " $.")}
                                    <Divider/>
                                </React.Fragment>
                            })
                        )}
                        {React.array(
                            result.stmts->Array.mapWithIndex((stmt,i) => {
                                <React.Fragment key={"stmt-" ++ i->Belt_Int.toString} >
                                    <span 
                                        style=ReactDOM.Style.make(
                                            ~backgroundColor=?{
                                                if (i == lastStmtIdx) {getFrmLabelBkgColor(stmt.label)} else {None}
                                            }, 
                                            ~borderRadius="3px",
                                            ()
                                        )
                                    >
                                        {React.string(stmt.label)}
                                    </span>
                                    <span>
                                        {React.string(": " ++ stmt.exprStr)}
                                    </span>
                                    {
                                        if (i != lastStmtIdx) {
                                            <Divider/>
                                        } else {
                                            React.null
                                        }
                                    }
                                </React.Fragment>
                            })
                        )}
                    </Col>
                </Paper>
            })
        ),
        resultsMaxPage: maxPage,
        resultsPage: 1,
        checkedResultsIdx: [],
    }
}

let setPage = (st,page):state => {
    {
        ...st,
        resultsPage: Math.Int.max(0, Math.Int.min(st.resultsMaxPage, page)),
    }
}

let setLabel = (st,label):state => {
    {
        ...st,
        label
    }
}

let setType = (st,typ):state => {
    {
        ...st,
        typ
    }
}

let setPatternStr = (st,patternStr):state => {
    {
        ...st,
        patternStr
    }
}

let setPatternErr = (st,patternErr):state => {
    {
        ...st,
        patternErr
    }
}

let toggleResultChecked = (st,idx) => {
    if (st.checkedResultsIdx->Array.includes(idx)) {
        {
            ...st,
            checkedResultsIdx: st.checkedResultsIdx->Array.filter(i => i != idx)
        }
    } else {
        {
            ...st,
            checkedResultsIdx: st.checkedResultsIdx->Array.concat([idx])
        }
    }
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~wrkCtx: mmContext,
    ~frms: frms,
    ~initialTyp:option<int>,
    ~onTypChange:int=>unit,
    ~onCanceled:unit=>unit,
    ~onResultsSelected:array<stmtsDto>=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState(frms, initialTyp))

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch frms->frmsGetByLabel(label) {
            | None => None
            | Some(frm) => {
                MM_react_common.getFrmLabelBkgColor(frm.frame, settings)
            }
        }
    }

    let actResultsRetrieved = results => {
        setState(setResults(_, ~results, ~getFrmLabelBkgColor))
    }

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let actSearch = () => {
        onTypChange(state.typ)
        switch makeSearchPattern( ~searchStr=state.patternStr->String.trim, ~ctx=wrkCtx ) {
            | Error(msg) => {
                setState(setPatternErr(_, Some(msg)))
                actResultsRetrieved([])
            }
            | Ok(searchPattern) => {
                setState(setPatternErr(_, None))
                openModal(modalRef, () => rndProgress(~text="Searching", ~pct=0. ))->promiseMap(modalId => {
                    updateModal(
                        modalRef, modalId, () => rndProgress(
                            ~text="Searching", ~pct=0., ~onTerminate=makeActTerminate(modalId)
                        )
                    )
                    searchAssertions(
                        ~settingsVer,
                        ~settings,
                        ~preCtxVer,
                        ~preCtx,
                        ~varsText,
                        ~disjText,
                        ~label=state.label->String.trim,
                        ~typ=state.typ,
                        ~searchPattern,
                        ~onProgress = pct => updateModal(
                            modalRef, modalId, () => rndProgress(
                                ~text="Searching", ~pct, ~onTerminate=makeActTerminate(modalId)
                            )
                        )
                    )->promiseMap(found => {
                        closeModal(modalRef, modalId)
                        actResultsRetrieved(found)
                    })
                })->ignore
            }
        }
    }

    let actPageChange = newPage => {
        setState(setPage(_, newPage))
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = () => {
        switch state.results {
            | None => ()
            | Some(results) => {
                onResultsSelected(results->Array.filterWithIndex((_,i) => state.checkedResultsIdx->Array.includes(i)))
            }
        }
    }

    let actLabelChange = str => {
        setState(setLabel(_,str))
    }

    let actTypeChange = newTypeStr => {
        setState(setType(_,wrkCtx->ctxSymToIntExn(newTypeStr)))
    }

    let actPatternChange = newPatternStr => {
        setState(setPatternStr(_,newPatternStr))
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndPattern = () => {
        <TextField 
            label="Pattern"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            autoFocus=true
            value=state.patternStr
            onChange=evt2str(actPatternChange)
            onKeyDown=kbrdHnd2(
                kbrdClbkMake(~key=keyEnter, ~act=actSearch),
                kbrdClbkMake(~key=keyEsc, ~act=onCanceled),
            )
        />
    }
    
    let rndLabel = () => {
        <TextField 
            label="Label"
            size=#small
            style=ReactDOM.Style.make(~width="100px", ())
            value=state.label
            onChange=evt2str(actLabelChange)
            onKeyDown=kbrdHnd2(
                kbrdClbkMake(~key=keyEnter, ~act=actSearch),
                kbrdClbkMake(~key=keyEsc, ~act=onCanceled),
            )
        />
    }
    
    let rndTyp = () => {
        <FormControl size=#small>
            <InputLabel id="asrt-type-select-label">"Type"</InputLabel>
            <Select 
                labelId="asrt-type-select-label"
                value={wrkCtx->ctxIntToSymExn(state.typ)}
                label="Type"
                onChange=evt2str(actTypeChange)
            >
                {React.array(
                    state.allTypes->Array.map(typI => {
                        let typStr = wrkCtx->ctxIntToSymExn(typI)
                        <MenuItem key=typStr value=typStr>{React.string(typStr)}</MenuItem>
                    })
                )}
            </Select>
        </FormControl>
    }

    let rndFilters = () => {
        <Col>
            <Row>
                {rndLabel()}
                {rndTyp()}
                {rndPattern()}
                <Button onClick={_=>actSearch()} variant=#contained color="grey" >
                    {React.string("Search")}
                </Button>
                <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
            </Row>
            {rndError(state.patternErr)}
        </Col>
    }

    let rndPagination = totalNumOfResults => {
        if (state.resultsPerPage < totalNumOfResults) {
            <Pagination count=state.resultsMaxPage page=state.resultsPage onChange={(_,newPage) => actPageChange(newPage)} />
        } else {
            React.null
        }
    }

    let rndResultButtons = () => {
        <Row>
            <Button onClick={_=>actChooseSelected()} variant=#contained disabled={state.checkedResultsIdx->Array.length == 0}>
                {React.string("Choose selected")}
            </Button>
            <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
        </Row>
    }

    let rndResults = () => {
        switch state.resultsForRender {
            | None => React.null
            | Some(resultsForRender) => {
                let items = []
                let minI = (state.resultsPage - 1) * state.resultsPerPage
                let maxI = Math.Int.min(minI + state.resultsPerPage - 1, resultsForRender->Array.length-1)
                for i in minI to maxI {
                    let resultForRender = resultsForRender->Array.getUnsafe(i)
                    items->Array.push(resultForRender)
                }
                let totalNumOfResults = resultsForRender->Array.length
                <Col>
                    {rndResultButtons()}
                    {rndPagination(totalNumOfResults)}
                    {
                        items->Array.mapWithIndex((item,i) => {
                            let resIdx = minI + i
                            <table key={resIdx->Belt_Int.toString}>
                                <tbody>
                                    <tr>
                                        <td>
                                            <Checkbox
                                                checked={state.checkedResultsIdx->Array.includes(resIdx)}
                                                onChange={_ => actToggleResultChecked(resIdx)}
                                            />
                                        </td>
                                        <td>
                                            item
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        })->React.array
                    }
                    {rndPagination(totalNumOfResults)}
                    {rndResultButtons()}
                </Col>
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndFilters()}
            {rndResults()}
        </Col>
    </Paper>
}