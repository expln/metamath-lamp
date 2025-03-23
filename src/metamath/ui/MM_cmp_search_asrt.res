open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open Expln_React_Modal
open MM_wrk_settings
open Common
open MM_wrk_pre_ctx_data

type state = {
    label:string,
    allTypes: array<int>,
    typ: int,
    patternStr: string,
    patternErr: option<string>,
    results: option<array<string>>,
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
        resultsPerPage:10,
        resultsMaxPage:1,
        resultsPage:1,
        checkedResultsIdx: [],
    }
}

let setResults = (
    st,
    ~results: array<string>,
):state => {
    let maxPage = Math.Int.ceil(results->Array.length->Belt_Int.toFloat /. st.resultsPerPage->Belt_Int.toFloat)
    {
        ...st,
        results:Some(results),
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
    ~preCtxData:preCtxData,
    ~wrkCtx: mmContext,
    ~initialTyp:option<int>,
    ~onTypChange:int=>unit,
    ~onCanceled:unit=>unit,
    ~onResultsSelected:array<string>=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState(preCtxData.frms, initialTyp))
    let rootPaperRef = React.useRef(Nullable.null)

    React.useEffect1(() => {
        switch rootPaperRef.current->Nullable.toOption {
            | None => ()
            | Some(rootPaperRef) => {
                switch Nullable.toOption(ReactDOM.domElementToObj(rootPaperRef)["parentNode"]) {
                    | None => ()
                    | Some(parentNode) => ReactDOM.domElementToObj(parentNode)["scrollTop"] = 0
                }
            }
        }
        None
    }, [state.resultsPage])

    let actResultsRetrieved = (results:array<string>) => {
        setState(setResults(_, ~results))
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
                        ~settingsVer=preCtxData.settingsV.ver,
                        ~settings=preCtxData.settingsV.val,
                        ~preCtxVer=preCtxData.ctxV.ver,
                        ~preCtx=preCtxData.ctxV.val.min,
                        ~isAxiom=None,
                        ~typ=Some(state.typ),
                        ~label=state.label->String.trim,
                        ~searchPattern,
                        ~isDisc=None,
                        ~isDepr=None,
                        ~isTranDepr=None,
                        ~dependsOn=[],
                        ~dependsOnTran=false,
                        ~onProgress = pct => updateModal(
                            modalRef, modalId, () => rndProgress(
                                ~text="Searching", ~pct, ~onTerminate=makeActTerminate(modalId)
                            )
                        )
                    )->promiseMap(foundLabels => {
                        closeModal(modalRef, modalId)
                        actResultsRetrieved(foundLabels)
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

    let addAsrtByLabel = (label:string):promise<result<unit,string>> => {
        onResultsSelected([label])
        Promise.resolve(Ok(()))
    }

    let rndFrameSummary = (label:string) => {
        switch preCtxData.ctxV.val.min->getFrame(label) {
            | None => React.null
            | Some(frame) => {
                <MM_cmp_pe_frame_summary
                    key={`${frame.ord->Belt.Int.toString}-${label}`}
                    modalRef
                    settings=preCtxData.settingsV.val
                    preCtx=preCtxData.ctxV.val.min
                    symColors=preCtxData.symColors
                    syntaxTypes=preCtxData.syntaxTypes
                    frms=preCtxData.frms
                    parenCnt=preCtxData.parenCnt
                    frame
                    order=None
                    typeColors=preCtxData.typeColors
                    typeOrderInDisj=preCtxData.typeOrderInDisj
                    editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                    openFrameExplorer=None
                    openExplorer=None
                    addAsrtByLabel=Some(addAsrtByLabel)
                />
            }
        }
    }

    let rndResults = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                let labelsToRender = []
                let minI = (state.resultsPage - 1) * state.resultsPerPage
                let maxI = Math.Int.min(minI + state.resultsPerPage - 1, results->Array.length-1)
                for i in minI to maxI {
                    labelsToRender->Array.push(results->Array.getUnsafe(i))
                }
                let totalNumOfResults = results->Array.length
                <Col>
                    {rndResultButtons()}
                    {rndPagination(totalNumOfResults)}
                    {
                        labelsToRender->Array.mapWithIndex((label,i) => {
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
                                            {rndFrameSummary(label)}
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

    <Paper ref=ReactDOM.Ref.domRef(rootPaperRef) style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndFilters()}
            {rndResults()}
        </Col>
    </Paper>
}