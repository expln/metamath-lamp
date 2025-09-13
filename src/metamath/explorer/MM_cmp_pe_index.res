open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_Modal
open Expln_React_Mui
open Expln_React_common
open MM_wrk_pre_ctx_data
open Common
open Local_storage_utils
open Expln_utils_promise
open MM_wrk_sort_asrts
open MM_wrk_pattern_search

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    tabTitle:string,
    openFrameExplorer:string=>unit,
    openExplorer:(~initPatternFilterStr:string=?, ~initDependsOnFilter:string=?)=>unit,
    toggleCtxSelector:React.ref<Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
    initPatternFilterStr:string,
    initDependsOnFilter:string,
    addAsrtByLabel:React.ref<option<string=>promise<result<unit,string>>>>,
    onTabTitleChange:string=>unit,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData
    && a.ctxSelectorIsExpanded === b.ctxSelectorIsExpanded
    && a.tabTitle == b.tabTitle
}

type sorting = {
    sortBys:sortBys, 
    comparator:Expln_utils_common.comparator<frame>
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    tabTitle,
    openFrameExplorer,
    openExplorer,
    toggleCtxSelector,
    ctxSelectorIsExpanded,
    initPatternFilterStr,
    initDependsOnFilter,
    addAsrtByLabel,
    onTabTitleChange,
}:props) => {
    let (lastNonEmptyPreCtxVer, setLastNonEmptyPreCtxVer) = React.useState(
        () => preCtxData.srcs->Array.length == 0 ? None : Some(preCtxData.ctxV.ver)
    )
    let (refreshIsNeeded, setRefreshIsNeeded) = React.useState(() => false)
    let (applyFiltersRequested, setApplyFiltersRequested) = React.useState(() => false)

    let (allFramesInDeclarationOrder, setAllFramesInDeclarationOrder) = React.useState(() => [])
    let (allStmtTypes, setAllStmtTypes) = React.useState(() => [])
    let (allStmtTypesConcat, setAllStmtTypesConcat) = React.useState(() => "all")

    let (isAxiomFilter, setIsAxiomFilter) = React.useState(() => None)
    let (stmtTypeFilter, setStmtTypeFilter) = React.useState(() => None)
    let (labelFilter, setLabelFilter) = React.useState(() => "")
    let (dependsOnFilter, setDependsOnFilter) = React.useState(() => initDependsOnFilter)
    let (dependsOnTranFilter, setDependsOnTranFilter) = React.useState(() => false)
    let (patternFilterStr, setPatternFilterStr) = React.useState(() => initPatternFilterStr)
    let (patternFilterErr, setPatternFilterErr) = React.useState(() => None)
    let (descrFilterStr, setDescrFilterStr) = React.useState(() => "")
    let (discFilter, setDiscFilter) = React.useState(() => None)
    let (deprFilter, setDeprFilter) = React.useState(() => None)
    let (tranDeprFilter, setTranDeprFilter) = React.useState(() => None)

    let (sorting, setSorting) = React.useState(():sorting => {sortBys:[], comparator:(_,_)=>Ordering.equal})

    let (filteredLabels, setFilteredLabels) = React.useState(() => [])

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Nullable.null)

    let (asrtsPerPage, setAsrtsPerPage) = useStateFromLocalStorageInt(
        ~key="pe-index-asrts-per-page", ~default=10
    )

    let actClearFilters = (~applyFilters:bool) => {
        setIsAxiomFilter(_ => None)
        setStmtTypeFilter(_ => None)
        setLabelFilter(_ => "")
        setDependsOnFilter(_ => "")
        setDependsOnTranFilter(_ => false)
        setPatternFilterStr(_ => "")
        setPatternFilterErr(_ => None)
        setDescrFilterStr(_ => "")
        setDiscFilter(_ => None)
        setDeprFilter(_ => None)
        setTranDeprFilter(_ => None)
        if (applyFilters) {
            setApplyFiltersRequested(_ => true)
        }
    }

    let filterByDescr = (frames:array<(frame,option<matchedIndices>)>):array<(frame,option<matchedIndices>)> => {
        let descrFilterStrNorm = normalizeDescr(descrFilterStr)
        if (descrFilterStrNorm->String.length > 0) {
            frames->Array.filter(((frame,_)) => {
                frame.descrNorm->Belt.Option.mapWithDefault(false, descrNorm => {
                    descrNorm->String.includes(descrFilterStrNorm)
                })
            })
        } else {
            frames
        }
    }

    let filterByDependsOn = (frames:array<(frame,option<matchedIndices>)>):array<(frame,option<matchedIndices>)> => {
        let dependsOn:Belt_HashSetString.t = dependsOnFilter
            ->Common.getSpaceSeparatedValuesAsArray
            ->Belt_HashSetString.fromArray
        if (dependsOn->Belt_HashSetString.isEmpty) {
            frames
        } else {
            let res:array<(frame,option<matchedIndices>)> = []
            frames->Array.forEach(frameAndIdxs => {
                let (frame, _) = frameAndIdxs
                switch frame.proof {
                    | None => ()
                    | Some(Uncompressed({labels:parentLabels})) | Some(Compressed({labels:parentLabels})) => {
                        if (parentLabels->Array.some(Belt_HashSetString.has(dependsOn, _))) {
                            res->Array.push(frameAndIdxs)
                            if (dependsOnTranFilter) {
                                dependsOn->Belt_HashSetString.add(frame.label)
                            }
                        }
                    }
                }
            })
            res
        }
    }

    let mapToLabel = (frames:array<(frame,option<matchedIndices>)>):array<(string,option<matchedIndices>)> => {
        frames->Array.map(((frame,idxs)) => (frame.label,idxs))
    }

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let actPreCtxDataChanged = () => {
        let preCtx = preCtxData.ctxV.val.full
        let allFramesInDeclarationOrder = preCtx->getAllFrames->Belt_MapString.valuesToArray
            ->Expln_utils_common.sortInPlaceWith(Expln_utils_common.comparatorByInt(frm => frm.ord))
        setAllFramesInDeclarationOrder(_ => allFramesInDeclarationOrder)
        setFilteredLabels(_ => {
            allFramesInDeclarationOrder
                ->Array.toSorted(sorting.comparator)
                ->Array.map(frame => (frame,None))
                ->mapToLabel
        })

        let allStmtIntTypes = []
        preCtx->forEachFrame(frame => {
            let stmtTyp = frame.asrt->Array.getUnsafe(0)
            if (!(allStmtIntTypes->Array.includes(stmtTyp))) {
                allStmtIntTypes->Array.push(stmtTyp)
            }
            None
        })->ignore
        let allStmtTypes = preCtx->ctxIntsToSymsExn(allStmtIntTypes)->Js.Array2.sortInPlace
        setAllStmtTypes(_ => allStmtTypes)
        setAllStmtTypesConcat(_ => "all" ++ allStmtTypes->Array.joinUnsafe(""))
        //make sure the selected type is still present in the context
        setStmtTypeFilter(typ => typ->Option.flatMap(ctxSymToInt(preCtx, _))->Option.flatMap(ctxIntToSym(preCtx, _)))

        setApplyFiltersRequested(_ => true)
    }

    let actRefreshOnPreCtxDataChange = () => {
        actPreCtxDataChanged()
        setLastNonEmptyPreCtxVer( _ => Some(preCtxData.ctxV.ver) )
        setRefreshIsNeeded(_ => false)
    }

    let actApplyFilters = () => {
        if (refreshIsNeeded) {
            actRefreshOnPreCtxDataChange()
        } else {
            setApplyFiltersRequested(_ => false)
            let patternStr = patternFilterStr->String.trim
            let patternVersion = 2
            let ctx = preCtxData.ctxV.val.full
            let searchPattern = MM_wrk_pattern_search.parsePattern( ~patternStr, ~patternVersion, ~ctx )
            switch searchPattern {
                | Error(msg) => setPatternFilterErr(_ => Some(msg))
                | Ok(searchPattern) => {
                    setPatternFilterErr(_ => None)
                    let resultComparator = Expln_utils_common.comparatorBy(((frame,_)) => frame, sorting.comparator)
                    if (MM_wrk_pattern_search.patternIsEmpty(searchPattern)) {
                        setFilteredLabels(_ => {
                            MM_wrk_search_asrt.doSearchAssertions(
                                ~allFramesInDeclarationOrder,
                                ~isAxiom=isAxiomFilter,
                                ~typ=stmtTypeFilter->Option.map(typ => {
                                    preCtxData.ctxV.val.full->ctxSymToInt(typ)->Option.getExn(
                                        ~message=`Cannot convert symbol '${typ}' to int ` 
                                            ++ `in MM_cmp_pe_index.actApplyFilters[1].`
                                    )
                                }), 
                                ~label=labelFilter, 
                                ~pattern="",
                                ~patternVersion,
                                ~ctx,
                                ~isDisc=discFilter,
                                ~isDepr=deprFilter,
                                ~isTranDepr=tranDeprFilter,
                            )
                            ->filterByDescr
                            ->filterByDependsOn
                            ->Array.toSorted(resultComparator)
                            ->mapToLabel
                        })
                    } else {
                        openModal(modalRef, () => rndProgress(~text="Searching", ~pct=0. ))->promiseMap(modalId => {
                            updateModal(
                                modalRef, modalId, () => rndProgress(
                                    ~text="Searching", ~pct=0., ~onTerminate=makeActTerminate(modalId)
                                )
                            )
                            MM_wrk_search_asrt.searchAssertions(
                                ~settingsVer=preCtxData.settingsV.ver,
                                ~settings=preCtxData.settingsV.val,
                                ~preCtxVer=preCtxData.ctxV.ver,
                                ~preCtx=preCtxData.ctxV.val.min,
                                ~isAxiom=isAxiomFilter,
                                ~typ=stmtTypeFilter->Option.map(typ => {
                                    preCtxData.ctxV.val.full->ctxSymToInt(typ)->Option.getExn(
                                        ~message=`Cannot convert symbol '${typ}' to int ` 
                                            ++ `in MM_cmp_pe_index.actApplyFilters[2].`
                                    )
                                }),
                                ~label=labelFilter,
                                ~pattern=patternStr,
                                ~patternVersion,
                                ~isDisc=discFilter,
                                ~isDepr=deprFilter,
                                ~isTranDepr=tranDeprFilter,
                                ~onProgress = pct => updateModal(
                                    modalRef, modalId, () => rndProgress(
                                        ~text="Searching", ~pct, ~onTerminate=makeActTerminate(modalId)
                                    )
                                )
                            )
                            ->promiseMap((foundLabels:array<(string,option<matchedIndices>)>) => {
                                let labelToIdxs = Belt_HashMapString.fromArray(foundLabels)
                                setFilteredLabels(_ => {
                                    allFramesInDeclarationOrder
                                        ->Array.filter(frame => labelToIdxs->Belt_HashMapString.has(frame.label))
                                        ->Array.map(frame => {
                                            (
                                                frame, 
                                                labelToIdxs->Belt_HashMapString.get(frame.label)
                                                    ->Option.getExn(~message="Error MM_cmp_pe_index.actApplyFilters[3]")
                                            )
                                        })
                                        ->filterByDescr
                                        ->filterByDependsOn
                                        ->Array.toSorted(resultComparator)
                                        ->mapToLabel
                                })
                                closeModal(modalRef, modalId)
                            })
                        })->ignore
                    }
                }
            }
        }
    }

    React.useEffect1(() => {
        if (applyFiltersRequested) {
            setApplyFiltersRequested(_ => false)
            actApplyFilters()
        }
        None
    }, [applyFiltersRequested])

    React.useEffect1(() => {
        switch lastNonEmptyPreCtxVer {
            | None => {
                /* the page is just loaded with the default empty context, 
                    and now some non-default context may have been loaded by the user,
                    or else some other change may have happened, instead of changing the context 
                    (for example a change in settings) */
                setLastNonEmptyPreCtxVer(
                    _ => preCtxData.srcs->Array.length == 0 ? None : Some(preCtxData.ctxV.ver)
                )
                /* In the case of a change other than the context update, we can safely restart the search
                    (because the context is empty and the search will be very fast).
                    In case of the context update, there is possiblility that there are many opened explorers
                    with some filters set. So on the one hand we should not start the search, 
                    because it may take too long time and be confusing. But on the other hand,
                    if this is the only explorer and the user just loaded the first context,
                    it will be confusing to show the "Refresh" button instead of the full list of assertions.
                    The first scenario is less probable, so let's run a search.*/
                actPreCtxDataChanged()
            }
            | Some(lastNonEmptyPreCtxVer) => {
                /* at least one context has been loaded by the user by this moment, 
                    so the page is not just loaded with the default empty context. 
                    In other words, this is not the first context load, so we can show the "Refresh" button.
                    But there is one exception. If this tab has been opened by the search button, then it
                    was provided with a non empty context from the very beginning, and we should not show 
                    the "Refresh" button in that case.*/
                if (lastNonEmptyPreCtxVer == preCtxData.ctxV.ver) {
                    /* this is the case when the explorer tab has been opened by the search button */
                    actPreCtxDataChanged()
                } else {
                    setRefreshIsNeeded(_ => true)
                    setApplyFiltersRequested(_ => false)
                }
            }
        }
        None
    }, [preCtxData])

    let isAxiomFilterToStr = typeFilter => {
        switch typeFilter {
            | None => "none"
            | Some(false) => "false"
            | Some(true) => "true"
        }
    }

    let isAxiomFilterFromStr = str => {
        switch str {
            | "false" => Some(false)
            | "true" => Some(true)
            | _ => None
        }
    }

    let stmtTypeFilterToStr = (typeFilter:option<string>) => {
        switch typeFilter {
            | None => allStmtTypesConcat
            | Some(str) => str
        }
    }

    let actIsAxiomFilterUpdated = isAxiomStr => {
        setIsAxiomFilter(_ => isAxiomStr->isAxiomFilterFromStr)
    }

    let actStmtTypeFilterUpdated = (stmtTypeStr:string) => {
        let ctx = preCtxData.ctxV.val.full
        setStmtTypeFilter(_ => ctx->ctxSymToInt(stmtTypeStr)->Option.flatMap(ctxIntToSym(ctx, _)))
    }

    let actLabelFilterUpdated = newLabelFilter => {
        setLabelFilter(_ => newLabelFilter)
    }

    let actDependsOnFilterUpdated = newDependsOnFilter => {
        setDependsOnFilter(_ => newDependsOnFilter)
    }

    let actToggleDependsOnTranFilter = () => {
        setDependsOnTranFilter(prev => !prev)
    }

    let actPatternFilterStrUpdated = newPatternFilterStr => {
        setPatternFilterStr(_ => newPatternFilterStr)
    }

    let actDescrFilterStrUpdated = newDescrFilterStr => {
        setDescrFilterStr(_ => newDescrFilterStr)
    }

    let actDiscFilterUpdated = newDiscFilter => {
        setDiscFilter(_ => newDiscFilter)
    }

    let actDeprFilterUpdated = newDeprFilter => {
        setDeprFilter(_ => newDeprFilter)
    }

    let actTranDeprFilterUpdated = newTranDeprFilter => {
        setTranDeprFilter(_ => newTranDeprFilter)
    }

    let (isFirstRender, setIsFirstRender) = React.useState(() => true)
    React.useEffect7(() => {
        setIsFirstRender(_ => false)
        if (!isFirstRender) {
            actApplyFilters()
        }
        None
    }, (isAxiomFilter, stmtTypeFilter, discFilter, deprFilter, tranDeprFilter, dependsOnTranFilter, sorting))

    let actOpenMainMenu = () => {
        setMainMenuIsOpened(_ => true)
    }

    let actCloseMainMenu = () => {
        setMainMenuIsOpened(_ => false)
    }

    let actRenameThisTab = () => {
        openModalPaneWithTitle(
            ~modalRef,
            ~title="Rename tab",
            ~content = (~close) => {
                <MM_cmp_rename_tab 
                    initName=tabTitle
                    onOk={newName => {
                        close()
                        onTabTitleChange(newName)
                    }}
                    onCancel=close
                />
            }
        )
    }

    let actSetAsrtsPerPage = (strNum:string):unit => {
        switch strNum->Belt_Int.fromString {
            | None => ()
            | Some(num) => setAsrtsPerPage(_ => Math.Int.max(1, Math.Int.min(num, 100)))
        }
    }

    let actOpenAsrtsPerPageDialog = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_asrts_per_page
                    initAsrtsPerPage={asrtsPerPage->Belt_Int.toString}
                    onOk={strNum=>{
                        closeModal(modalRef, modalId)
                        actSetAsrtsPerPage(strNum)
                    }}
                    onCancel={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    let rndIsAxiomFilter = () => {
        <FormControl size=#small>
            <InputLabel id="isAxiomFilter-label">"Assertion type"</InputLabel>
            <Select
                sx={"width": 130}
                labelId="isAxiomFilter-label"
                value={isAxiomFilter->isAxiomFilterToStr}
                label="Assertion type"
                onChange=evt2str(actIsAxiomFilterUpdated)
            >
                <MenuItem value="none">{React.string("All")}</MenuItem>
                <MenuItem value="true">{React.string("Axiom")}</MenuItem>
                <MenuItem value="false">{React.string("Theorem")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndStmtTypeFilter = () => {
        <FormControl size=#small>
            <InputLabel id="stmtTypeFilter-label">"Statement type"</InputLabel>
            <Select
                sx={"width": 130}
                labelId="stmtTypeFilter-label"
                value={stmtTypeFilter->stmtTypeFilterToStr}
                label="Statement type"
                onChange=evt2str(actStmtTypeFilterUpdated)
            >
                <MenuItem value=allStmtTypesConcat>{React.string("All")}</MenuItem>
                {
                    allStmtTypes->Array.map(stmtType => {
                        <MenuItem key=stmtType value=stmtType>{React.string(stmtType)}</MenuItem>
                    })->React.array
                }
            </Select>
        </FormControl>
    }

    let rndLabelFilter = () => {
        <TextField 
            label="Label"
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            autoFocus=true
            value=labelFilter
            onChange=evt2str(actLabelFilterUpdated)
            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters)
        />
    }

    let rndDependsOnFilter = () => {
            <Row style=ReactDOM.Style.make(~border="1px solid lightgray", ~borderRadius="5px", ())>
                <TextField 
                    label="Depends on"
                    size=#small
                    style=ReactDOM.Style.make(~width="200px", ())
                    value=dependsOnFilter
                    onChange=evt2str(actDependsOnFilterUpdated)
                    onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters)
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=dependsOnTranFilter
                            onChange={_ => {
                                actToggleDependsOnTranFilter()
                            }}
                        />
                    }
                    label="transitively"
                />
            </Row>
    }

    let actSetSorting = (sortBys:sortBys) => {
        setSorting(_ => {
            sortBys,
            comparator: MM_wrk_sort_asrts.makeComparator(sortBys),
        })
    }

    let actOpenSortDialog = () => {
        openModalPaneWithTitle(
            ~modalRef,
            ~title="Sort by",
            ~content = (~close) => {
                <MM_cmp_sort_asrts_selector 
                    init={sorting.sortBys}
                    onOk={newSortBys => {
                        close()
                        actSetSorting(newSortBys)
                    }}
                    onCancel=close
                />
            }
        )
    }

    let rndPatternFilter = () => {
        <TextField 
            label="Pattern"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            value=patternFilterStr
            onChange=evt2str(actPatternFilterStrUpdated)
            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters)
        />
    }

    let rndDescrFilter = () => {
        <TextField 
            label="Description"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            value=descrFilterStr
            onChange=evt2str(actDescrFilterStrUpdated)
            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters)
        />
    }

    let rnd3StateCheckbox = (
        ~label:string, ~style:ReactDOM.Style.t, ~checked:option<bool>, ~onChange:option<bool>=>unit
    ) => {
        <FormControlLabel
            control={
                <Checkbox
                    checked={checked->Option.getOr(false)}
                    indeterminate={checked->Option.getOr(true) == false}
                    onChange={_ => {
                        switch checked {
                            | None => onChange(Some(true))
                            | Some(true) => onChange(Some(false))
                            | Some(false) => onChange(None)
                        }
                    }}
                />
            }
            label
            style
        />
    }

    let rndDiscFilter = () => {
        rnd3StateCheckbox(
            ~label="Discouraged", 
            ~style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () ), 
            ~checked=discFilter, 
            ~onChange=actDiscFilterUpdated
        )
    }

    let rndDeprFilter = () => {
        rnd3StateCheckbox(
            ~label="Deprecated", 
            ~style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () ), 
            ~checked=deprFilter, 
            ~onChange=actDeprFilterUpdated
        )
    }

    let rndTranDeprFilter = () => {
        rnd3StateCheckbox(
            ~label="Transitively deprecated", 
            ~style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () ), 
            ~checked=tranDeprFilter, 
            ~onChange=actTranDeprFilterUpdated
        )
    }

    let rndApplyFiltersBtn = () => {
        <span title="Apply filters">
            <IconButton onClick={_ => actApplyFilters()} color="primary"> 
                <MM_Icons.FilterAlt/>
            </IconButton>
        </span>
    }

    let rndClearFiltersBtn = () => {
        <span title="Clear filters">
            <IconButton onClick={_ => actClearFilters(~applyFilters=true)} color="primary"> 
                <MM_Icons.FilterAltOff/>
            </IconButton>
        </span>
    }

    let rndSortBtn = () => {
        <span title="Sort results">
            <IconButton onClick={_ => actOpenSortDialog()} color="primary"> 
                <MM_Icons.SwapVert/>
            </IconButton>
        </span>
    }

    let rndMainMenuBtn = () => {
        <span title="Additional actions" ref=ReactDOM.Ref.domRef(mainMenuButtonRef)>
            <IconButton onClick={_ => actOpenMainMenu()} color="primary"> 
                <MM_Icons.Menu/>
            </IconButton>
        </span>
    }

    let rndFilters = () => {
        <Col>
            <Row>
                {rndIsAxiomFilter()}
                {rndLabelFilter()}
                {rndPatternFilter()}
                {rndDescrFilter()}
                {rndApplyFiltersBtn()}
                {rndClearFiltersBtn()}
                {rndSortBtn()}
                {rndMainMenuBtn()}
            </Row>
            <Row>
                {rndStmtTypeFilter()}
                {rndDiscFilter()}
                {rndDeprFilter()}
                {rndTranDeprFilter()}
                {rndDependsOnFilter()}
            </Row>
            {
                if (sorting.sortBys->Array.length > 0) {
                    MM_cmp_sort_asrts_selector.renderSortBys(
                        ~sortBys=sorting.sortBys,
                        ~onClick=actOpenSortDialog,
                        ~onCancel=() => actSetSorting([])
                    )
                } else {
                    React.null
                }
            }
        </Col>
    }

    let rndMainMenu = () => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Nullable.toOption {
                | None => <></>
                | Some(mainMenuButtonRef) => {
                    <Menu
                        opn=true
                        anchorEl=mainMenuButtonRef
                        onClose=actCloseMainMenu
                    >
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                openExplorer()
                            }}
                        >
                            {React.string("Open new Explorer tab")}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                toggleCtxSelector.current->Nullable.toOption
                                    ->Belt.Option.forEach(toggleCtxSelector => toggleCtxSelector())
                            }}
                        >
                            {React.string(if ctxSelectorIsExpanded {"Hide context"} else {"Show context"})}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actRenameThisTab()
                            }}
                        >
                            {React.string("Rename this tab")}
                        </MenuItem>
                        <MenuItem
                            onClick={() => {
                                actCloseMainMenu()
                                actOpenAsrtsPerPageDialog()
                            }}
                        >
                            {React.string(`Assertions per page: ${asrtsPerPage->Belt_Int.toString}`)}
                        </MenuItem>
                    </Menu>
                }
            }
        } else {
            <></>
        }
    }

    <Col style=ReactDOM.Style.make(~margin="15px", ())>
        {rndFilters()}
        {rndMainMenu()}
        {
            if (refreshIsNeeded) {
                <Button onClick=(_=>actRefreshOnPreCtxDataChange()) variant=#contained > 
                    { React.string("Refresh") }
                </Button>
            } else {
                switch patternFilterErr {
                    | Some(msg) => {
                        <pre style=ReactDOM.Style.make(~color="red", ())>
                            {React.string("Malformed pattern text: " ++ msg)}
                        </pre>
                    }
                    | None => {
                        if (allFramesInDeclarationOrder->Array.length == 0) {
                            "The loaded context does not contain any assertions."->React.string
                        } else {
                            <MM_cmp_pe_frame_list
                                key=`${preCtxData.ctxV.ver->Belt_Int.toString}`
                                modalRef
                                editStmtsByLeftClick=preCtxData.settingsV.val.editStmtsByLeftClick
                                settings=preCtxData.settingsV.val
                                typeColors=preCtxData.typeColors
                                preCtx=preCtxData.ctxV.val.full
                                symColors=preCtxData.symColors
                                frms=preCtxData.frms
                                parenCnt=preCtxData.parenCnt
                                syntaxTypes=preCtxData.syntaxTypes
                                labels=filteredLabels
                                openFrameExplorer
                                openExplorer
                                asrtsPerPage
                                typeOrderInDisj=preCtxData.typeOrderInDisj
                                addAsrtByLabel={label=>{
                                    switch addAsrtByLabel.current {
                                        | Some(addAsrtByLabel) => addAsrtByLabel(label)
                                        | None => Promise.resolve(Error("Internal error: addAsrtByLabel is null"))
                                    }
                                }}
                            />
                        }
                    }
                }
            }
        }
    </Col>

}, propsAreSame)