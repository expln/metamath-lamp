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

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    openFrameExplorer:string=>unit,
    toggleCtxSelector:React.ref<Js.Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData && a.ctxSelectorIsExpanded === b.ctxSelectorIsExpanded
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    openFrameExplorer,
    toggleCtxSelector,
    ctxSelectorIsExpanded,
}:props) => {
    let settings = preCtxData.settingsV.val
    let preCtx = preCtxData.ctxV.val

    let (preCtxVer, setPreCtxVer) = React.useState(() => preCtxData.ctxV.ver)
    let (typeColors, setTypeColors) = React.useState(() => settings->settingsGetTypeColors)
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])
    let (allStmtTypes, setAllStmtTypes) = React.useState(() => [])
    let (allStmtTypesConcat, setAllStmtTypesConcat) = React.useState(() => "all")

    let (isAxiomFilter, setIsAxiomFilter) = React.useState(() => None)
    let (stmtTypeFilter, setStmtTypeFilter) = React.useState(() => None)
    let (labelFilter, setLabelFilter) = React.useState(() => "")
    let (patternFilterStr, setPatternFilterStr) = React.useState(() => "")
    let (patternFilterErr, setPatternFilterErr) = React.useState(() => None)
    let (discFilter, setDiscFilter) = React.useState(() => false)
    let (deprFilter, setDeprFilter) = React.useState(() => false)
    let (tranDeprFilter, setTranDeprFilter) = React.useState(() => false)
    let (applyFiltersRequested, setApplyFiltersRequested) = React.useState(() => false)

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Js.Nullable.null)

    let (asrtsPerPage, setAsrtsPerPage) = useStateFromLocalStorageInt(
        ~key="pe-index-asrts-per-page", ~default=10
    )

    let actClearFilters = (~applyFilters:bool) => {
        setIsAxiomFilter(_ => None)
        setStmtTypeFilter(_ => None)
        setLabelFilter(_ => "")
        setPatternFilterStr(_ => "")
        setPatternFilterErr(_ => None)
        setDiscFilter(_ => false)
        setDeprFilter(_ => false)
        setTranDeprFilter(_ => false)
        if (applyFilters) {
            setApplyFiltersRequested(_ => true)
        }
    }

    let actApplyFilters = () => {
        let patternFilterSyms = patternFilterStr->getSpaceSeparatedValuesAsArray
        let incorrectSymbol = patternFilterSyms->Js_array2.find(sym => !(preCtxData.ctxV.val->isConst(sym)))
        switch incorrectSymbol {
            | Some(sym) => setPatternFilterErr(_ => Some(`'${sym}' - is not a constant.`))
            | None => {
                setPatternFilterErr(_ => None)
                let patternFilterInts = preCtxData.ctxV.val->ctxSymsToIntsExn(patternFilterSyms)
                let frameMatchesPattern = MM_wrk_search_asrt.frameMatchesPattern(_, patternFilterInts)
                setFilteredLabels(_ => {
                    allLabels->Js.Array2.filter(((_,label)) => {
                        let frame = preCtxData.ctxV.val->getFrameExn(label)
                        isAxiomFilter->Belt_Option.mapWithDefault(
                            true, 
                            isAxiomFilter => isAxiomFilter === frame.isAxiom
                        ) 
                        && stmtTypeFilter->Belt_Option.mapWithDefault(
                            true, 
                            stmtType => stmtType === frame.asrt[0]
                        ) 
                        && (!discFilter || frame.isDisc)
                        && (!deprFilter || frame.isDepr)
                        && (!tranDeprFilter || frame.isTranDepr)
                        && label->Js_string2.toLowerCase->Js.String2.includes(labelFilter->Js_string2.toLowerCase)
                        && frameMatchesPattern(frame)
                    })
                })
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

    let actPreCtxDataChanged = () => {
        let settings = preCtxData.settingsV.val
        setTypeColors(_ => settings->settingsGetTypeColors)

        let preCtx = preCtxData.ctxV.val
        setPreCtxVer(_ => preCtxData.ctxV.ver)
        let allFrames = preCtx->getAllFrames
        let allLabels = Expln_utils_common.createArray(allFrames->Belt_MapString.size)
        allFrames->Belt_MapString.forEach((_,frame) => {
            allLabels[frame.ord] = (frame.ord+1, frame.label)
        })
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)

        let allStmtIntTypes = []
        preCtx->forEachFrame(frame => {
            let stmtTyp = frame.asrt[0]
            if (!(allStmtIntTypes->Js.Array2.includes(stmtTyp))) {
                allStmtIntTypes->Js.Array2.push(stmtTyp)->ignore
            }
            None
        })->ignore
        let allStmtTypes = preCtx->ctxIntsToSymsExn(allStmtIntTypes)->Js.Array2.sortInPlace
        setAllStmtTypes(_ => allStmtTypes)
        setAllStmtTypesConcat(_ => "all" ++ allStmtTypes->Js.Array2.joinWith(""))

        actClearFilters(~applyFilters=false)
    }

    React.useEffect1(() => {
        actPreCtxDataChanged()
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

    let stmtTypeFilterToStr = typeFilter => {
        switch typeFilter {
            | None => allStmtTypesConcat
            | Some(n) => preCtx->ctxIntToSymExn(n)
        }
    }

    let stmtTypeFilterFromStr = str => {
        if (str == allStmtTypesConcat) {
            None
        } else {
            preCtx->ctxSymToInt(str)
        }
    }

    let actIsAxiomFilterUpdated = isAxiomStr => {
        setIsAxiomFilter(_ => isAxiomStr->isAxiomFilterFromStr)
    }

    let actStmtTypeFilterUpdated = stmtTypeStr => {
        setStmtTypeFilter(_ => stmtTypeStr->stmtTypeFilterFromStr)
    }

    let actLabelFilterUpdated = newLabelFilter => {
        setLabelFilter(_ => newLabelFilter)
    }

    let actPatternFilterStrUpdated = newPatternFilterStr => {
        setPatternFilterStr(_ => newPatternFilterStr)
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

    React.useEffect5(() => {
        actApplyFilters()
        None
    }, (isAxiomFilter, stmtTypeFilter, discFilter, deprFilter, tranDeprFilter))

    let actOpenMainMenu = () => {
        setMainMenuIsOpened(_ => true)
    }

    let actCloseMainMenu = () => {
        setMainMenuIsOpened(_ => false)
    }

    let actSetAsrtsPerPage = (strNum:string):unit => {
        switch strNum->Belt_Int.fromString {
            | None => ()
            | Some(num) => setAsrtsPerPage(_ => Js.Math.max_int(1, Js.Math.min_int(num, 100)))
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
                    allStmtTypes->Js_array2.map(stmtType => {
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
            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters, ())
        />
    }

    let rndPatternFilter = () => {
        <TextField 
            label="Pattern"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            value=patternFilterStr
            onChange=evt2str(actPatternFilterStrUpdated)
            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actApplyFilters, ())
        />
    }

    let rndPatternError = () => {
        switch patternFilterErr {
            | None => <></>
            | Some(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Malformed pattern text: " ++ msg)}
                </pre>
            }
        }
    }

    let rndDiscFilter = () => {
        <FormControlLabel
            control={
                <Checkbox
                    checked=discFilter
                    onChange=evt2bool(actDiscFilterUpdated)
                />
            }
            label="Discouraged"
            style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () )
        />
    }

    let rndDeprFilter = () => {
        <FormControlLabel
            control={
                <Checkbox
                    checked=deprFilter
                    onChange=evt2bool(actDeprFilterUpdated)
                />
            }
            label="Deprecated"
            style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () )
        />
    }

    let rndTranDeprFilter = () => {
        <FormControlLabel
            control={
                <Checkbox
                    checked=tranDeprFilter
                    onChange=evt2bool(actTranDeprFilterUpdated)
                />
            }
            label="Transitively deprecated"
            style=ReactDOM.Style.make( ~paddingRight="10px", ~marginTop="-2px", ~marginLeft="2px", () )
        />
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
                {rndApplyFiltersBtn()}
                {rndClearFiltersBtn()}
                {rndMainMenuBtn()}
            </Row>
            <Row>
                {rndStmtTypeFilter()}
                {rndDiscFilter()}
                {rndDeprFilter()}
                {rndTranDeprFilter()}
            </Row>
        </Col>
    }

    let rndMainMenu = () => {
        if (mainMenuIsOpened) {
            switch mainMenuButtonRef.current->Js.Nullable.toOption {
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
                                toggleCtxSelector.current->Js.Nullable.toOption
                                    ->Belt.Option.forEach(toggleCtxSelector => toggleCtxSelector())
                            }}
                        >
                            {React.string(if ctxSelectorIsExpanded {"Hide context"} else {"Show context"})}
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
        {rndPatternError()}
        {rndMainMenu()}
        <MM_cmp_pe_frame_list
            key=`${preCtxVer->Belt_Int.toString}`
            modalRef
            editStmtsByLeftClick=settings.editStmtsByLeftClick
            settings=preCtxData.settingsV.val
            typeColors
            preCtx
            frms=preCtxData.frms
            parenCnt=preCtxData.parenCnt
            syntaxTypes=preCtxData.syntaxTypes
            labels=filteredLabels
            openFrameExplorer
            asrtsPerPage
        />
    </Col>

}, propsAreSame)