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
    openExplorer:(~initPatternFilterStr:string=?)=>unit,
    toggleCtxSelector:React.ref<Nullable.t<unit=>unit>>,
    ctxSelectorIsExpanded:bool,
    initPatternFilterStr:string,
    addAsrtByLabel:React.ref<option<string=>promise<result<unit,string>>>>,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData && a.ctxSelectorIsExpanded === b.ctxSelectorIsExpanded
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    openFrameExplorer,
    openExplorer,
    toggleCtxSelector,
    ctxSelectorIsExpanded,
    initPatternFilterStr,
    addAsrtByLabel,
}:props) => {
    let settings = preCtxData.settingsV.val
    let preCtx = preCtxData.ctxFullV.val

    let (preCtxVer, setPreCtxVer) = React.useState(() => preCtxData.ctxFullV.ver)
    let (typeColors, setTypeColors) = React.useState(() => settings->settingsGetTypeColors)
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])
    let (allStmtTypes, setAllStmtTypes) = React.useState(() => [])
    let (allStmtTypesConcat, setAllStmtTypesConcat) = React.useState(() => "all")
    let (typeOrderInDisj, setTypeOrderInDisj) = React.useState(() => Belt_HashMapInt.make(~hintSize=0))

    let (isAxiomFilter, setIsAxiomFilter) = React.useState(() => None)
    let (stmtTypeFilter, setStmtTypeFilter) = React.useState(() => None)
    let (labelFilter, setLabelFilter) = React.useState(() => "")
    let (patternFilterStr, setPatternFilterStr) = React.useState(() => initPatternFilterStr)
    let (patternFilterErr, setPatternFilterErr) = React.useState(() => None)
    let (descrFilterStr, setDescrFilterStr) = React.useState(() => "")
    let (discFilter, setDiscFilter) = React.useState(() => None)
    let (deprFilter, setDeprFilter) = React.useState(() => None)
    let (tranDeprFilter, setTranDeprFilter) = React.useState(() => None)
    let (applyFiltersRequested, setApplyFiltersRequested) = React.useState(() => false)

    let (mainMenuIsOpened, setMainMenuIsOpened) = React.useState(_ => false)
    let mainMenuButtonRef = React.useRef(Nullable.null)

    let (asrtsPerPage, setAsrtsPerPage) = useStateFromLocalStorageInt(
        ~key="pe-index-asrts-per-page", ~default=10
    )

    let actClearFilters = (~applyFilters:bool) => {
        setIsAxiomFilter(_ => None)
        setStmtTypeFilter(_ => None)
        setLabelFilter(_ => "")
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

    let actApplyFilters = () => {
        let searchPattern = MM_wrk_search_asrt.makeSearchPattern(
            ~searchStr=patternFilterStr->String.trim,
            ~ctx=preCtxData.ctxFullV.val
        )
        switch searchPattern {
            | Error(msg) => setPatternFilterErr(_ => Some(msg))
            | Ok(searchPattern) => {
                setPatternFilterErr(_ => None)
                let mapping = Belt_HashMapInt.make(~hintSize=10)
                let frameMatchesPattern = frame => MM_wrk_search_asrt.frameMatchesPattern(
                    ~frame, ~searchPattern, ~mapping
                )
                let labelFilterTrim = labelFilter->String.trim->String.toLowerCase
                let descrFilterStrNorm = normalizeDescr(descrFilterStr)
                let filterByDescr = descrFilterStrNorm->String.length > 0
                setFilteredLabels(_ => {
                    allLabels->Array.filter(((_,label,frame)) => {
                        isAxiomFilter->Belt_Option.mapWithDefault(
                            true, 
                            isAxiomFilter => isAxiomFilter === frame.isAxiom
                        ) 
                        && stmtTypeFilter->Belt_Option.mapWithDefault(
                            true, 
                            stmtType => stmtType === frame.asrt->Array.getUnsafe(0)
                        ) 
                        && (MM_wrk_search_asrt.threeStateBoolMatchesTwoStateBool(discFilter, frame.isDisc))
                        && (MM_wrk_search_asrt.threeStateBoolMatchesTwoStateBool(deprFilter, frame.isDepr))
                        && (MM_wrk_search_asrt.threeStateBoolMatchesTwoStateBool(tranDeprFilter, frame.isTranDepr))
                        && label->String.toLowerCase->String.includes(labelFilterTrim)
                        && (
                            !filterByDescr
                            || frame.descrNorm->Belt.Option.mapWithDefault(false, descrNorm => {
                                descrNorm->String.includes(descrFilterStrNorm)
                            })
                        )
                        && frameMatchesPattern(frame)
                    })->Array.map(((idx,label,_)) => (idx,label))
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

        let preCtx = preCtxData.ctxFullV.val
        setPreCtxVer(_ => preCtxData.ctxFullV.ver)
        let allFrames = preCtx->getAllFrames
        let allLabels = Expln_utils_common.createArray(allFrames->Belt_MapString.size)
        allFrames->Belt_MapString.forEach((_,frame) => {
            allLabels[frame.ord] = (frame.ord+1, frame.label, frame)
        })
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels->Array.map(((idx,label,_)) => (idx,label)))

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

        let typeOrderInDisj = createTypeOrderFromStr(
            ~sortDisjByType=settings.sortDisjByType, 
            ~typeNameToInt=ctxSymToInt(preCtx, _)
        )
        setTypeOrderInDisj(_ => typeOrderInDisj)

        setApplyFiltersRequested(_ => true)
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
    React.useEffect5(() => {
        setIsFirstRender(_ => false)
        if (!isFirstRender) {
            actApplyFilters()
        }
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
            openExplorer
            asrtsPerPage
            typeOrderInDisj
            addAsrtByLabel={label=>{
                switch addAsrtByLabel.current {
                    | Some(addAsrtByLabel) => addAsrtByLabel(label)
                    | None => Promise.resolve(Error("Internal error: addAsrtByLabel is null"))
                }
            }}
        />
    </Col>

}, propsAreSame)