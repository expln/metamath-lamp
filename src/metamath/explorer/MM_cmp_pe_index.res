open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_Modal
open Expln_React_Mui
open Expln_React_common
open MM_wrk_pre_ctx_data
open Common

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    openFrameExplorer:string=>unit,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    openFrameExplorer,
}:props) => {
    let settings = preCtxData.settingsV.val
    let preCtx = preCtxData.ctxV.val

    let (preCtxVer, setPreCtxVer) = React.useState(() => preCtxData.ctxV.ver)
    let (typeColors, setTypeColors) = React.useState(() => settings->settingsGetTypeColors)
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])

    let (isAxiomFilter, setIsAxiomFilter) = React.useState(() => None)
    let (labelFilter, setLabelFilter) = React.useState(() => "")
    let (patternFilterStr, setPatternFilterStr) = React.useState(() => "")
    let (patternFilterErr, setPatternFilterErr) = React.useState(() => None)
    let (applyFiltersRequested, setApplyFiltersRequested) = React.useState(() => false)

    let actClearFilters = (~applyFilters:bool) => {
        setIsAxiomFilter(_ => None)
        setLabelFilter(_ => "")
        setPatternFilterStr(_ => "")
        setPatternFilterErr(_ => None)
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
        let allLabels = preCtx->getAllFrameLabels->Js.Array2.mapi((label,i) => (i+1, label))
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)
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

    let actIsAxiomFilterUpdated = isAxiomStr => {
        setIsAxiomFilter(_ => isAxiomStr->isAxiomFilterFromStr)
    }

    let actLabelFilterUpdated = newLabelFilter => {
        setLabelFilter(_ => newLabelFilter)
    }

    let actPatternFilterStrUpdated = newPatternFilterStr => {
        setPatternFilterStr(_ => newPatternFilterStr)
    }

    React.useEffect1(() => {
        actApplyFilters()
        None
    }, [isAxiomFilter])

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

    let rndLabelFilter = () => {
        <TextField 
            label="Label"
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            autoFocus=true
            value=labelFilter
            onChange=evt2str(actLabelFilterUpdated)
            onKeyDown=kbrdHnd(~onEnter=actApplyFilters, ())
        />
    }

    let rndPatternFilter = () => {
        <TextField 
            label="Pattern"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            value=patternFilterStr
            onChange=evt2str(actPatternFilterStrUpdated)
            onKeyDown=kbrdHnd(~onEnter=actApplyFilters, ())
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

    let rndClearFiltersBtn = () => {
        <span title="Clear filters">
            <IconButton onClick={_ => actClearFilters(~applyFilters=true)} color="primary"> 
                <MM_Icons.FilterAltOff/>
            </IconButton>
        </span>
    }

    let rndFilters = () => {
        <Row>
            {rndIsAxiomFilter()}
            {rndLabelFilter()}
            {rndPatternFilter()}
            {rndClearFiltersBtn()}
        </Row>
    }

    <Col style=ReactDOM.Style.make(~margin="15px", ())>
        {rndFilters()}
        {rndPatternError()}
        <MM_cmp_pe_frame_list
            key=`${preCtxVer->Belt_Int.toString}`
            modalRef
            editStmtsByLeftClick=settings.editStmtsByLeftClick
            typeColors
            preCtx
            frms=preCtxData.frms
            parenCnt=preCtxData.parenCnt
            syntaxTypes=preCtxData.syntaxTypes
            labels=filteredLabels
            openFrameExplorer
        />
    </Col>

}, propsAreSame)