open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_Modal
open Expln_React_Mui
open Expln_React_common
open MM_wrk_pre_ctx_data

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

    let actPreCtxDataChanged = () => {
        let settings = preCtxData.settingsV.val
        setTypeColors(_ => settings->settingsGetTypeColors)

        let preCtx = preCtxData.ctxV.val
        setPreCtxVer(_ => preCtxData.ctxV.ver)
        let allLabels = preCtx->getAllFrameLabels->Js.Array2.mapi((label,i) => (i+1, label))
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)
        setIsAxiomFilter(_ => None)
        setLabelFilter(_ => "")
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

    let actApplyFilters = () => {
        setFilteredLabels(_ => {
            allLabels->Js.Array2.filter(((i,label)) => {
                isAxiomFilter->Belt_Option.mapWithDefault(
                    true, 
                    isAxiomFilter => isAxiomFilter === (preCtxData.ctxV.val->getFrameExn(label)).isAxiom
                ) && label->Js_string2.toLowerCase->Js.String2.includes(labelFilter->Js_string2.toLowerCase)
            })
        })
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
            style=ReactDOM.Style.make(~width="100px", ())
            autoFocus=true
            value=labelFilter
            onChange=evt2str(actLabelFilterUpdated)
            onKeyDown=kbrdHnd(~onEnter=actApplyFilters, ())
        />
    }

    let rndFilters = () => {
        <Row>
            {rndIsAxiomFilter()}
            {rndLabelFilter()}
        </Row>
    }

    <Col style=ReactDOM.Style.make(~marginLeft="15px", ~marginRight="15px", ())>
        {rndFilters()}
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