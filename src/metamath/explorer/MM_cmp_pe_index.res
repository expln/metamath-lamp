open MM_context
open MM_wrk_settings
open Expln_React_Modal

type props = {
    modalRef:modalRef,
    settingsVer:int,
    settings:settings,
    preCtxVer:int,
    preCtx:mmContext,
}

let propsAreSame = (a:props, b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.preCtxVer == b.preCtxVer
}

let make = React.memoCustomCompareProps(({
    modalRef,
    settingsVer,
    settings,
    preCtxVer,
    preCtx,
}:props) => {
    let (typeColors, setTypeColors) = React.useState(() => settings->settingsGetTypeColors)
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])
    let (filteredLabelsVer, setFilteredLabelsVer) = React.useState(() => 0)

    let actSettingsChanged = () => {
        setTypeColors(_ => settings->settingsGetTypeColors)
    }

    let actCtxChanged = () => {
        let allLabels = preCtx->getAllFrameLabels->Js.Array2.mapi((label,i) => (i+1, label))
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)
        setFilteredLabelsVer(prev => prev+1)
    }

    React.useEffect1(() => {
        actSettingsChanged()
        None
    }, [settingsVer])

    React.useEffect1(() => {
        actCtxChanged()
        None
    }, [preCtxVer])

    <MM_cmp_pe_frame_list
        modalRef
        settingsVer
        settings
        typeColors
        preCtxVer
        preCtx
        labelsVer=filteredLabelsVer
        labels=filteredLabels
    />

}, propsAreSame)