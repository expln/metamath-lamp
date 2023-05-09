open MM_context
open MM_wrk_settings

type props = {
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
    settingsVer,
    settings,
    preCtxVer,
    preCtx,
}:props) => {
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])
    let (filteredLabelsVer, setFilteredLabelsVer) = React.useState(() => 0)

    let actSettingsChanged = () => {
        ()
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

    <MM_cmp_pe_asrt_list
        settingsVer
        settings
        preCtxVer
        preCtx
        labelsVer=filteredLabelsVer
        labels=filteredLabels
    />

}, propsAreSame)