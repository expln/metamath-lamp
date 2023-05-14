open MM_context
open MM_wrk_settings
open Expln_React_Modal

type props = {
    modalRef:modalRef,
    settings:settings,
    preCtx:mmContext,
}

let propsAreSame = (a:props, b:props):bool => {
    a.settings === b.settings
    && a.preCtx === b.preCtx
}

let make = React.memoCustomCompareProps(({
    modalRef,
    settings,
    preCtx,
}:props) => {
    let (typeColors, setTypeColors) = React.useState(() => settings->settingsGetTypeColors)
    let (allLabels, setAllLabels) = React.useState(() => [])
    let (filteredLabels, setFilteredLabels) = React.useState(() => [])

    let actSettingsChanged = () => {
        setTypeColors(_ => settings->settingsGetTypeColors)
    }

    let actCtxChanged = () => {
        let allLabels = preCtx->getAllFrameLabels->Js.Array2.mapi((label,i) => (i+1, label))
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)
    }

    React.useEffect1(() => {
        actSettingsChanged()
        None
    }, [settings])

    React.useEffect1(() => {
        actCtxChanged()
        None
    }, [preCtx])

    <MM_cmp_pe_frame_list
        modalRef
        editStmtsByLeftClick=settings.editStmtsByLeftClick
        typeColors
        preCtx
        labels=filteredLabels
    />

}, propsAreSame)