open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_pre_ctx_data

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

    let actPreCtxDataChanged = () => {
        let settings = preCtxData.settingsV.val
        setTypeColors(_ => settings->settingsGetTypeColors)

        let preCtx = preCtxData.ctxV.val
        setPreCtxVer(_ => preCtxData.ctxV.ver)
        let allLabels = preCtx->getAllFrameLabels->Js.Array2.mapi((label,i) => (i+1, label))
        setAllLabels(_ => allLabels)
        setFilteredLabels(_ => allLabels)
    }

    React.useEffect1(() => {
        actPreCtxDataChanged()
        None
    }, [preCtxData])

    <Col style=ReactDOM.Style.make(~marginLeft="15px", ~marginRight="15px", ())>
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