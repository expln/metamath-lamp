open MM_context
open MM_wrk_settings

type props = {
    settingsV:int,
    settings:settings,
    preCtxV:int,
    preCtx:mmContext,
}

let propsAreSame = (a:props, b:props):bool => {
    a.settingsV == b.settingsV
    && a.preCtxV == b.preCtxV
}

let make = React.memoCustomCompareProps(({
    settingsV,
    settings,
    preCtxV,
    preCtx,
}:props) => {
    let (frames, setFrames) = React.useState(() => [])

    // React.useEffect2(() => {
    //     // preCtx->forEachFrame
    //     None
    // }, settingsV, preCtxV)
    React.null
}, propsAreSame)