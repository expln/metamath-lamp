open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_wrk_editor
open MM_wrk_settings
open Expln_React_Modal

type tabData =
    | Settings
    | Editor
    | Search
    | ProofExplorer({label:string})

type state = {
    settings: settings,
    settingsV: int,
    srcs: array<mmCtxSrcDto>,
    ctx: mmContext,
    ctxV: int,
}

let createInitialState = (~settings) => {
    settings,
    settingsV: 0,
    srcs: [],
    ctx: createContext(()),
    ctxV: 0,
}

let setCtx = (st,srcs,ctx) => {
    {
        ...st,
        srcs,
        ctx,
        ctxV: st.ctxV + 1,
    }
}

let setSettings = (st,settings) => {
    {
        ...st,
        settings,
        settingsV: st.settingsV + 1
    }
}

@get external getClientHeight: Dom.element => int = "clientHeight"
@new external makeMutationObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

let mainTheme = ThemeProvider.createTheme(
    {
        "palette": {
            "grey": {
                "main": "#e0e0e0",
            }
        }
    }
)

@react.component
let make = () => {
    let modalRef = useModalRef()
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = Expln_React_UseTabs.useTabs()
    let (state, setState) = React.useState(_ => createInitialState(~settings=settingsReadFromLocStor()))

    let reloadCtx = React.useRef(Js.Nullable.null)

    let actCtxUpdated = (srcs:array<mmCtxSrcDto>, newCtx:mmContext, settingsOpt) => {
        let settings = switch settingsOpt {
            | Some(settings) => settings
            | None => state.settings
        }
        newCtx->moveConstsToBegin(settings.parens)
        setState(setCtx(_,srcs,newCtx))
    }

    let actSettingsUpdated = newSettings => {
        setState(setSettings(_,newSettings))
        settingsSaveToLocStor(newSettings)
        actCtxUpdated(state.srcs, state.ctx, Some(newSettings))
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->Expln_React_UseTabs.getTabs->Js_array2.length == 0) {
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Settings", ~closable=false, ~data=Settings)
                let (st, editorTabId) = st->Expln_React_UseTabs.addTab(~label="Editor", ~closable=false, ~data=Editor)
                // let (st, _) = st->Expln_React_UseTabs.addTab(~label="Search", ~closable=false, ~data=Search)
                let st = st->Expln_React_UseTabs.openTab(editorTabId)
                st
            } else {
                st
            }
        })
        None
    })

    let rndTabContent = (top:int, tab:Expln_React_UseTabs.tab<'a>) => {
        <div key=tab.id style=ReactDOM.Style.make(~display=if (tab.id == activeTabId) {"block"} else {"none"}, ())>
            {
                switch tab.data {
                    | Settings => 
                        <MM_cmp_settings 
                            modalRef
                            ctx=state.ctx 
                            settingsVer=state.settingsV
                            settings=state.settings
                            onChange=actSettingsUpdated
                        />
                    | Editor => 
                        <MM_cmp_editor
                            top
                            modalRef
                            settings=state.settings
                            settingsV=state.settingsV
                            srcs=state.srcs
                            preCtxV=state.ctxV
                            preCtx=state.ctx
                        />
                    | Search => <MM_cmp_click_counter title="Search" />
                    | ProofExplorer({label}) => <MM_cmp_click_counter title=label />
                }
            }
        </div>
    }

    <ThemeProvider theme=mainTheme>
        <Expln_React_ContentWithStickyHeader
            top=0
            header={
                <Col>
                    <MM_cmp_context_selector 
                        modalRef 
                        webSrcSettings={state.settings.webSrcSettings}
                        onUrlBecomesTrusted={url=>{
                            state.settings->markUrlAsTrusted(url)->actSettingsUpdated
                        }}
                        onChange={(srcs,ctx)=>actCtxUpdated(srcs, ctx, None)}
                        reloadCtx
                    />
                    {renderTabs()}
                </Col>
            }
            content={contentTop => {
                <Col>
                    {React.array(tabs->Js_array2.map(rndTabContent(contentTop, _)))}
                    <Expln_React_Modal modalRef />
                </Col>
            }}
        />
    </ThemeProvider>
}