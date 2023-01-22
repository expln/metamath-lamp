open Expln_React_Mui
open MM_context
open MM_cmp_settings
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
    ctx: mmContext,
    ctxV: int,
}

let createInitialState = (~settings) => {
    ctx: createContext(()),
    ctxV: 0,
    settings,
    settingsV: 0,
}

let setCtx = (st,ctx) => {
    {
        ...st,
        ctx,
        ctxV: st.ctxV + 1
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

@react.component
let make = () => {
    let modalRef = useModalRef()
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = Expln_React_UseTabs.useTabs()
    let (state, setState) = React.useState(_ => createInitialState(~settings=settingsReadFromLocStor()))

    let actCtxUpdated = (newCtx, settingsOpt) => {
        let settings = switch settingsOpt {
            | Some(settings) => settings
            | None => state.settings
        }
        newCtx->moveConstsToBegin(settings.parens)
        setState(setCtx(_,newCtx))
    }

    let actSettingsUpdated = newSettings => {
        setState(setSettings(_,newSettings))
        actCtxUpdated(state.ctx, Some(newSettings))
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
                            initialSettings=state.settings 
                            onChange=actSettingsUpdated 
                        />
                    | Editor => 
                        <MM_cmp_editor
                            top
                            modalRef
                            settings=state.settings
                            settingsV=state.settingsV
                            preCtxV=state.ctxV
                            preCtx=state.ctx
                        />
                    | Search => <MM_cmp_click_counter title="Search" />
                    | ProofExplorer({label}) => <MM_cmp_click_counter title=label />
                }
            }
        </div>
    }

    <Expln_React_ContentWithStickyHeader
        top=0
        header={
            <Col>
                <MM_cmp_context_selector onChange=actCtxUpdated(_, None) modalRef />
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
}