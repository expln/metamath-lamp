open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_wrk_settings
open Modal

type tabData =
    | Settings
    | Editor
    | Search
    | ProofExplorer({label:string})

type state = {
    ctx: mmContext,
    ctxV: int,
    settings: settings,
    settingsV: int,
}

let createInitialState = (~settingsLocStor:option<settings>) => {
    ctx: createContext(()),
    ctxV: 0,
    settings: settingsLocStor->Belt_Option.getWithDefault(createDefaultSettings()),
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
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = UseTabs.useTabs()
    let (state, setState) = React.useState(_ => createInitialState(~settingsLocStor=settingsReadFromLocStor("settings")))

    let onContextWasUpdated = newCtx => {
        setState(setCtx(_,newCtx))
        tabs->Js.Array2.forEach(tab => {
            switch tab.data {
                | ProofExplorer(_) => removeTab(tab.id)
                | _ => ()
            }
        })
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->UseTabs.getTabs->Js_array2.length == 0) {
                let (st, _) = st->UseTabs.addTab(~label="Settings", ~closable=false, ~data=Settings)
                let (st, editorTabId) = st->UseTabs.addTab(~label="Editor", ~closable=false, ~data=Editor)
                // let (st, _) = st->UseTabs.addTab(~label="Search", ~closable=false, ~data=Search)
                let st = st->UseTabs.openTab(editorTabId)
                st
            } else {
                st
            }
        })
        None
    })

    let rndTabContent = (top:int, tab:UseTabs.tab<'a>) => {
        <div key=tab.id style=ReactDOM.Style.make(~display=if (tab.id == activeTabId) {"block"} else {"none"}, ())>
            {
                switch tab.data {
                    | Settings => 
                        <MM_cmp_settings 
                            modalRef
                            ctx=state.ctx 
                            initialSettings=state.settings 
                            onChange={newSettings => setState(setSettings(_,newSettings))} 
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

    <ContentWithStickyHeader 
        top=0
        header={
            <Col>
                <MM_cmp_context_selector onChange=onContextWasUpdated modalRef />
                {renderTabs()}
            </Col>
        }
        content={contentTop => {
            <Col>
                {React.array(tabs->Js_array2.map(rndTabContent(contentTop, _)))}
                <Modal modalRef />
            </Col>
        }}
    />
}