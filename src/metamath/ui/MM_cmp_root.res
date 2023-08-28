open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_wrk_settings
open Expln_React_Modal
open Common
open MM_wrk_pre_ctx_data
open MM_react_common

type tabData =
    | Settings
    | Editor
    | ExplorerIndex
    | ExplorerFrame({label:string})

type state = {
    preCtxData:preCtxData,
    ctxSelectorIsExpanded:bool,
}

let createInitialState = (~settings) => {
    preCtxData: preCtxDataMake(~settings),
    ctxSelectorIsExpanded:true,
}

let updatePreCtxData = (
    st:state,
    ~settings:option<settings>=?,
    ~ctx:option<(array<mmCtxSrcDto>,mmContext)>=?,
    ()
): state => {
    {
        ...st,
        preCtxData: st.preCtxData->preCtxDataUpdate( ~settings?, ~ctx?, () )
    }
}

let updateCtxSelectorIsExpanded = (st:state,ctxSelectorIsExpanded:bool):state => {
    {...st, ctxSelectorIsExpanded}
}

@get external getClientHeight: Dom.element => int = "clientHeight"
@new external makeMutationObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

let mainTheme = ThemeProvider.createTheme(
    {
        "palette": {
            "white": {
                "main": "#ffffff",
            },
            "grey": {
                "main": "#e0e0e0",
            },
            "red": {
                "main": "#FF0000",
            },
            "pastelred": {
                "main": "#FAA0A0",
            }
        }
    }
)

@new external parseUrlQuery: string => {..} = "URLSearchParams"
@val external window: {..} = "window"
@val external document: {..} = "document"

let location = window["location"]
let tempMode = ref(false)
let editorInitialStateJsonStr = switch parseUrlQuery(location["search"])["get"](. "editorState")->Js.Nullable.toOption {
    | Some(initialStateSafeBase64) => {
        window["history"]["replaceState"](. 
            "removing editorState from the URL", 
            "", 
            location["origin"] ++ location["pathname"]
        )->ignore
        tempMode := true
        Some(initialStateSafeBase64->safeBase64ToStr)
    }
    | None => Local_storage_utils.locStorReadString(MM_cmp_editor.editorStateLocStorKey)
}

if (tempMode.contents) {
    document["title"] = "TEMP " ++ document["title"]
}

@react.component
let make = () => {
    let modalRef = useModalRef()
    @warning("-27")
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = Expln_React_UseTabs.useTabs()
    let (state, setState) = React.useState(_ => createInitialState(~settings=settingsReadFromLocStor()))
    let (showTabs, setShowTabs) = React.useState(() => true)

    let reloadCtx: React.ref<Js.Nullable.t<MM_cmp_context_selector.reloadCtxFunc>> = React.useRef(Js.Nullable.null)
    let toggleCtxSelector = React.useRef(Js.Nullable.null)
    let loadEditorState = React.useRef(Js.Nullable.null)

    let isFrameExplorerTab = (tabData:tabData, ~label:option<string>=?, ()):bool => {
        switch tabData {
            | ExplorerFrame({label:lbl}) => label->Belt_Option.mapWithDefault(true, label => lbl == label)
            | _ => false
        }
    }

    let isEditorTab = (tabData:tabData):bool => {
        switch tabData {
            | Editor => true
            | _ => false
        }
    }

    let actCloseFrmTabs = () => {
        tabs->Js.Array2.forEach(tab => {
            if (isFrameExplorerTab(tab.data, ())) {
                removeTab(tab.id)
            }
        })
    }

    let actCtxUpdated = (srcs:array<mmCtxSrcDto>, newCtx:mmContext) => {
        actCloseFrmTabs()
        setState(updatePreCtxData(_,~ctx=(srcs,newCtx), ()))
    }

    let actSettingsUpdated = (newSettings:settings) => {
        actCloseFrmTabs()
        settingsSaveToLocStor(newSettings)
        setState(updatePreCtxData(_,~settings=newSettings, ()))
        if (
            state.preCtxData.settingsV.val.descrRegexToDisc != newSettings.descrRegexToDisc
            || state.preCtxData.settingsV.val.labelRegexToDisc != newSettings.labelRegexToDisc
            || state.preCtxData.settingsV.val.descrRegexToDepr != newSettings.descrRegexToDepr
            || state.preCtxData.settingsV.val.labelRegexToDepr != newSettings.labelRegexToDepr
        ) {
            reloadCtx.current->Js.Nullable.toOption->Belt.Option.forEach(reloadCtx => {
                reloadCtx(
                    ~srcs=state.preCtxData.srcs, 
                    ~settings=newSettings, 
                    ~force=true, 
                    ~showError=true, 
                    ()
                )->ignore
            })
        }
    }

    let actCtxSelectorExpandedChange = (expanded) => {
        setState(updateCtxSelectorIsExpanded(_,expanded))
    }

    let openFrameExplorer = (label:string):unit => {
        setState(st => {
            switch st.preCtxData.ctxV.val->getFrame(label) {
                | None => {
                    openInfoDialog( ~modalRef, ~text=`Cannot find an assertion by label '${label}'`, () )
                }
                | Some(_) => {
                    updateTabs(tabsSt => {
                        let tabsSt = switch tabsSt->Expln_React_UseTabs.getTabs
                                                ->Js.Array2.find(tab => isFrameExplorerTab(tab.data, ~label, ())) {
                            | Some(tab) => tabsSt->Expln_React_UseTabs.openTab(tab.id)
                            | None => {
                                let (tabsSt, tabId) = tabsSt->Expln_React_UseTabs.addTab( 
                                    ~label, ~closable=true, ~data=ExplorerFrame({label:label}), ~doOpen=true, ()
                                )
                                let tabsSt = tabsSt->Expln_React_UseTabs.openTab(tabId)
                                tabsSt
                            }
                        }
                        tabsSt
                    })
                }
            }
            st
        })
    }

    let focusEditorTab = ():unit => {
        updateTabs(tabsSt => {
            switch tabsSt->Expln_React_UseTabs.getTabs->Js.Array2.find(tab => isEditorTab(tab.data)) {
                | Some(tab) => tabsSt->Expln_React_UseTabs.openTab(tab.id)
                | None => tabsSt
            }
        })
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->Expln_React_UseTabs.getTabs->Js_array2.length == 0) {
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Settings", ~closable=false, ~data=Settings, ())
                let (st, _) = st->Expln_React_UseTabs.addTab(
                    ~label="Editor", ~closable=false, ~data=Editor, ~doOpen=true, 
                    ~color=?(if (tempMode.contents) {Some("orange")} else {None}), 
                    ()
                )
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Explorer", ~closable=false, ~data=ExplorerIndex, ())
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
                            preCtxData=state.preCtxData
                            onChange=actSettingsUpdated
                        />
                    | Editor => 
                        <MM_cmp_editor
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            reloadCtx
                            loadEditorState
                            initialStateJsonStr=editorInitialStateJsonStr
                            tempMode=tempMode.contents
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                            showTabs
                            setShowTabs={b=>setShowTabs(_ => b)}
                            openFrameExplorer
                        />
                    | ExplorerIndex => 
                        <MM_cmp_pe_index
                            modalRef
                            preCtxData=state.preCtxData
                            openFrameExplorer
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                        />
                    | ExplorerFrame({label}) => 
                        <MM_cmp_pe_frame_full
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            label
                            openFrameExplorer
                            loadEditorState
                            focusEditorTab
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                        />
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
                        settings={state.preCtxData.settingsV.val}
                        onUrlBecomesTrusted={
                            url => state.preCtxData.settingsV.val->markUrlAsTrusted(url)->actSettingsUpdated
                        }
                        onChange={(srcs,ctx)=>actCtxUpdated(srcs, ctx)}
                        reloadCtx
                        style=ReactDOM.Style.make(
                            ~display=
                                ?if(state.preCtxData.settingsV.val.hideContextSelector 
                                        && !state.ctxSelectorIsExpanded) {
                                    Some("none")
                                } else {None}, 
                            ()
                        )
                        onExpandedChange=actCtxSelectorExpandedChange
                        doToggle=toggleCtxSelector
                    />
                    {
                        if (showTabs) {
                            renderTabs()
                        } else {
                            <div style=ReactDOM.Style.make(~display="none", ()) />
                        }
                    }
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