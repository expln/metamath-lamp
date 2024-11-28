open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_wrk_settings
open Expln_React_Modal
open Common
open MM_wrk_pre_ctx_data
open MM_react_common

type editorTabData = {
    editorId:int, 
    addAsrtByLabel: ref<option<string=>promise<result<unit,string>>>>
}

type tabData =
    | Settings
    | TabsManager
    | Editor(editorTabData)
    | ExplorerIndex({initPatternFilterStr:string})
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
    ~ctx:option<(array<mmCtxSrcDto>,mmContext)>=?
): state => {
    {
        ...st,
        preCtxData: st.preCtxData->preCtxDataUpdate( ~settings?, ~ctx? )
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
            "lightgrey": {
                "main": "#e2e2e2",
            },
            "red": {
                "main": "#FF0000",
            },
            "pastelred": {
                "main": "#FAA0A0",
            },
            "orange": {
                "main": "#FF7900",
            },
            "yellow": {
                "main": "#FFE143",
            }
        }
    }
)

@new external parseUrlQuery: string => {..} = "URLSearchParams"
@val external window: {..} = "window"
@val external document: {..} = "document"

let editorsOrderLocStorKey = "editors-order"

let readEditorsOrderFromLocStor = ():array<int> => {
    switch Local_storage_utils.locStorReadString(editorsOrderLocStorKey) {
        | None => []
        | Some(orderStr) => {
            open Expln_utils_jsonParse
            let parseRes = orderStr->parseJson(asArr(_, asInt(_)), ~default=()=>[])
            switch parseRes {
                | Error(_) => []
                | Ok(res) => res
            }
        }
    }
}

let location = window["location"]
let tempMode = ref(false)
let editorInitialStateJsonStr = switch parseUrlQuery(location["search"])["get"](. "editorState")->Nullable.toOption {
    | Some(initialStateSafeBase64) => {
        removeQueryParamsFromUrl("removing editorState from the URL")
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

    let beforeTabRemove = (tab:Expln_React_UseTabs.tab<'a>) => {
        switch tab.data {
            | Settings | TabsManager => Promise.resolve(false)
            | ExplorerIndex(_) | ExplorerFrame(_) => Promise.resolve(true)
            | Editor(_) => {
                openOkCancelDialog(
                    ~modalRef, 
                    ~title="Confirm closing an editor tab",
                    ~text=`Close this editor tab? "${tab.label}"`, 
                )
            }
        }
    }

    @warning("-27")
    let {
        tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId, setLabel, moveTabLeft, moveTabRight
    } = Expln_React_UseTabs.useTabs(~beforeTabRemove)
    let (state, setState) = React.useState(_ => createInitialState(~settings=settingsReadFromLocStor()))
    let (showTabs, setShowTabs) = React.useState(() => true)
    let (lastOpenedEditorId, setLastOpenedEditorId) = React.useState(() => None)

    let reloadCtx: React.ref<Nullable.t<MM_cmp_context_selector.reloadCtxFunc>> = React.useRef(Nullable.null)
    let addAsrtByLabel: React.ref<option<string=>promise<result<unit,string>>>> = React.useRef(None)
    let toggleCtxSelector = React.useRef(Nullable.null)
    let loadEditorState = React.useRef(Nullable.null)

    let isFrameExplorerTab = (tabData:tabData, ~label:option<string>=?):bool => {
        switch tabData {
            | ExplorerFrame({label:lbl}) => label->Belt_Option.mapWithDefault(true, label => lbl == label)
            | _ => false
        }
    }

    let isEditorTab = (tabData:tabData):option<editorTabData> => {
        switch tabData {
            | Editor(editorTabData) => Some(editorTabData)
            | _ => None
        }
    }

    React.useEffect1(() => {
        switch tabs->Array.find(tab => tab.id == activeTabId) {
            | None => ()
            | Some(tab) => {
                switch isEditorTab(tab.data) {
                    | None => ()
                    | Some({editorId, addAsrtByLabel:addAsrtByLabelRef}) => {
                        setLastOpenedEditorId(_ => Some(editorId))
                        addAsrtByLabel.current = addAsrtByLabelRef.contents
                    }
                }
            }
        }
        None
    }, [activeTabId])

    let actCloseFrmTabs = () => {
        tabs->Array.forEach(tab => {
            if (isFrameExplorerTab(tab.data)) {
                removeTab(tab.id)
            }
        })
    }

    let actCtxUpdated = (srcs:array<mmCtxSrcDto>, newCtx:mmContext) => {
        actCloseFrmTabs()
        setState(updatePreCtxData(_,~ctx=(srcs,newCtx)))
    }

    let actSettingsUpdated = (newSettings:settings) => {
        actCloseFrmTabs()
        settingsSaveToLocStor(newSettings)
        setState(updatePreCtxData(_,~settings=newSettings))
        if (
            state.preCtxData.settingsV.val.descrRegexToDisc != newSettings.descrRegexToDisc
            || state.preCtxData.settingsV.val.labelRegexToDisc != newSettings.labelRegexToDisc
            || state.preCtxData.settingsV.val.descrRegexToDepr != newSettings.descrRegexToDepr
            || state.preCtxData.settingsV.val.labelRegexToDepr != newSettings.labelRegexToDepr
        ) {
            reloadCtx.current->Nullable.toOption->Belt.Option.forEach(reloadCtx => {
                reloadCtx(
                    ~srcs=state.preCtxData.srcs, 
                    ~settings=newSettings, 
                    ~force=true, 
                    ~showError=true
                )->ignore
            })
        }
    }

    let actCtxSelectorExpandedChange = (expanded) => {
        setState(updateCtxSelectorIsExpanded(_,expanded))
    }

    let actRenameTab = (id:Expln_React_UseTabs.tabId, newName:string) => {
        setLabel(id,newName)
    }

    let actMoveTab = (id:Expln_React_UseTabs.tabId, toRight:bool) => {
        switch tabs->Array.findIndexOpt(t => t.id == id) {
            | None => ()
            | Some(curIdx) => {
                let newIdx = toRight ? curIdx+1 : curIdx-1
                if (2 <= newIdx) {
                    toRight ? moveTabRight(id) : moveTabLeft(id)
                }
            }
        }
    }
    let actMoveTabLeft = (id:Expln_React_UseTabs.tabId) => actMoveTab(id, false)
    let actMoveTabRight = (id:Expln_React_UseTabs.tabId) => actMoveTab(id, true)

    let actCloseTab = (tabId:Expln_React_UseTabs.tabId) => {
        switch tabs->Array.find(t => t.id == tabId) {
            | None => ()
            | Some(tab) => {
                if (tab.closable) {
                    switch tab.data {
                        | Editor(_) => removeTab(tab.id)
                        | Settings | TabsManager | ExplorerIndex(_) | ExplorerFrame(_) => removeTab(tab.id)
                    }
                }
            }
        }
    }

    let openFrameExplorer = (label:string):unit => {
        setState(st => {
            switch st.preCtxData.ctxV.val->getFrame(label) {
                | None => {
                    openInfoDialog( ~modalRef, ~text=`Cannot find an assertion by label '${label}'` )
                }
                | Some(_) => {
                    updateTabs(tabsSt => {
                        let tabsSt = switch tabsSt->Expln_React_UseTabs.getTabs
                                                ->Array.find(tab => isFrameExplorerTab(tab.data, ~label)) {
                            | Some(tab) => tabsSt->Expln_React_UseTabs.openTab(tab.id)
                            | None => {
                                let (tabsSt, tabId) = tabsSt->Expln_React_UseTabs.addTab( 
                                    ~label, ~closable=true, ~data=ExplorerFrame({label:label}), ~doOpen=true
                                )
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

    let openExplorer = (~initPatternFilterStr:string=""):unit => {
        updateTabs(tabsSt => {
            let (tabsSt, tabId) = tabsSt->Expln_React_UseTabs.addTab( 
                ~label=`EXPLORER ${initPatternFilterStr->String.substring(~start=0, ~end=40)}`,
                ~closable=true, 
                ~data=ExplorerIndex({initPatternFilterStr:initPatternFilterStr}), 
                ~doOpen=true
            )
            tabsSt
        })
    }

    let focusEditorTab = ():unit => {
        updateTabs(tabsSt => {
            switch tabsSt->Expln_React_UseTabs.getTabs->Array.find(tab => isEditorTab(tab.data)->Option.isSome) {
                | Some(tab) => tabsSt->Expln_React_UseTabs.openTab(tab.id)
                | None => tabsSt
            }
        })
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->Expln_React_UseTabs.getTabs->Array.length == 0) {
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Settings", ~closable=false, ~data=Settings)
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Tabs", ~closable=false, ~data=TabsManager)
                let (st, _) = st->Expln_React_UseTabs.addTab(
                    ~label="EDITOR", ~closable=true, 
                    ~data=Editor({editorId:0, addAsrtByLabel:ref(None)}), 
                    ~doOpen=true, ~color=?(if (tempMode.contents) {Some("orange")} else {None})
                )
                let (st, _) = st->Expln_React_UseTabs.addTab(
                    ~label="EXPLORER", ~closable=true, ~data=ExplorerIndex({initPatternFilterStr:""})
                )
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
                    | TabsManager => 
                        <MM_cmp_tabs_manager
                            modalRef
                            tabs={
                                tabs
                                    ->Array.sliceToEnd(~start=2)
                                    ->Array.map(tab => {MM_cmp_tabs_manager.id:tab.id, label:tab.label})
                            }
                            onTabRename=actRenameTab
                            onTabMoveUp=actMoveTabLeft
                            onTabMoveDown=actMoveTabRight
                            onTabFocus=openTab
                            onTabClose=actCloseTab
                            onOpenExplorer={()=>openExplorer()}
                        />
                    | Editor({editorId, addAsrtByLabel}) => 
                        <MM_cmp_editor
                            editorId
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            reloadCtx
                            addAsrtByLabel
                            loadEditorState
                            initialStateJsonStr=editorInitialStateJsonStr
                            tempMode=tempMode.contents
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                            showTabs
                            setShowTabs={b=>setShowTabs(_ => b)}
                            openFrameExplorer
                        />
                    | ExplorerIndex({initPatternFilterStr}) => 
                        <MM_cmp_pe_index
                            modalRef
                            preCtxData=state.preCtxData
                            openFrameExplorer
                            openExplorer
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                            initPatternFilterStr
                            addAsrtByLabel
                        />
                    | ExplorerFrame({label}) => 
                        <MM_cmp_pe_frame_full
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            label
                            openFrameExplorer
                            openExplorer
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
                    {React.array(tabs->Array.map(rndTabContent(contentTop, _)))}
                    <Expln_React_Modal modalRef />
                </Col>
            }}
        />
    </ThemeProvider>
}