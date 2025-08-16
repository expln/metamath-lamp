open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_wrk_settings
open Expln_React_Modal
open Common
open MM_wrk_pre_ctx_data
open MM_react_common

let settingsSymbol = <img className="settings-img"></img>
let pencilSymbol = <img className="pencil-img"></img>
let magnifyingGlassSymbol = <img className="magnifying-glass-img"></img>
let turnstileSymbol = <img className="turnstile-img"></img>

type editorTabDataLocStor = {
    editorId:int, 
}

type editorTabData = {
    editorId:int, 
    initialStateLocStor:MM_wrk_editor_json.editorStateLocStor,
    addAsrtByLabel: ref<option<string=>promise<result<unit,string>>>>,
    updateTabTitle: ref<option<string=>unit>>,
}

type tabData =
    | Settings
    | TabsManager
    | Editor(editorTabData)
    | ExplorerIndex({initPatternFilterStr:string, initDependsOnFilter:string})
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

let readEditorsOrderFromLocStor = ():array<editorTabDataLocStor> => {
    switch Local_storage_utils.locStorReadString(editorsOrderLocStorKey) {
        | None => []
        | Some(orderStr) => {
            open Expln_utils_jsonParse
            let parseRes = orderStr->parseJson(asArr(_, asObj(_, o => {
                {
                    editorId:o->int("editorId"),
                }
            })), ~default=()=>[])
            switch parseRes {
                | Error(_) => []
                | Ok(res) => res
            }
        }
    }
}

let saveEditorsOrderToLocStor = (editorsOrder: array<editorTabDataLocStor>):unit => {
    Local_storage_utils.locStorWriteString(editorsOrderLocStorKey, Expln_utils_common.stringify(editorsOrder))
}

let getNewEditorIdFromEditorsOrder = (~editorsOrder: array<editorTabDataLocStor>):int => {
    let existingIds = editorsOrder->Array.map(ord => ord.editorId)
    let newId = ref(0)
    while (existingIds->Array.includes(newId.contents)) {
        newId := newId.contents + 1
    }
    newId.contents
}

let updateEditorsDataInLocStor = () => {
    /* 
    This function transforms local storage data of editors from the old format (when only a single editor was supported)
    to the newer format which supports multiple editors.
    */
    switch Local_storage_utils.locStorReadString(MM_cmp_editor.editorStateLocStorKey) {
        | None => ()
        | Some(oldEditorStateStr) => {
            let editorsOrder = readEditorsOrderFromLocStor()
            let newEditorId = getNewEditorIdFromEditorsOrder(~editorsOrder=editorsOrder)
            editorsOrder->Array.splice( ~start=0, ~remove=0, ~insert=[ {editorId:newEditorId} ] )
            saveEditorsOrderToLocStor(editorsOrder)
            Local_storage_utils.locStorWriteString(MM_cmp_editor.getEditorLocStorKey(newEditorId), oldEditorStateStr)
            switch Local_storage_utils.locStorReadString(MM_cmp_editor.editorHistRegLocStorKey) {
                | None => ()
                | Some(oldHistReg) => {
                    Local_storage_utils.locStorWriteString(
                        MM_cmp_editor.getEditorHistLocStorKey(newEditorId), oldHistReg
                    )
                }
            }
        }
    }
    Local_storage_utils.locStorDeleteKey(MM_cmp_editor.editorStateLocStorKey)
    Local_storage_utils.locStorDeleteKey(MM_cmp_editor.editorHistRegLocStorKey)
    Local_storage_utils.locStorDeleteKey(MM_cmp_editor.editorHistTmpLocStorKey)
}
updateEditorsDataInLocStor()

let deleteOutdatedEditorsDataInLocStor = () => {
    let existingEditorIds = readEditorsOrderFromLocStor()->Array.map(d => d.editorId)
    let allowedLocStorKeys = Array.concat(
        existingEditorIds->Array.map(MM_cmp_editor.getEditorLocStorKey),
        existingEditorIds->Array.map(MM_cmp_editor.getEditorHistLocStorKey)
    )->Belt_SetString.fromArray
    let locStorLen = Local_storage_utils.locStorLength()
    let keysToDelete = Belt_MutableSetString.make()
    let editorLocStorKeyPrefix = MM_cmp_editor.editorStateLocStorKey ++ "-"
    let editorHistLocStorKeyPrefix = MM_cmp_editor.editorHistLocStorKey ++ "-"
    for i in 0 to locStorLen-1 {
        switch Local_storage_utils.locStorKey(i) {
            | None => ()
            | Some(key) => {
                if (
                    (
                        key->String.startsWith(editorLocStorKeyPrefix)
                        || key->String.startsWith(editorHistLocStorKeyPrefix)
                    ) && !(allowedLocStorKeys->Belt_SetString.has(key))
                ) {
                    keysToDelete->Belt_MutableSetString.add(key)
                }
            }
        }
    }
    keysToDelete->Belt_MutableSetString.forEach(Local_storage_utils.locStorDeleteKey)
}

let readEditorStateFromJsonStr = (str:string):option<MM_wrk_editor_json.editorStateLocStor> => {
    MM_wrk_editor_json.readEditorStateFromJsonStr(str)->Result.mapOr(None, res=>Some(res))
}

let location = window["location"]
let editorInitialStateFromUrl:option<MM_wrk_editor_json.editorStateLocStor> =
    switch parseUrlQuery(location["search"])["get"]("editorState")->Nullable.toOption {
        | Some(initialStateSafeBase64) => {
            removeQueryParamsFromUrl("removing editorState from the URL")
            readEditorStateFromJsonStr(initialStateSafeBase64->safeBase64ToStr)
        }
        | None => None
    }

let getNewEditorId = (~existingTabs: array<Expln_React_UseTabs.tab<'a>>):int => {
    let existingIds = []
    existingTabs->Array.forEach(t => {
        switch t.data {
            | Editor({editorId}) => existingIds->Array.push(editorId)
            | Settings | TabsManager | ExplorerIndex(_) | ExplorerFrame(_) => ()
        }
    })
    let newId = ref(0)
    while (existingIds->Array.includes(newId.contents)) {
        newId := newId.contents + 1
    }
    newId.contents
}

let makeNewTabTitle = (
    ~existingTabs: array<Expln_React_UseTabs.tab<'a>>, 
    ~prefix:string
):string => {
    let existingTitles = existingTabs->Array.map(t => t.label)
    let newId = ref(1)
    let makeTitle = (i:int):string => prefix ++ " [" ++ i->Int.toString ++ "]"
    let newTitle = ref(makeTitle(newId.contents))
    while (existingTitles->Array.includes(newTitle.contents)) {
        newId := newId.contents + 1
        newTitle := makeTitle(newId.contents)
    }
    newTitle.contents
}

let getNewExplorerTabTitle = (
    ~existingTabs: array<Expln_React_UseTabs.tab<'a>>, 
    ~initPatternFilterStr:string="",
    ~initDependsOnFilter:string=""
):string => {
    if (initPatternFilterStr->String.length > 0) {
        let substrInc = initPatternFilterStr->String.startsWith("$+ ") ? 3 : 0
        initPatternFilterStr->String.substring(
            ~start=substrInc, 
            ~end=40+substrInc
        )
    } else if (initDependsOnFilter->String.length > 0) {
        ("Dependents of " ++ initDependsOnFilter)->String.substring(
            ~start=0, 
            ~end=40
        )
    } else {
        makeNewTabTitle(~existingTabs, ~prefix="EXPLORER")
    }
}

let synchEditorsDataInLocStor = (~existingTabs: array<Expln_React_UseTabs.tab<'a>>):unit => {
    let editorsOrder = []
    existingTabs->Array.forEach(tab => {
        switch tab.data {
            | Editor({editorId}) => editorsOrder->Array.push({editorId:editorId})
            | Settings | TabsManager | ExplorerIndex(_) | ExplorerFrame(_) => ()
        }
    })
    saveEditorsOrderToLocStor(editorsOrder)
    deleteOutdatedEditorsDataInLocStor()
}

let contextFromUrlHasBeenReloaded = ref(false)

@react.component
let make = () => {
    let modalRef = useModalRef()
    let (lastOpenedEditorId, setLastOpenedEditorId) = React.useState(() => None)

    let beforeTabRemove = (tab:Expln_React_UseTabs.tab<'a>) => {
        switch tab.data {
            | Settings | TabsManager => Promise.resolve(false)
            | ExplorerIndex(_) | ExplorerFrame(_) => Promise.resolve(true)
            | Editor({editorId}) => {
                openOkCancelDialog(
                    ~modalRef, 
                    ~title="Close editor tab?",
                    ~text=tab.label,
                )->Promise.thenResolve(confirmed => {
                    if (confirmed) {
                        MM_api_editor.deleteEditor(editorId)
                        if (lastOpenedEditorId == Some(editorId)) {
                            setLastOpenedEditorId(_ => None)
                        }
                    }
                    confirmed
                })
            }
        }
    }

    @warning("-27")
    let {
        tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId, setLabel, moveTabLeft, moveTabRight
    } = Expln_React_UseTabs.useTabs(~beforeTabRemove)
    let (state, setState) = React.useState(_ => createInitialState(~settings=settingsReadFromLocStor()))
    let (showTabs, setShowTabs) = React.useState(() => true)

    let reloadCtx: React.ref<option<MM_cmp_context_selector.reloadCtxFunc>> = React.useRef(None)
    let addAsrtByLabel: React.ref<option<string=>promise<result<unit,string>>>> = React.useRef(None)
    let toggleCtxSelector = React.useRef(Nullable.null)

    let (canStartSynchTabsOrder, setCanStartSynchTabsOrder) = React.useState(() => false)

    React.useEffect2(() => {
        if (canStartSynchTabsOrder) {
            synchEditorsDataInLocStor(~existingTabs=tabs)
        }
        None
    }, (canStartSynchTabsOrder, tabs))

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
                        MM_api_editor.setLastOpenedEditorId(editorId)
                        addAsrtByLabel.current = addAsrtByLabelRef.contents->Option.map(addAsrtByLabelOrig => {
                            str => {
                                addAsrtByLabelOrig(str)->Promise.thenResolve(res => {
                                    switch res {
                                        | Error(_) => ()
                                        | Ok(_) => openTab(activeTabId)
                                    }
                                    res
                                })
                            }
                        })
                    }
                }
            }
        }
        None
    }, [activeTabId])

    let actSettingsUpdated = (newSettings:settings):promise<result<unit,string>> => {
        settingsSaveToLocStor(newSettings)
        setState(updatePreCtxData(_,~settings=newSettings))
        if (
            state.preCtxData.settingsV.val.descrRegexToDisc != newSettings.descrRegexToDisc
            || state.preCtxData.settingsV.val.labelRegexToDisc != newSettings.labelRegexToDisc
            || state.preCtxData.settingsV.val.descrRegexToDepr != newSettings.descrRegexToDepr
            || state.preCtxData.settingsV.val.labelRegexToDepr != newSettings.labelRegexToDepr
        ) {
            switch reloadCtx.current {
                | None => Promise.resolve(Error("The reloadCtx() function is not initialized in MM_cmp_root."))
                | Some(reloadCtx) => reloadCtx(~srcs=state.preCtxData.srcs, ~settings=newSettings, ~force=true)
            }
        } else {
            Promise.resolve(Ok(()))
        }
    }

    let actCtxUpdated = (srcs:array<mmCtxSrcDto>, newCtx:mmContext) => {
        setState(updatePreCtxData(_,~ctx=(srcs,newCtx)))
    }

    let actCtxSelectorExpandedChange = (expanded) => {
        setState(updateCtxSelectorIsExpanded(_,expanded))
    }

    let actRenameTab = (id:Expln_React_UseTabs.tabId, newName:string) => {
        setLabel(id,newName)
        tabs->Array.forEach(tab => {
            if (tab.id == id) {
                switch isEditorTab(tab.data) {
                    | None => ()
                    | Some({updateTabTitle}) => {
                        updateTabTitle.contents->Option.forEach(updateTabTitle => updateTabTitle(newName))
                    }
                }
            }
        })
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
                    removeTab(tab.id)
                }
            }
        }
    }

    let openFrameExplorer = (label:string):unit => {
        setState(st => {
            switch st.preCtxData.ctxV.val.full->getFrame(label) {
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
                                    ~label, ~icon=turnstileSymbol, 
                                    ~closable=true, ~data=ExplorerFrame({label:label}), ~doOpen=true
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

    let actOpenExplorer = (~initPatternFilterStr:string="", ~initDependsOnFilter:string=""):unit => {
        updateTabs(tabsSt => {
            let newTabTitle = getNewExplorerTabTitle(
                ~existingTabs=tabsSt->Expln_React_UseTabs.getTabs,
                ~initPatternFilterStr=initPatternFilterStr->String.trim,
                ~initDependsOnFilter=initDependsOnFilter->String.trim,
            )
            let (tabsSt, _) = tabsSt->Expln_React_UseTabs.addTab(
                ~label=newTabTitle,
                ~icon=magnifyingGlassSymbol,
                ~closable=true, 
                ~data=ExplorerIndex({initPatternFilterStr, initDependsOnFilter}), 
                ~doOpen=true
            )
            tabsSt
        })
    }

    let setTabTitle = (
        ~editorStateLocStor:MM_wrk_editor_json.editorStateLocStor,
        ~existingTabs: array<Expln_React_UseTabs.tab<'a>>, 
    ):MM_wrk_editor_json.editorStateLocStor => {
        if (editorStateLocStor.tabTitle->String.trim == "") {
            let newTabTitle = makeNewTabTitle( ~existingTabs, ~prefix="EDITOR" )
            let newEditorStateLocStor = {...editorStateLocStor, tabTitle:newTabTitle}
            newEditorStateLocStor
        } else {
            editorStateLocStor
        }
    }

    let actOpenEditor = (
        ~initialStateLocStor:option<MM_wrk_editor_json.editorStateLocStor>=?,
    ):unit => {
        updateTabs(tabsSt => {
            let initialStateLocStor = switch initialStateLocStor {
                | Some(initialStateLocStor) => {
                    setTabTitle(
                        ~editorStateLocStor=initialStateLocStor,
                        ~existingTabs=tabsSt->Expln_React_UseTabs.getTabs,
                    )
                }
                | None => {
                    let newTabTitle = makeNewTabTitle(
                        ~existingTabs=tabsSt->Expln_React_UseTabs.getTabs,
                        ~prefix="EDITOR"
                    )
                    MM_wrk_editor_json.makeEmptyEditorStateLocStor(~tabTitle=newTabTitle)
                }
            }
            let newEditorId = getNewEditorId(~existingTabs=tabsSt->Expln_React_UseTabs.getTabs)
            let (tabsSt, _) = tabsSt->Expln_React_UseTabs.addTab(
                ~label=initialStateLocStor.tabTitle,
                ~icon=pencilSymbol,
                ~closable=true, 
                ~data=Editor({
                    editorId:newEditorId, 
                    initialStateLocStor,
                    addAsrtByLabel:ref(None),
                    updateTabTitle:ref(None),
                }),
                ~doOpen=true
            )
            tabsSt
        })
        switch initialStateLocStor {
            | None => ()
            | Some(initialStateLocStor) => {
                reloadCtx.current->Option.forEach(reloadCtx => {
                    reloadCtx(~srcs=initialStateLocStor.srcs, ~settings=state.preCtxData.settingsV.val)->Promise.done
                })
            }
        }
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            let st = if (st->Expln_React_UseTabs.getTabs->Array.length == 0) {
                let (st, _) = st->Expln_React_UseTabs.addTab(
                    ~label="", ~closable=false, ~data=Settings, ~icon=settingsSymbol,
                )
                let (st, _) = st->Expln_React_UseTabs.addTab(~label="Tabs", ~closable=false, ~data=TabsManager)
                let st = readEditorsOrderFromLocStor()->Array.reduceWithIndex(st, (st,{editorId},idx) => {
                    switch Local_storage_utils.locStorReadString(MM_cmp_editor.getEditorLocStorKey(editorId)) {
                        | None => st
                        | Some(editorStateJsonStr) => {
                            switch readEditorStateFromJsonStr(editorStateJsonStr) {
                                | None => st
                                | Some(editorStateLocStor) => {
                                    let editorStateLocStor = setTabTitle(
                                        ~editorStateLocStor,
                                        ~existingTabs=st->Expln_React_UseTabs.getTabs,
                                    )
                                    let (st, _) = st->Expln_React_UseTabs.addTab(
                                        ~label=editorStateLocStor.tabTitle, ~icon=pencilSymbol, ~closable=true, 
                                        ~doOpen= idx==0 && editorInitialStateFromUrl->Option.isNone,
                                        ~data=Editor({
                                            editorId, 
                                            initialStateLocStor:editorStateLocStor,
                                            addAsrtByLabel:ref(None),
                                            updateTabTitle:ref(None),
                                        }), 
                                    )
                                    st
                                }
                            }
                        }
                    }
                })
                let (st, _) = st->Expln_React_UseTabs.addTab(
                    ~label="EXPLORER", ~icon=magnifyingGlassSymbol, 
                    ~closable=true, ~data=ExplorerIndex({initPatternFilterStr:"", initDependsOnFilter:""}),
                    ~doOpen=!(st->Expln_React_UseTabs.getTabs->Array.some(t => isEditorTab(t.data)->Option.isSome))
                )
                st
            } else {
                st
            }
            setCanStartSynchTabsOrder(_ => true)
            st
        })
        switch editorInitialStateFromUrl {
            | None => ()
            | Some(editorInitialStateFromUrl) => {
                if (!contextFromUrlHasBeenReloaded.contents) {
                    contextFromUrlHasBeenReloaded := true
                    actOpenEditor(~initialStateLocStor=editorInitialStateFromUrl)
                }
            }
        }
        None
    })

    let actTabTitleUpdatedFromEditor = (tabId:Expln_React_UseTabs.tabId, newTitle:string):unit => {
        let newVerifiedTitle = if (newTitle->String.trim == "") {
            switch tabs->Array.find(tab => tab.id == tabId) {
                | None => newTitle
                | Some(tab) => tab.label
            }
        } else {
            newTitle
        }
        actRenameTab(tabId, newVerifiedTitle)
    }

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
                                    ->Array.map(tab => {MM_cmp_tabs_manager.id:tab.id, icon:tab.icon, label:tab.label})
                            }
                            onTabRename=actRenameTab
                            onTabMoveUp=actMoveTabLeft
                            onTabMoveDown=actMoveTabRight
                            onTabFocus=openTab
                            onTabClose=actCloseTab
                            onOpenEditor={()=>actOpenEditor()}
                            onOpenExplorer={()=>actOpenExplorer()}
                        />
                    | Editor({editorId, initialStateLocStor, addAsrtByLabel, updateTabTitle}) => 
                        <MM_cmp_editor
                            editorId
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            reloadCtx
                            addAsrtByLabel
                            updateTabTitle
                            initialStateLocStor=Some(initialStateLocStor)
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                            showTabs
                            setShowTabs={b=>setShowTabs(_ => b)}
                            openFrameExplorer
                            onTabTitleChange=actTabTitleUpdatedFromEditor(tab.id, _)
                        />
                    | ExplorerIndex({initPatternFilterStr, initDependsOnFilter}) => 
                        <MM_cmp_pe_index
                            modalRef
                            preCtxData=state.preCtxData
                            tabTitle=tab.label
                            openFrameExplorer
                            openExplorer=actOpenExplorer
                            toggleCtxSelector
                            ctxSelectorIsExpanded=state.ctxSelectorIsExpanded
                            initPatternFilterStr
                            initDependsOnFilter
                            addAsrtByLabel
                            onTabTitleChange={newTitle=>actRenameTab(tab.id, newTitle)}
                        />
                    | ExplorerFrame({label}) => 
                        <MM_cmp_pe_frame_full
                            top
                            modalRef
                            preCtxData=state.preCtxData
                            label
                            openFrameExplorer
                            openExplorer=actOpenExplorer
                            openEditor={editorStateLocStor=>
                                actOpenEditor(~initialStateLocStor=editorStateLocStor)
                            }
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
                                ->Promise.done
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
                    <MM_cmp_api modalRef />
                </Col>
            }}
        />
    </ThemeProvider>
}