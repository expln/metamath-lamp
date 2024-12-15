open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

@val external window: {..} = "window"

type tabId = string

type tab<'a> = {
    id:tabId,
    icon: string,
    label: string,
    closable: bool,
    color:option<string>,
    scrollX:float,
    scrollY:float,
    data: 'a
}

type state<'a> = {
    nextId: int,
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    tabHistory:array<tabId>,
}

type tabMethods<'a> = {
    addTab: (~label:string, ~closable:bool, ~color:string=?, ~data:'a, ~doOpen:bool=?) => promise<tabId>,
    openTab: tabId => unit,
    moveTabLeft: tabId => unit,
    moveTabRight: tabId => unit,
    removeTab: tabId => unit,
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    renderTabs: unit => reElem,
    setLabel: (tabId,string) => unit,

    updateTabs: (state<'a> => state<'a>) => unit
}

let createEmptyState = () => {
    {
        nextId: 0,
        tabs: [],
        activeTabId: "",
        tabHistory: [],
    }
}

let getNextId = st => {
    ({...st, nextId: st.nextId+1}, st.nextId->Belt_Int.toString)
}

let getTabs = (st:state<'a>) => st.tabs

let addTab = (
    st, ~icon:string="", ~label:string, ~closable:bool, ~color:option<string>=?, ~data:'a, ~doOpen:bool=false
) => {
    let (st, newId) = st->getNextId
    let newTabs = st.tabs->Array.concat([{id:newId, icon, label, closable, color, scrollX:0.0, scrollY:0.0, data}])
    let newActiveTabId = if (newTabs->Array.length == 1) {
        newId
    } else {
        if (doOpen) {newId} else {st.activeTabId}
    }
    let newTabHistory = if (newTabs->Array.length == 1) {
        [newId]
    } else {
        if (doOpen) {st.tabHistory->Array.concat([newId])} else {st.tabHistory}
    }
    (
        {
            ...st,
            tabs: newTabs->Array.map(tab => {
                if (tab.id == st.activeTabId) {
                    {...tab, scrollX:window["scrollX"], scrollY:window["scrollY"]}
                } else {
                    tab
                }
            }),
            activeTabId: newActiveTabId,
            tabHistory: newTabHistory,
        },
        newId
    )
}

let openTab = (st:state<'a>, tabId):state<'a> => {
    if (st.tabs->Array.some(t => t.id == tabId)) {
        {
            ...st,
            activeTabId:tabId,
            tabs: st.tabs->Array.map(tab => {
                if (tab.id == st.activeTabId) {
                    {...tab, scrollX:window["scrollX"], scrollY:window["scrollY"]}
                } else {
                    tab
                }
            }),
            tabHistory:
                if (
                    st.tabHistory->Belt_Array.get(st.tabHistory->Array.length-1)
                        ->Belt.Option.mapWithDefault(false, lastId => lastId == tabId)
                ) {
                    st.tabHistory
                } else {
                    st.tabHistory->Array.concat([tabId])
                }
        }
    } else {
        st
    }
}

let moveTab = (st:state<'a>, tabId:tabId, toRight:bool):state<'a> => {
    switch st.tabs->Array.findIndexOpt(t => t.id == tabId) {
        | None => st
        | Some(curIdx) => {
            let newIndex = toRight ? curIdx+1 : curIdx-1
            if (0 <= newIndex && newIndex < st.tabs->Array.length) {
                let newTabs = st.tabs->Array.toSpliced(~start=curIdx, ~remove=1, ~insert=[])
                let newTabs = newTabs->Array.toSpliced(
                    ~start=newIndex, ~remove=0, ~insert=[st.tabs->Array.getUnsafe(curIdx)]
                )
                {...st, tabs:newTabs}
            } else {
                st
            }
        }
    }
}

let moveTabLeft = (st:state<'a>, tabId:tabId):state<'a> => moveTab(st, tabId, false)
let moveTabRight = (st:state<'a>, tabId:tabId):state<'a> => moveTab(st, tabId, true)

let removeTab = (st:state<'a>, tabId):state<'a> => {
    let newTabs = st.tabs->Array.filter(t => t.id != tabId)
    let newTabHistory = st.tabHistory->Array.filter(id => id != tabId)
    {
        ...st, 
        tabs: newTabs,
        activeTabId:
            if (newTabs->Array.length == 0) {
                ""
            } else if (st.activeTabId == tabId) {
                newTabHistory->Belt_Array.get(newTabHistory->Array.length-1)
                    ->Belt.Option.getWithDefault((newTabs->Array.getUnsafe(0)).id)
            } else {
                st.activeTabId
            },
        tabHistory: newTabHistory,
    }
}

let setLabel = (st:state<'a>, tabId:tabId, newLabel:string):state<'a> => {
    {
        ...st, 
        tabs: st.tabs->Array.map(tab => {
            if (tab.id == tabId) {
                {...tab, label:newLabel}
            } else {
                tab
            }
        }),
    }
}

let useTabs = (
    ~beforeTabRemove:option<tab<'a>=>promise<bool>>=?,
):tabMethods<'a> => {
    let (state, setState) = React.useState(createEmptyState)

    React.useEffect1(() => {
        switch state.tabs->Array.find(tab => tab.id == state.activeTabId) {
            | None => ()
            | Some(curTab) => window["scrollTo"](curTab.scrollX, curTab.scrollY)
        }
        None
    }, [state.activeTabId])

    let addTab = (~label:string, ~closable:bool, ~color:option<string>=?, ~data:'a, ~doOpen:bool=false):promise<tabId> => promise(rlv => {
        setState(prev => {
            let (st, tabId) = prev->addTab(~label, ~closable, ~color?, ~data, ~doOpen)
            rlv(tabId)
            st
        })
    })

    let openTab = id => {
        setState(prev => prev->openTab(id))
    }

    let moveTabLeft = id => {
        setState(prev => prev->moveTabLeft(id))
    }

    let moveTabRight = id => {
        setState(prev => prev->moveTabRight(id))
    }

    let removeTab = id => {
        switch state.tabs->Array.find(tab => tab.id == id) {
            | None => ()
            | Some(tabToRemove) => {
                switch beforeTabRemove {
                    | None => setState(prev => prev->removeTab(id))
                    | Some(beforeTabRemove) => {
                        beforeTabRemove(tabToRemove)->Promise.thenResolve(confirmed => {
                            if (confirmed) {
                                setState(prev => prev->removeTab(id))
                            }
                        })->ignore
                    }
                }
            }
        }
    }

    let setLabel = (id:tabId, newLabel:string) => {
        setState(prev => prev->setLabel(id,newLabel))
    }

    let renderTabs = () => {
        let {tabs, activeTabId} = state
        if (tabs->Array.length == 0) {
            React.null
        } else {
            <Tabs
                value=activeTabId 
                variant=#scrollable 
                onChange={(_,id)=>openTab(id)} 
                style=ReactDOM.Style.make(
                    ~minHeight="25px", 
                    ()
                )
            >
                {React.array(
                    tabs->Array.map(tab => {
                        <Tab 
                            key=tab.id 
                            value=tab.id 
                            label={
                                let labelWithIcon = tab.icon->String.length == 0 
                                    ? tab.label 
                                    : tab.icon ++ tab.label
                                if (tab.closable) {
                                    <span style=ReactDOM.Style.make(~textTransform="none", ())>
                                        <span style=ReactDOM.Style.make(~marginRight="5px", ())>
                                            {React.string(labelWithIcon)}
                                        </span>
                                        <IconButton 
                                            component="div" 
                                            onClick={evt => {
                                                ReactEvent.Synthetic.stopPropagation(evt)
                                                removeTab(tab.id)
                                            }} 
                                            style=ReactDOM.Style.make(
                                                ~padding="0px",
                                                ()
                                            )
                                        >
                                            <Icons.Clear fontSize="small"  />
                                        </IconButton>
                                    </span>
                                } else {
                                    React.string(labelWithIcon)
                                }
                            }
                            style=ReactDOM.Style.make(
                                ~minHeight="25px",
                                ~padding="3px",
                                ~backgroundColor=?(tab.color),
                                ()
                            )
                        />
                    })
                )}
            </Tabs>
        }
    }

    {
        addTab,
        openTab,
        moveTabLeft,
        moveTabRight,
        removeTab,
        tabs: state.tabs->Array.copy,
        activeTabId: state.activeTabId,
        renderTabs,
        setLabel,

        updateTabs: modifier => setState(modifier)
    }
}