open Expln_React_common
open Expln_React_Mui

@val external window: {..} = "window"

type tabId = string

type tab<'a> = {
    id:tabId,
    icon: option<React.element>,
    label: string,
    closable: bool,
    singleton: bool,
    color:option<string>,
    scrollX:float,
    scrollY:float,
    data: 'a
}

type state<'a> = {
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    tabHistory:array<tabId>,
}

type tabMethods<'a> = {
    addTab: (
        ~id:tabId=?, ~label:string, ~closable:bool, ~singleton:bool=?, ~color:string=?, ~data:'a, ~doOpen:bool=?
    ) => promise<tabId>,
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
        tabs: [],
        activeTabId: "",
        tabHistory: [],
    }
}

// Returns a random string of the given length (at least 1) consisting of lowercase latin letters and digits.
// The first character is always a letter, so the result never looks like a number.
// crypto.getRandomValues() is used rather than crypto.randomUUID(),
// because the latter is available only in secure contexts (https or localhost).
let makeRandomId: int => string = %raw(`(len) => {
    const letters = 'abcdefghijklmnopqrstuvwxyz'
    const lettersAndDigits = letters + '0123456789'
    const bytes = new Uint8Array(len)
    crypto.getRandomValues(bytes)
    return Array.from(bytes, (b, i) => i === 0 ? letters[b % letters.length] : lettersAndDigits[b % lettersAndDigits.length])
        .join('')
}`)

let tabIdLength = 10

// Returns a new random tab id for which isTaken returns false.
let makeUniqueTabId = (isTaken:tabId=>bool):tabId => {
    let newId = ref(makeRandomId(tabIdLength))
    while (isTaken(newId.contents)) {
        newId := makeRandomId(tabIdLength)
    }
    newId.contents
}

let getTabs = (st:state<'a>) => st.tabs

let tabExists = (st:state<'a>, tabId:tabId):bool => st.tabs->Array.some(t => t.id == tabId)

// Returns a new random id which is not used by any of the existing tabs.
let makeNewTabId = (st:state<'a>):tabId => makeUniqueTabId(tabExists(st, _))

let addNewTab = (
    st, ~id:option<tabId>, ~icon:option<React.element>, ~label:string, ~closable:bool, ~singleton:bool, 
    ~color:option<string>, ~data:'a, ~doOpen:bool
) => {
    let newId = switch id {
        | None => st->makeNewTabId
        | Some(id) => {
            if (st->tabExists(id)) {
                JsError.throwWithMessage(`Expln_React_UseTabs.addNewTab: a tab with id '${id}' already exists.`)
            }
            id
        }
    }
    let newTabs = st.tabs->Array.concat([
        {id:newId, icon, label, closable, singleton, color, scrollX:0.0, scrollY:0.0, data}
    ])
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

// If id is not specified, a new random id is generated.
// If a tab with the specified id already exists, an exception is thrown.
let addTab = (
    st:state<'a>, ~id:option<tabId>=?, ~icon:option<React.element>=?, ~label:string, ~closable:bool, ~singleton:bool=false, 
    ~color:option<string>=?, ~data:'a, ~doOpen:bool=false
):(state<'a>, tabId) => {
    let existingSingletonTab = if (singleton) {
        st.tabs->Array.find(tab => tab.singleton && tab.data == data)
    } else {
        None
    }
    switch existingSingletonTab {
        | Some(tab) => (doOpen ? st->openTab(tab.id) : st, tab.id)
        | None => addNewTab(st, ~id, ~icon, ~label, ~closable, ~singleton, ~color, ~data, ~doOpen)
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

    let addTab = (
        ~id:option<tabId>=?, ~label:string, ~closable:bool, ~singleton:bool=false, ~color:option<string>=?, ~data:'a, 
        ~doOpen:bool=false
    ):promise<tabId> => Promise.make((rlv,_) => {
        setState(prev => {
            let (st, tabId) = prev->addTab(~id?, ~label, ~closable, ~singleton, ~color?, ~data, ~doOpen)
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
            >
                {React.array(
                    tabs->Array.map(tab => {
                        <Tab 
                            key=tab.id 
                            value=tab.id 
                            label={
                                let labelWithIcon = switch tab.icon {
                                    | None => React.string(tab.label)
                                    | Some(icon) => {
                                        <span>
                                            icon
                                            {React.string(" " ++ tab.label)}
                                        </span>
                                    }
                                }
                                if (tab.closable) {
                                    <span style={textTransform:"none"}>
                                        <span style={marginRight:"5px"}>
                                            labelWithIcon
                                        </span>
                                        <IconButton 
                                            component="div" 
                                            onClick={evt => {
                                                ReactEvent.Synthetic.stopPropagation(evt)
                                                removeTab(tab.id)
                                            }} 
                                            style={ padding:"0px", }
                                        >
                                            <Icons.Clear fontSize="small"  />
                                        </IconButton>
                                    </span>
                                } else {
                                    labelWithIcon
                                }
                            }
                            style={ backgroundColor:?(tab.color), }
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