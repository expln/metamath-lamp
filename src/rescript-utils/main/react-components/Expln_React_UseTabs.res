open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type tabId = string

type tab<'a> = {
    id:tabId,
    label: string,
    closable: bool,
    color:option<string>,
    data: 'a
}

type state<'a> = {
    nextId: int,
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    tabHistory:array<tabId>,
}

type tabMethods<'a> = {
    addTab: (~label:string, ~closable:bool, ~color:string=?, ~data:'a, ~doOpen:bool=?, ()) => promise<tabId>,
    openTab: tabId => unit,
    removeTab: tabId => unit,
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    renderTabs: unit => reElem,

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

let addTab = (st, ~label:string, ~closable:bool, ~color:option<string>=?, ~data:'a, ~doOpen:bool=false, ()) => {
    let (st, newId) = st->getNextId
    let newTabs = st.tabs->Js_array2.concat([{id:newId, label, closable, color, data}])
    let newActiveTabId = if (newTabs->Js_array2.length == 1) {
        newId
    } else {
        if (doOpen) {newId} else {st.activeTabId}
    }
    let newTabHistory = if (newTabs->Js_array2.length == 1) {
        [newId]
    } else {
        if (doOpen) {st.tabHistory->Js.Array2.concat([newId])} else {st.tabHistory}
    }
    (
        {
            ...st,
            tabs: newTabs,
            activeTabId: newActiveTabId,
            tabHistory: newTabHistory,
        },
        newId
    )
}

let openTab = (st:state<'a>, tabId):state<'a> => {
    if (st.tabs->Js_array2.some(t => t.id == tabId)) {
        {
            ...st, 
            activeTabId:tabId, 
            tabHistory:
                if (
                    st.tabHistory->Belt_Array.get(st.tabHistory->Js_array2.length-1)
                        ->Belt.Option.mapWithDefault(false, lastId => lastId == tabId)
                ) {
                    st.tabHistory
                } else {
                    st.tabHistory->Js_array2.concat([tabId])
                }
        }
    } else {
        st
    }
}

let removeTab = (st:state<'a>, tabId):state<'a> => {
    let newTabs = st.tabs->Js_array2.filter(t => t.id != tabId)
    let newTabHistory = st.tabHistory->Js_array2.filter(id => id != tabId)
    {
        ...st, 
        tabs: newTabs,
        activeTabId:
            if (newTabs->Js_array2.length == 0) {
                ""
            } else if (st.activeTabId == tabId) {
                newTabHistory->Belt_Array.get(newTabHistory->Js_array2.length-1)
                    ->Belt.Option.getWithDefault(newTabs[0].id)
            } else {
                st.activeTabId
            },
        tabHistory: newTabHistory,
    }
}

let useTabs = ():tabMethods<'a> => {
    let (state, setState) = React.useState(createEmptyState)

    let addTab = (~label:string, ~closable:bool, ~color:option<string>=?, ~data:'a, ~doOpen:bool=false, ()):promise<tabId> => promise(rlv => {
        setState(prev => {
            let (st, tabId) = prev->addTab(~label, ~closable, ~color?, ~data, ~doOpen, ())
            rlv(tabId)
            st
        })
    })

    let openTab = id => {
        setState(prev => prev->openTab(id))
    }

    let removeTab = id => {
        setState(prev => prev->removeTab(id))
    }

    let renderTabs = () => {
        let {tabs, activeTabId} = state
        if (tabs->Js_array2.length == 0) {
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
                    tabs->Js_array2.map(tab => {
                        <Tab 
                            key=tab.id 
                            value=tab.id 
                            label={
                                if (tab.closable) {
                                    <span style=ReactDOM.Style.make(~textTransform="none", ())>
                                        <span style=ReactDOM.Style.make(~marginRight="10px", ())>
                                            {React.string(tab.label)}
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
                                    React.string(tab.label)
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
        removeTab,
        tabs: state.tabs->Js.Array2.copy,
        activeTabId: state.activeTabId,
        renderTabs,

        updateTabs: modifier => setState(modifier)
    }
}