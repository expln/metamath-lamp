open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type tabId = string

type tab<'a> = {
    id:tabId,
    label: string,
    closable: bool,
    data: 'a
}

type state<'a> = {
    nextId: int,
    tabs: array<tab<'a>>,
    activeTabId: tabId
}

type tabMethods<'a> = {
    addTab: (~label:string, ~closable:bool, ~data:'a) => promise<tabId>,
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
    }
}

let getNextId = st => {
    ({...st, nextId: st.nextId+1}, st.nextId->Belt_Int.toString)
}

let getTabs = (st:state<'a>) => st.tabs

let addTab = (st, ~label:string, ~closable:bool, ~data:'a) => {
    let (st, newId) = st->getNextId
    let newTabs = st.tabs->Js_array2.concat([{id:newId, label, closable, data}])
    (
        {
            ...st,
            tabs: newTabs,
            activeTabId: if (newTabs->Js_array2.length == 1) {newId} else {st.activeTabId}
        },
        newId
    )
}

let openTab = (st:state<'a>, tabId):state<'a> => {
    if (st.tabs->Js_array2.some(t => t.id == tabId)) {
        {...st, activeTabId:tabId}
    } else {
        st
    }
}

let removeTab = (st:state<'a>, tabId):state<'a> => {
    let newTabs = st.tabs->Js_array2.filter(t => t.id != tabId)
    {
        ...st, 
        tabs: newTabs,
        activeTabId:
            if (newTabs->Js_array2.length == 0) {
                ""
            } else if (st.activeTabId == tabId) {
                newTabs[0].id
            } else {
                st.activeTabId
            }
    }
}

let useTabs = ():tabMethods<'a> => {
    let (state, setState) = React.useState(createEmptyState)

    let addTab = (~label:string, ~closable:bool, ~data:'a):promise<tabId> => promise(rlv => {
        setState(prev => {
            let (st, tabId) = prev->addTab(~label, ~closable, ~data)
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
            <Col>
                <Tabs key=activeTabId value=activeTabId variant=#scrollable onChange={(_,id)=>openTab(id)} >
                    {React.array(
                        tabs->Js_array2.map(tab => {
                            <Tab key=tab.id value=tab.id label={
                                if (tab.closable) {
                                    <span>
                                        {React.string(tab.label)}
                                        <IconButton component="div" 
                                                    onClick={evt => {
                                                        ReactEvent.Synthetic.stopPropagation(evt)
                                                        removeTab(tab.id)
                                                    }} >
                                            <Icons.Clear />
                                        </IconButton>
                                    </span>
                                } else {
                                    React.string(tab.label)
                                }
                            }/>
                        })
                    )}
                </Tabs>
            </Col>
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