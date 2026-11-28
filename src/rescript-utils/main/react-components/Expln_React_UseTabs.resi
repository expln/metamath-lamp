open Expln_React_common

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

type state<'a>
let getTabs: state<'a> => array<tab<'a>>
let getActiveTabId: state<'a> => tabId
let tabExists: (state<'a>, tabId) => bool
/**
Returns a new random tab id for which `isTaken` returns false.
*/
let makeUniqueTabId: (tabId=>bool) => tabId
/**
Returns a new random id which is not used by any of the existing tabs.
*/
let makeNewTabId: state<'a> => tabId
/**
Adds a new tab.
If `id` is not specified, a new random id is generated.
If a tab with the specified `id` already exists, an exception is thrown.
If `singleton` is true and there is already a singleton tab with equal (structurally) data,
then a new tab is not added; the existing tab is opened if `doOpen` is true and its id is returned.
*/
let addTab: (
    state<'a>, ~id:tabId=?, ~icon:React.element=?, ~label:string, ~closable:bool, ~singleton:bool=?, 
    ~color:string=?, ~data:'a, ~doOpen:bool=?
) => (state<'a>, tabId)
let openTab: (state<'a>, tabId) => state<'a>
let setLabel: (state<'a>, tabId, string) => state<'a>
let removeTab: (state<'a>, tabId) => state<'a>

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

let useTabs: (
    ~beforeTabRemove:tab<'a>=>promise<bool>=?
) => tabMethods<'a>

