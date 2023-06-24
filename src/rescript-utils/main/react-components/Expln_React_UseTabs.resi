open Expln_React_common
open Expln_utils_promise

type tabId = string

type tab<'a> = {
    id:tabId,
    label: string,
    closable: bool,
    color:option<string>,
    data: 'a
}

type state<'a>
let getTabs: state<'a> => array<tab<'a>>
let addTab: (state<'a>, ~label:string, ~closable:bool, ~color:string=?, ~data:'a, ~doOpen:bool=?, ()) => (state<'a>, tabId)
let openTab: (state<'a>, tabId) => state<'a>
let removeTab: (state<'a>, tabId) => state<'a>

type tabMethods<'a> = {
    addTab: (~label:string, ~closable:bool, ~color:string=?, ~data:'a, ~doOpen:bool=?, ()) => promise<tabId>,
    openTab: tabId => unit,
    removeTab: tabId => unit,
    tabs: array<tab<'a>>,
    activeTabId: tabId,
    renderTabs: unit => reElem,

    updateTabs: (state<'a> => state<'a>) => unit
}

let useTabs: unit => tabMethods<'a>
