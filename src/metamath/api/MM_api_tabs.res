open MM_api

type tabInfo = {
    tabId:string,
    typ:string,
    title:string,
    isVisible:bool,
}

let tabInfoToJson = (tab:tabInfo):JSON.t => {
    Dict.fromArray([
        ("tabId", tab.tabId->JSON.Encode.string),
        ("type", tab.typ->JSON.Encode.string),
        ("title", tab.title->JSON.Encode.string),
        ("isVisible", tab.isVisible->JSON.Encode.bool),
    ])->JSON.Encode.object
}

let apiListTabs = (~listTabs:unit=>promise<array<tabInfo>>):promise<result<JSON.t,string>> => {
    listTabs()->Promise.thenResolve(tabs => Ok(tabs->Array.map(tabInfoToJson)->JSON.Encode.array))
}

let updateTabsApi = (
    ~listTabs:unit=>promise<array<tabInfo>>,
):unit => {
    setTabsApi({
        "list": makeApiFunc("tabs.list", _ => apiListTabs(~listTabs)),
    })
}
