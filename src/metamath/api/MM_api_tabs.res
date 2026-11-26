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

// params may be omitted: {open?:bool, title?:string}.
let apiCreateEditorTab = (
    ~params:apiInput,
    ~createEditorTab:(~doOpen:bool, ~title:option<string>)=>promise<result<string,string>>,
):promise<result<JSON.t,string>> => {
    let parsed = Raw_js_utils.invokeExnFunc("Parsing parameters of tabs.createEditorTab", () => {
        switch params->apiInputToNullableObj->Nullable.toOption {
            | None => (true, None)
            | Some(params) => (
                Raw_js_utils.optBoolExn(params["open"], "'open' must be a boolean")->Option.getOr(true),
                Raw_js_utils.optStrExn(params["title"], "'title' must be a string"),
            )
        }
    })
    switch parsed {
        | Error(msg) => Promise.resolve(Error(msg))
        | Ok((doOpen, title)) => {
            createEditorTab(~doOpen, ~title)->Promise.thenResolve(res => res->Result.map(JSON.Encode.string))
        }
    }
}

let updateTabsApi = (
    ~listTabs:unit=>promise<array<tabInfo>>,
    ~createEditorTab:(~doOpen:bool, ~title:option<string>)=>promise<result<string,string>>,
):unit => {
    setTabsApi({
        "list": makeApiFunc("tabs.list", _ => apiListTabs(~listTabs)),
        "createEditorTab": makeApiFunc("tabs.createEditorTab", params => apiCreateEditorTab(~params, ~createEditorTab)),
    })
}
