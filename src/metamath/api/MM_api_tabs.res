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

// params: {tabId:string, askConfirmation?:bool}. Resolves to true if the tab was closed.
let apiCloseTab = (
    ~params:apiInput,
    ~closeTab:(~tabId:string, ~askConfirmation:bool)=>promise<result<bool,string>>,
):promise<result<JSON.t,string>> => {
    let parsed = Raw_js_utils.invokeExnFunc("Parsing parameters of tabs.close", () => {
        let params = params->apiInputToObjExn("got empty parameters")
        (
            Raw_js_utils.reqStrExn(params["tabId"], "'tabId' must be a string"),
            Raw_js_utils.optBoolExn(params["askConfirmation"], "'askConfirmation' must be a boolean")
                ->Option.getOr(true),
        )
    })
    switch parsed {
        | Error(msg) => Promise.resolve(Error(msg))
        | Ok((tabId, askConfirmation)) => {
            closeTab(~tabId, ~askConfirmation)->Promise.thenResolve(res => res->Result.map(JSON.Encode.bool))
        }
    }
}

// params: {tabId:string}.
let apiOpenTab = (~params:apiInput, ~openTab:string=>promise<result<unit,string>>):promise<result<JSON.t,string>> => {
    let parsed = Raw_js_utils.invokeExnFunc("Parsing parameters of tabs.open", () => {
        let params = params->apiInputToObjExn("got empty parameters")
        Raw_js_utils.reqStrExn(params["tabId"], "'tabId' must be a string")
    })
    switch parsed {
        | Error(msg) => Promise.resolve(Error(msg))
        | Ok(tabId) => openTab(tabId)->Promise.thenResolve(res => res->Result.map(() => JSON.Encode.null))
    }
}

let updateTabsApi = (
    ~listTabs:unit=>promise<array<tabInfo>>,
    ~createEditorTab:(~doOpen:bool, ~title:option<string>)=>promise<result<string,string>>,
    ~closeTab:(~tabId:string, ~askConfirmation:bool)=>promise<result<bool,string>>,
    ~openTab:string=>promise<result<unit,string>>,
):unit => {
    setTabsApi({
        "list": makeApiFunc("tabs.list", _ => apiListTabs(~listTabs)),
        "createEditorTab": makeApiFunc("tabs.createEditorTab", params => apiCreateEditorTab(~params, ~createEditorTab)),
        "close": makeApiFunc("tabs.close", params => apiCloseTab(~params, ~closeTab)),
        "open": makeApiFunc("tabs.open", params => apiOpenTab(~params, ~openTab)),
    })
}
