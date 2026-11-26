type tabInfo = {
    tabId:string,
    // One of: settings, tabs, editor, explorer, assertion, db-info, mcp.
    typ:string,
    title:string,
    // True only for the active tab.
    isVisible:bool,
}

/*
* listTabs must return all tabs in the order they are displayed, including Settings and Tabs.
* */
let updateTabsApi: (
    ~listTabs:unit=>promise<array<tabInfo>>,
) => unit
