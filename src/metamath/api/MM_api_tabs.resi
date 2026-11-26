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
* createEditorTab must create a new empty editor tab, open it if doOpen is true, and resolve to its id
* when the new editor can be used via the editor API.
* */
let updateTabsApi: (
    ~listTabs:unit=>promise<array<tabInfo>>,
    ~createEditorTab:(~doOpen:bool)=>promise<result<string,string>>,
) => unit
