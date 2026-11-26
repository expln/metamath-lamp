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
* If title is None or blank, a default title is generated.
* closeTab must close the tab, asking the user for a confirmation if askConfirmation is true
* (only some tabs, e.g. editors, need a confirmation). It resolves to false if the user didn't confirm,
* and to an error if the tab doesn't exist or cannot be closed (Settings, Tabs, and MCP tabs cannot be closed).
* openTab must make the tab visible. It resolves to an error if the tab doesn't exist.
* */
let updateTabsApi: (
    ~listTabs:unit=>promise<array<tabInfo>>,
    ~createEditorTab:(~doOpen:bool, ~title:option<string>)=>promise<result<string,string>>,
    ~closeTab:(~tabId:string, ~askConfirmation:bool)=>promise<result<bool,string>>,
    ~openTab:string=>promise<result<unit,string>>,
) => unit
