// apiInput can be anything. Setting "apiInput = int" just to make the compiler happy.
type apiInput = int

// apiOutput can be anything. Setting "apiOutput = int" just to make the compiler happy.
type apiOutput = int

type apiResp = {
    "isOk": bool,
    "res": option<apiOutput>,
    "err": option<string>,
}

type api = apiInput => promise<apiResp>

let okResp = (res:apiOutput):apiResp => {
    {
        "isOk": true,
        "res": Some(res),
        "err": None,
    }
}

let errResp = (msg:string):apiResp => {
    {
        "isOk": false,
        "res": None,
        "err": Some(msg),
    }
}

external apiInputToJson: apiInput => JSON.t = "%identity"
external apiInputToNullableObj: apiInput => Nullable.t<{..}> = "%identity"

let apiInputToObjExn = (apiInput:apiInput, msg:string):{..} => {
    switch apiInput->apiInputToNullableObj->Nullable.toOption {
        | None => JsError.throwWithMessage(msg)
        | Some(obj) => obj
    }
}

external makeApiOutput: 'a => apiOutput = "%identity"

let apiCallCnt = ref(0)

let logApiCallsToConsole = ref(false)

let makeApiFunc = (name:string, func:apiInput=>promise<result<'a,string>>): api => {
    params => {
        apiCallCnt := apiCallCnt.contents + 1
        let apiCallId = apiCallCnt.contents
        if (logApiCallsToConsole.contents) {
            Console.log2(`[${apiCallId->Belt.Int.toString}] <<< ${name}`, params)
        }
        func(params)->Promise.thenResolve(res => {
            let resp = switch res {
                | Error(msg) => errResp(msg)
                | Ok(res) => res->makeApiOutput->okResp
            }
            if (logApiCallsToConsole.contents) {
                Console.log2(`[${apiCallId->Belt.Int.toString}] >>> `, resp)
            }
            resp
        })
    }
}

let makeApiFuncFromRef = (ref:ref<option<api>>):api => {
    params => {
        switch ref.contents {
            | None => Promise.resolve(errResp("The API function is not defined."))
            | Some(func) => func(params)
        }
    }
}

type singleEditorApi = {
    "getState": api,
    "proveBottomUp": api,
    "unifyAll": api,
    "addSteps": api,
    "updateSteps": api,
    "renameSteps": api,
    "deleteSteps": api,
    "markStepsChecked": api,
    "setDisjoints": api,
    "setDescription": api,
    "resetEditorContent": api,
    "getTokenType": api,
    "substitute": api,
    "mergeDuplicatedSteps": api,
    "setContentIsHidden": api,
    "buildSyntaxTrees": api,
    "findAsrtsByUnif": api,
    "addAsrtByLabel": api,
}

type editorApi = option<string> => singleEditorApi

let makeEmptySingleEditorApi = (msg:string):singleEditorApi => {
    {
        "getState": _ => Promise.resolve(errResp(msg)),
        "proveBottomUp": _ => Promise.resolve(errResp(msg)),
        "unifyAll": _ => Promise.resolve(errResp(msg)),
        "addSteps": _ => Promise.resolve(errResp(msg)),
        "updateSteps": _ => Promise.resolve(errResp(msg)),
        "renameSteps": _ => Promise.resolve(errResp(msg)),
        "deleteSteps": _ => Promise.resolve(errResp(msg)),
        "markStepsChecked": _ => Promise.resolve(errResp(msg)),
        "setDisjoints": _ => Promise.resolve(errResp(msg)),
        "setDescription": _ => Promise.resolve(errResp(msg)),
        "resetEditorContent": _ => Promise.resolve(errResp(msg)),
        "getTokenType": _ => Promise.resolve(errResp(msg)),
        "substitute": _ => Promise.resolve(errResp(msg)),
        "mergeDuplicatedSteps": _ => Promise.resolve(errResp(msg)),
        "setContentIsHidden": _ => Promise.resolve(errResp(msg)),
        "buildSyntaxTrees": _ => Promise.resolve(errResp(msg)),
        "findAsrtsByUnif": _ => Promise.resolve(errResp(msg)),
        "addAsrtByLabel": _ => Promise.resolve(errResp(msg)),
    }
}

type macroApi = {
    "registerMacroModule": api,
    "unregisterMacroModule": api,
    "listRegisteredMacroModules": api,
    "listRegisteredMacrosInModule": api,
    "runMacro": api,
}

type settingsApi = {
    "setMarkFirstProvableStepAsGoal": api,
    "getMarkFirstProvableStepAsGoal": api,
}

type uiApi = {
    "showInfoMsg": api,
    "showErrMsg": api,
    "multilineTextInput": api,
}

type explorerApi = {
    "findAssertions": api,
}

type contextApi = {
    "load": api,
}

type tabsApi = {
    "list": api,
    "createEditorTab": api,
    "close": api,
    "open": api,
    "rename": api,
}

let setLogApiCallsToConsoleRef:ref<option<api>> = ref(None)
let uiRef:ref<option<uiApi>> = ref(None)
let editorRef:ref<option<editorApi>> = ref(None)
let macroRef:ref<option<macroApi>> = ref(None)
let settingsRef:ref<option<settingsApi>> = ref(None)
let explorerRef:ref<option<explorerApi>> = ref(None)
let contextRef:ref<option<contextApi>> = ref(None)
let tabsRef:ref<option<tabsApi>> = ref(None)

let makeSingleUiApi = (methodGetter:uiApi=>api):api => {
    apiInput => {
        switch uiRef.contents {
            | None => Promise.resolve(errResp("The UI API function is not defined."))
            | Some(uiApi) => methodGetter(uiApi)(apiInput)
        }
    }
}

let makeSingleMacroApi = (methodGetter:macroApi=>api):api => {
    apiInput => {
        switch macroRef.contents {
            | None => Promise.resolve(errResp("The macro API function is not defined."))
            | Some(macroApi) => methodGetter(macroApi)(apiInput)
        }
    }
}

let makeSingleSettingsApi = (methodGetter:settingsApi=>api):api => {
    apiInput => {
        switch settingsRef.contents {
            | None => Promise.resolve(errResp("The settings API function is not defined."))
            | Some(settingsApi) => methodGetter(settingsApi)(apiInput)
        }
    }
}

let makeSingleExplorerApi = (methodGetter:explorerApi=>api):api => {
    apiInput => {
        switch explorerRef.contents {
            | None => Promise.resolve(errResp("The explorer API function is not defined."))
            | Some(explorerApi) => methodGetter(explorerApi)(apiInput)
        }
    }
}

let makeSingleContextApi = (methodGetter:contextApi=>api):api => {
    apiInput => {
        switch contextRef.contents {
            | None => Promise.resolve(errResp("The context API function is not defined."))
            | Some(contextApi) => methodGetter(contextApi)(apiInput)
        }
    }
}

let makeSingleTabsApi = (methodGetter:tabsApi=>api):api => {
    apiInput => {
        switch tabsRef.contents {
            | None => Promise.resolve(errResp("The tabs API function is not defined."))
            | Some(tabsApi) => methodGetter(tabsApi)(apiInput)
        }
    }
}

let makeUiApi = ():uiApi => {
    {
        "showInfoMsg": makeSingleUiApi(a => a["showInfoMsg"]),
        "showErrMsg": makeSingleUiApi(a => a["showErrMsg"]),
        "multilineTextInput": makeSingleUiApi(a => a["multilineTextInput"]),
    }
}

let makeMacroApi = ():macroApi => {
    {
        "registerMacroModule": makeSingleMacroApi(a => a["registerMacroModule"]),
        "unregisterMacroModule": makeSingleMacroApi(a => a["unregisterMacroModule"]),
        "listRegisteredMacroModules": makeSingleMacroApi(a => a["listRegisteredMacroModules"]),
        "listRegisteredMacrosInModule": makeSingleMacroApi(a => a["listRegisteredMacrosInModule"]),
        "runMacro": makeSingleMacroApi(a => a["runMacro"]),
    }
}

let makeSettingsApi = ():settingsApi => {
    {
        "setMarkFirstProvableStepAsGoal": makeSingleSettingsApi(a => a["setMarkFirstProvableStepAsGoal"]),
        "getMarkFirstProvableStepAsGoal": makeSingleSettingsApi(a => a["getMarkFirstProvableStepAsGoal"]),
    }
}

let makeExplorerApi = ():explorerApi => {
    {
        "findAssertions": makeSingleExplorerApi(a => a["findAssertions"]),
    }
}

let makeContextApi = ():contextApi => {
    {
        "load": makeSingleContextApi(a => a["load"]),
    }
}

let makeTabsApi = ():tabsApi => {
    {
        "list": makeSingleTabsApi(a => a["list"]),
        "createEditorTab": makeSingleTabsApi(a => a["createEditorTab"]),
        "close": makeSingleTabsApi(a => a["close"]),
        "open": makeSingleTabsApi(a => a["open"]),
        "rename": makeSingleTabsApi(a => a["rename"]),
    }
}

// Editor ids used to be integers. Editors created before that keep their old ids as strings ("0", "1", ...).
// This lets macros which pass an old integer id keep working.
// The id comes from JS code, so it may be of any type: anything other than undefined or null is converted to a string,
// so the result is always either None or Some(string).
let normalizeTabId: option<string> => option<string> = %raw(`
    id => id === undefined || id === null ? undefined : String(id)
`)

let api = {
    "setLogApiCallsToConsole": makeApiFuncFromRef(setLogApiCallsToConsoleRef),
    "ui": makeUiApi(),
    "editor": (tabId:option<string>) => {
        switch editorRef.contents {
            | None => makeEmptySingleEditorApi("The editor API function is not defined.")
            | Some(func) => func(normalizeTabId(tabId))
        }
    },
    "macro": makeMacroApi(),
    "settings": makeSettingsApi(),
    "explorer": makeExplorerApi(),
    "context": makeContextApi(),
    "tabs": makeTabsApi(),
}

let setLogApiCallsToConsole = (params:apiInput):promise<result<unit,string>> => {
    switch params->apiInputToJson->JSON.Decode.bool {
        | None => Promise.resolve(Error("The parameter of setLogApiCallsToConsole() must be a boolean."))
        | Some(bool) => {
            logApiCallsToConsole := bool
            Promise.resolve(Ok(()))
        }
    }
}

setLogApiCallsToConsoleRef := Some(makeApiFunc("setLogApiCallsToConsole", setLogApiCallsToConsole))

let setUiApi = ( uiApi:uiApi ):unit => {
    uiRef:= Some(uiApi)
}

let setEditorApi = ( editorApi:editorApi ):unit => {
    editorRef:= Some(editorApi)
}

let setMacroApi = ( macroApi:macroApi ):unit => {
    macroRef:= Some(macroApi)
}

let setSettingsApi = ( settingsApi:settingsApi ):unit => {
    settingsRef:= Some(settingsApi)
}

let setExplorerApi = ( explorerApi:explorerApi ):unit => {
    explorerRef:= Some(explorerApi)
}

let setContextApi = ( contextApi:contextApi ):unit => {
    contextRef:= Some(contextApi)
}

let setTabsApi = ( tabsApi:tabsApi ):unit => {
    tabsRef:= Some(tabsApi)
}
