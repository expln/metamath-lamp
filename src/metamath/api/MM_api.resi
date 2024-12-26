type apiResp = {
    "isOk": bool,
    "res": option<JSON.t>,
    "err": option<string>,
}

type api = JSON.t => promise<apiResp>

type singleEditorApi = {
    "getState": api,
    "proveBottomUp": api,
    "unifyAll": api,
    "addSteps": api,
    "updateSteps": api,
    "deleteSteps": api,
    "getTokenType": api,
    "substitute": api,
    "mergeDuplicatedSteps": api,
    "setContentIsHidden": api,
    "buildSyntaxTrees": api,
}

type editorApi = option<int> => singleEditorApi

type macroApi = {
    "registerMacroModule": api,
    "unregisterMacroModule": api,
    "listRegisteredMacroModules": api,
    "listRegisteredMacrosInModule": api,
    "runMacro": api,
}

let api: {
    "setLogApiCallsToConsole": api,
    "showInfoMsg": api,
    "showErrMsg": api,
    "editor" : editorApi,
    "macro" : macroApi,
}

let makeApiFunc: (string, JSON.t=>promise<result<JSON.t,string>>) => api

let setUiApi: ( ~showInfoMsg:api, ~showErrMsg:api, ) => unit
let setEditorApi: editorApi => unit
let setMacroApi: macroApi => unit

let makeEmptySingleEditorApi: string => singleEditorApi