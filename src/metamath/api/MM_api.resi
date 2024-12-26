type apiInput
type apiOutput

type apiResp = {
    "isOk": bool,
    "res": option<apiOutput>,
    "err": option<string>,
}

type api = apiInput => promise<apiResp>

let apiInputToNullableObj: apiInput => Nullable.t<{..}>
let apiInputToJson: apiInput => JSON.t

let makeApiFunc: (string, apiInput=>promise<result<'a,string>>) => api

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

let setUiApi: ( ~showInfoMsg:api, ~showErrMsg:api, ) => unit
let setEditorApi: editorApi => unit
let setMacroApi: macroApi => unit

let makeEmptySingleEditorApi: string => singleEditorApi