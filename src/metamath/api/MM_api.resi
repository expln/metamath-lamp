type apiInput
type apiOutput

type apiResp = {
    "isOk": bool,
    "res": option<apiOutput>,
    "err": option<string>,
}

type api = apiInput => promise<apiResp>

let apiInputToJson: apiInput => JSON.t
let apiInputToNullableObj: apiInput => Nullable.t<{..}>
let apiInputToObjExn: (apiInput, string) => {..}

let makeApiFunc: (string, apiInput=>promise<result<'a,string>>) => api

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
    "multilineTextInput": api,
    "editor" : editorApi,
    "macro" : macroApi,
}

let setUiApi: ( ~showInfoMsg:api, ~showErrMsg:api, ~multilineTextInput:api, ) => unit
let setEditorApi: editorApi => unit
let setMacroApi: macroApi => unit

let makeEmptySingleEditorApi: string => singleEditorApi