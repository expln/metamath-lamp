type apiResp = {
    "isOk": bool,
    "res": option<JSON.t>,
    "err": option<string>,
}

type api = JSON.t => promise<apiResp>

type editorApi = option<int> => {
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

let api: {
    "setLogApiCallsToConsole": api,
    "showInfoMsg": api,
    "showErrMsg": api,
    "editor" : editorApi
}

let makeApiFunc: (string, JSON.t=>promise<result<JSON.t,string>>) => api

let updateUiApi: ( ~showInfoMsg:api, ~showErrMsg:api, ) => unit