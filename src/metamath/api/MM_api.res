open Common

type apiResp = {
    "isOk": bool,
    "res": option<JSON.t>,
    "err": option<string>,
}

type api = JSON.t => promise<apiResp>

let okResp = (res:JSON.t):apiResp => {
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

let apiCallCnt = ref(0)

let logApiCallsToConsole = ref(false)

let makeApiFunc = (name:string, func:JSON.t=>promise<result<JSON.t,string>>):api => {
    params => {
        apiCallCnt := apiCallCnt.contents + 1
        let apiCallId = apiCallCnt.contents
        if (logApiCallsToConsole.contents) {
            Console.log2(`[${apiCallId->Belt.Int.toString}] <<< ${name}`, params)
        }
        func(params)->Promise.thenResolve(res => {
            let resp = switch res {
                | Error(msg) => errResp(msg)
                | Ok(json) => okResp(json)
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
    "deleteSteps": api,
    "getTokenType": api,
    "substitute": api,
    "mergeDuplicatedSteps": api,
    "setContentIsHidden": api,
    "buildSyntaxTrees": api,
}

type editorApi = option<int> => singleEditorApi

let makeEmptySingleEditorApi = (msg:string):singleEditorApi => {
    {
        "getState": _ => Promise.resolve(errResp(msg)),
        "proveBottomUp": _ => Promise.resolve(errResp(msg)),
        "unifyAll": _ => Promise.resolve(errResp(msg)),
        "addSteps": _ => Promise.resolve(errResp(msg)),
        "updateSteps": _ => Promise.resolve(errResp(msg)),
        "deleteSteps": _ => Promise.resolve(errResp(msg)),
        "getTokenType": _ => Promise.resolve(errResp(msg)),
        "substitute": _ => Promise.resolve(errResp(msg)),
        "mergeDuplicatedSteps": _ => Promise.resolve(errResp(msg)),
        "setContentIsHidden": _ => Promise.resolve(errResp(msg)),
        "buildSyntaxTrees": _ => Promise.resolve(errResp(msg)),
    }
}

type macroApi = {
    "registerMacroModule": api,
    "unregisterMacroModule": api,
    "listRegisteredMacroModules": api,
    "listRegisteredMacrosInModule": api,
    "runMacro": api,
}

let makeEmptyMacroApi = (msg:string):macroApi => {
    {
        "registerMacroModule": _ => Promise.resolve(errResp(msg)),
        "unregisterMacroModule": _ => Promise.resolve(errResp(msg)),
        "listRegisteredMacroModules": _ => Promise.resolve(errResp(msg)),
        "listRegisteredMacrosInModule": _ => Promise.resolve(errResp(msg)),
        "runMacro": _ => Promise.resolve(errResp(msg)),
    }
}

let setLogApiCallsToConsoleRef:ref<option<api>> = ref(None)
let showInfoMsgRef:ref<option<api>> = ref(None)
let showErrMsgRef:ref<option<api>> = ref(None)
let editorRef:ref<option<editorApi>> = ref(None)
let macroRef:ref<option<macroApi>> = ref(None)

let api = {
    "setLogApiCallsToConsole": makeApiFuncFromRef(setLogApiCallsToConsoleRef),
    "showInfoMsg": makeApiFuncFromRef(showInfoMsgRef),
    "showErrMsg": makeApiFuncFromRef(showErrMsgRef),
    "editor": (editorId:option<int>) => {
        switch editorRef.contents {
            | None => makeEmptySingleEditorApi("The editor API function is not defined.")
            | Some(func) => func(editorId)
        }
    },
    "macro": makeEmptyMacroApi("The macro API function is not defined."),
}

let setLogApiCallsToConsole = (params:JSON.t):promise<result<JSON.t,string>> => {
    switch JSON.Decode.bool(params) {
        | None => Promise.resolve(Error("The parameter of setLogApiCallsToConsole() must be a boolean."))
        | Some(bool) => {
            logApiCallsToConsole := bool
            Promise.resolve(Ok(JSON.Encode.null))
        }
    }
}

setLogApiCallsToConsoleRef := Some(makeApiFunc("setLogApiCallsToConsole", setLogApiCallsToConsole))

let setUiApi = (
    ~showInfoMsg:api,
    ~showErrMsg:api,
):unit => {
    showInfoMsgRef := Some(showInfoMsg)
    showErrMsgRef := Some(showErrMsg)
}

let setEditorApi = ( editorApi:editorApi ):unit => {
    editorRef:= Some(editorApi)
}