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

let setLogApiCallsToConsoleRef:ref<option<api>> = ref(None)
let showInfoMsgRef:ref<option<api>> = ref(None)
let showErrMsgRef:ref<option<api>> = ref(None)

let api = {
    "setLogApiCallsToConsole": makeApiFuncFromRef(setLogApiCallsToConsoleRef),
    "showInfoMsg": makeApiFuncFromRef(showInfoMsgRef),
    "showErrMsg": makeApiFuncFromRef(showErrMsgRef),
    "editor": (editorId:option<int>) => {
        "getState": _ => Promise.resolve(errResp("Not implemented")),
        "proveBottomUp": _ => Promise.resolve(errResp("Not implemented")),
        "unifyAll": _ => Promise.resolve(errResp("Not implemented")),
        "addSteps": _ => Promise.resolve(errResp("Not implemented")),
        "updateSteps": _ => Promise.resolve(errResp("Not implemented")),
        "deleteSteps": _ => Promise.resolve(errResp("Not implemented")),
        "getTokenType": _ => Promise.resolve(errResp("Not implemented")),
        "substitute": _ => Promise.resolve(errResp("Not implemented")),
        "mergeDuplicatedSteps": _ => Promise.resolve(errResp("Not implemented")),
        "setContentIsHidden": _ => Promise.resolve(errResp("Not implemented")),
        "buildSyntaxTrees": _ => Promise.resolve(errResp("Not implemented")),
    }
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

let updateUiApi = (
    ~showInfoMsg:api,
    ~showErrMsg:api,
):unit => {
    showInfoMsgRef := Some(showInfoMsg)
    showErrMsgRef := Some(showErrMsg)
}