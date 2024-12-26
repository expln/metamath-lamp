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

external apiInputToNullableObj: apiInput => Nullable.t<{..}> = "%identity"
external apiInputToJson: apiInput => JSON.t = "%identity"

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

let setMacroApi = ( macroApi:macroApi ):unit => {
    macroRef:= Some(macroApi)
}