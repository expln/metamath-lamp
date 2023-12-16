open MM_wrk_editor

type api = (string,Js.Json.t) => Js_json.t

let apiRef:ref<option<api>> = ref(None)
let api = ():option<api> => apiRef.contents

let getAllLabelsStr = "getAllLabels"
let proveBottomUpStr = "proveBottomUp"
let fun = {
    "getAllLabels": getAllLabelsStr,
    "proveBottomUp": proveBottomUpStr,
}

let getAllLabels = (~state:editorState):Js_json.t => {
    state.stmts->Js.Array2.map(stmt => stmt.label->Js.Json.string)->Js.Json.array
}

type proveBottomUpParams = {
    stepLabel:option<string>,
}
let proveBottomUp = (
    ~paramsJson:Js_json.t,
    ~state:editorState,
    ~showError:string=>unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:(MM_wrk_editor.stmtId, MM_provers.bottomUpProverParams)=>unit,
):Js_json.t => {
    if (!canStartProvingBottomUp) {
        showError("Cannot start proving bottom-up because either there are syntax errors in the editor or edit is in progress.")
    } else {
        open Expln_utils_jsonParse
        let parseResult = fromJson(paramsJson, asObj(_,d=>{
            {
                stepLabel: d->strOpt("stepLabel", ())
            }
        }, ()), ())
        switch parseResult {
            | Error(msg) => showError(msg)
            | Ok(params) => {
                switch params.stepLabel {
                    | None => showError(`"stepLabel" parameter must not be empty.`)
                    | Some(stepLabel) => {
                        switch state.stmts->Js.Array2.find(stmt => stmt.label == stepLabel) {
                            | None => showError(`Cannot find a step with label '${stepLabel}'`)
                            | Some(stmt) => {
                                startProvingBottomUp(
                                    stmt.id,
                                    {
                                        asrtLabel: None,
                                        maxSearchDepth: 4,
                                        lengthRestrict: Less,
                                        allowNewDisjForExistingVars: true,
                                        allowNewStmts: true,
                                        allowNewVars: false,
                                        args0: [],
                                        args1: [],
                                        maxNumberOfBranches: None,
                                    }
                                )
                            }
                        }
                    }
                }
            }
        }
    }
    Js.Json.null
}

let makeShowError = (funcName, showError:string=>unit):(string=>unit) => msg => {
    showError(`${funcName}: ${msg}`)
}

let makeEditorApi = (
    ~state:editorState,
    ~showError:string=>unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:(MM_wrk_editor.stmtId, MM_provers.bottomUpProverParams)=>unit,
):api => (funcName,paramsJson) => {
    if (funcName == getAllLabelsStr) {
        getAllLabels(~state)
    } else if (funcName == proveBottomUpStr) {
        proveBottomUp(
            ~paramsJson, 
            ~state, 
            ~showError=makeShowError(funcName,showError),
            ~canStartProvingBottomUp,
            ~startProvingBottomUp,
        )
    } else {
        showError(`Unknown api function ${funcName}`)
        Js.Json.null
    }
}

let updateEditorApi = (
    ~state:editorState,
    ~showError:string=>unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:(MM_wrk_editor.stmtId, MM_provers.bottomUpProverParams)=>unit,
):unit => {
    apiRef := Some(
        makeEditorApi(
            ~state,
            ~showError,
            ~canStartProvingBottomUp,
            ~startProvingBottomUp,
        )
    )
}