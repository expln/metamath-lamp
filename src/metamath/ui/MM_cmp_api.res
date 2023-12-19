open MM_wrk_editor
open Expln_utils_promise

type api = (string,Js.Json.t) => promise<Js_json.t>

let apiRef:ref<option<api>> = ref(None)
let apiEntry = (funcName:string, params:Js_json.t):promise<Js_json.t> => {
    switch apiRef.contents {
        | None => promise(resolve => resolve(Js_json.null))
        | Some(api) => api(funcName, params)
    }
}

let funcNameGetAllSteps = "editor.getAllSteps"
let funcNameProveBottomUp = "editor.proveBottomUp"
let fun = {
    "editor": {
        "getAllSteps": funcNameGetAllSteps, 
        "proveBottomUp": funcNameProveBottomUp,
    }
}

let getAllSteps = (~state:editorState):promise<Js_json.t> => {
    promise(resolve => {
        resolve(
            state.stmts->Js.Array2.map(stmt => {
                Js_dict.fromArray([
                    ("id", stmt.id->Js_json.string),
                    ("label", stmt.label->Js_json.string),
                    ("isHyp", (stmt.typ == E)->Js_json.boolean),
                    ("jstf", stmt.jstfText->Js_json.string),
                    (
                        "jstfParsed", 
                        stmt.jstfText->MM_wrk_editor.parseJstf->Belt.Result.mapWithDefault(
                            Js_json.null, 
                            (jstf:option<MM_statements_dto.jstf>) => {
                                switch jstf {
                                    | None => Js_json.null
                                    | Some(jstf) => {
                                        Js_dict.fromArray([
                                            ("args", jstf.args->Js_array2.map(Js_json.string)->Js_json.array),
                                            ("asrt", jstf.label->Js_json.string),
                                        ])->Js_json.object_
                                    }
                                }
                            }
                        )
                    ),
                    ("stmt", stmt.cont->MM_wrk_editor.contToStr->Js_json.string),
                ])->Js_json.object_
            })->Js.Json.array
        )
    })
}

let labelsToExprs = (st:editorState, labels:array<string>):result<array<MM_context.expr>,string> => {
    labels->Js_array2.reduce(
        (res,label) => {
            switch res {
                | Error(_) => res
                | Ok(arr) => {
                    switch st->editorGetStmtByLabel(label) {
                        | None => Error(`Cannot find a step by label '${label}'`)
                        | Some(stmt) => {
                            switch stmt.expr {
                                | None => Error(`Internal error: the step with label '${label}' doesn't have expr.`)
                                | Some(expr) => {
                                    arr->Js_array2.push(expr)->ignore
                                    res
                                }
                            }
                        }
                    }
                }
            }
        },
        Ok([])
    )
}

type proveBottomUpApiParams = {
    delayBeforeStartMs:option<int>,
    stepToProve:string,
    debugLevel:option<int>,
    args0:array<string>,
    args1:array<string>,
    frmsToUse:option<array<string>>,
    maxSearchDepth:int,
    lengthRestrict:string,
    allowNewStmts:bool,
    allowNewVars:bool,
    allowNewDisjForExistingVars:bool,
    maxNumberOfBranches:option<int>,
    selectFirstFoundProof:option<bool>,
}
type proverParams = {
    delayBeforeStartMs:int,
    stmtId: MM_wrk_editor.stmtId,
    debugLevel:int,
    bottomUpProverParams: MM_provers.bottomUpProverParams,
    selectFirstFoundProof:bool,
}
let proveBottomUp = (
    ~paramsJson:Js_json.t,
    ~state:editorState,
    ~showError:string=>promise<Js_json.t>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>unit,
):promise<Js_json.t> => {
    if (!canStartProvingBottomUp) {
        showError("Cannot start proving bottom-up because either there are syntax errors in the editor or edit is in progress.")
    } else {
        open Expln_utils_jsonParse
        let parseResult:result<proveBottomUpApiParams,string> = fromJson(paramsJson, asObj(_, d=>{
            {
                delayBeforeStartMs: d->intOpt("delayBeforeStartMs", ()),
                stepToProve: d->str("stepToProve", ()),
                debugLevel: d->intOpt("debugLevel", ()),
                args0: d->arr("args0", asStr(_, ()), ()),
                args1: d->arr("args1", asStr(_, ()), ()),
                frmsToUse: d->arrOpt("frmsToUse", asStr(_, ()), ()),
                maxSearchDepth: d->int("maxSearchDepth", ()),
                lengthRestrict: d->str("lengthRestrict", ~validator = str => {
                    switch MM_provers.lengthRestrictFromStr(str) {
                        | Some(_) => Ok(str)
                        | None => Error(`lengthRestrict must be one of: No, LessEq, Less.`)
                    }
                }, ()),
                allowNewStmts: d->bool("allowNewStmts", ()),
                allowNewVars: d->bool("allowNewVars", ()),
                allowNewDisjForExistingVars: d->bool("allowNewDisjForExistingVars", ()),
                maxNumberOfBranches: d->intOpt("maxNumberOfBranches", ()),
                selectFirstFoundProof: d->boolOpt("selectFirstFoundProof", ()),
            }
        }, ()), ())
        switch parseResult {
            | Error(msg) => showError(msg)
            | Ok(apiParams) => {
                switch state.stmts->Js.Array2.find(stmt => stmt.label == apiParams.stepToProve) {
                    | None => showError(`Cannot find a step with label '${apiParams.stepToProve}'`)
                    | Some(stmtToProve) => {
                        switch state->labelsToExprs(apiParams.args0) {
                            | Error(msg) => showError(msg)
                            | Ok(args0) => {
                                switch state->labelsToExprs(apiParams.args1) {
                                    | Error(msg) => showError(msg)
                                    | Ok(args1) => {
                                        startProvingBottomUp({
                                            delayBeforeStartMs:
                                                apiParams.delayBeforeStartMs->Belt_Option.getWithDefault(1000),
                                            stmtId: stmtToProve.id,
                                            debugLevel: apiParams.debugLevel->Belt_Option.getWithDefault(0),
                                            bottomUpProverParams: {
                                                asrtLabel: None,
                                                args0,
                                                args1,
                                                frmsToUse: apiParams.frmsToUse,
                                                maxSearchDepth: apiParams.maxSearchDepth,
                                                lengthRestrict: 
                                                    apiParams.lengthRestrict->MM_provers.lengthRestrictFromStrExn,
                                                allowNewDisjForExistingVars: apiParams.allowNewDisjForExistingVars,
                                                allowNewStmts: apiParams.allowNewStmts,
                                                allowNewVars: apiParams.allowNewVars,
                                                maxNumberOfBranches: apiParams.maxNumberOfBranches,
                                            },
                                            selectFirstFoundProof:
                                                apiParams.selectFirstFoundProof->Belt_Option.getWithDefault(false),
                                        })
                                        promise(resolve => resolve(Js_json.null))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let makeShowError = (funcName, showError:string=>unit):(string=>promise<Js_json.t>) => msg => {
    showError(`${funcName}: ${msg}`)
    promise(resolve => resolve(Js_json.null))
}

let makeEditorApi = (
    ~state:editorState,
    ~showError:string=>unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>unit,
):api => {
    (funcName,paramsJson) => {
        if (funcName == funcNameGetAllSteps) {
            getAllSteps(~state)
        } else if (funcName == funcNameProveBottomUp) {
            proveBottomUp(
                ~paramsJson, 
                ~state, 
                ~showError=makeShowError(funcName,showError),
                ~canStartProvingBottomUp,
                ~startProvingBottomUp,
            )
        } else {
            showError(`Unknown api function ${funcName}`)
            promise(resolve => resolve(Js_json.null))
        }
    }
}

let updateEditorApi = (
    ~state:editorState,
    ~showError:string=>unit,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>unit,
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