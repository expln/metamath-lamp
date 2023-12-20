open MM_wrk_editor
open Expln_utils_promise
open Common

type api = (string,Js.Json.t) => promise<Js_json.t>

let apiRef:ref<option<api>> = ref(None)
let apiEntry = (funcName:string, params:Js_json.t):promise<Js_json.t> => {
    switch apiRef.contents {
        | None => promise(resolve => resolve(Js_json.null))
        | Some(api) => api(funcName, params)
    }
}

let funcNameGetState = "editor.getState"
let funcNameProveBottomUp = "editor.proveBottomUp"
let fun = {
    "editor": {
        "getState": funcNameGetState, 
        "proveBottomUp": funcNameProveBottomUp,
    }
}

let getAllSteps = (~state:editorState):Js_json.t => {
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
}

let getEditorState = (~state:editorState):promise<Js_json.t> => {
    let stmtIdToLabel = Belt_HashMapString.fromArray(
        state.stmts->Js_array2.map(stmt => (stmt.id, stmt.label))
    )
    promise(resolve => {
        resolve(
            Js_dict.fromArray([
                ("descr", state.descr->Js_json.string),
                ("varsText", state.varsText->Js_json.string),
                ("varsErr", state.varsErr->Belt.Option.map(Js_json.string)->Belt_Option.getWithDefault(Js_json.null)),
                ("vars", 
                    switch MM_wrk_ctx_data.textToVarDefs(state.varsText) {
                        | Error(_) => Js_json.null
                        | Ok(varDefs) => {
                            varDefs->Js_array2.map(varDef => {
                                varDef->Js_array2.map(Js_json.string)->Js.Json.array
                            })->Js.Json.array
                        }
                    }
                ),
                ("disjText", state.disjText->Js_json.string),
                ("disjErr", state.disjErr->Belt.Option.map(Js_json.string)->Belt_Option.getWithDefault(Js_json.null)),
                ("disj", 
                    state.disjText->multilineTextToNonEmptyLines->Js_array2.map(disjLine => {
                        disjLine->Js.String2.split(",")
                            ->Js_array2.map(Js_string2.trim)
                            ->Js.Array2.filter(str => str != "")
                            ->Js_array2.map(Js_json.string)
                            ->Js.Json.array
                    })->Js.Json.array
                ),
                ("steps", getAllSteps(~state)),
                ("selectedSteps", 
                    state.checkedStmtIds->Js_array2.map(((stmtId,_)) => stmtIdToLabel->Belt_HashMapString.get(stmtId))
                        ->Js_array2.filter(Belt.Option.isSome)
                        ->Js_array2.map(labelOpt => labelOpt->Belt.Option.getExn->Js_json.string)
                        ->Js.Json.array
                ),
            ])->Js_json.object_
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
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
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
                                        })->promiseMap(proved => {
                                            switch proved {
                                                | None => Js_json.null
                                                | Some(proved) => proved->Js_json.boolean
                                            }
                                        })
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
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
):api => {
    (funcName,paramsJson) => {
        if (funcName == funcNameGetState) {
            getEditorState(~state)
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
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
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