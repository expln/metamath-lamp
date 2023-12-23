open MM_wrk_editor
open Expln_utils_promise
open Common
open MM_context
open MM_syntax_tree
open MM_wrk_editor_substitution

type apiResp = {
    "isOk": bool,
    "res": option<Js_json.t>,
    "err": option<string>,
}

type api = Js.Json.t => promise<apiResp>

let okResp = (res:Js_json.t):apiResp => {
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

let rec syntaxTreeNodeToJson = (ctx:mmContext, node:syntaxTreeNode):Js_json.t => {
    Js_dict.fromArray([
        ("nodeType", "expr"->Js_json.string),
        ("exprType", ctx->ctxIntToSymExn(node.typ)->Js_json.string),
        ("label", node.label->Js_json.string),
        ("children", node.children->Js.Array2.map(childNodeToJson(ctx,_))->Js_json.array ),
    ])->Js_json.object_ 
} and childNodeToJson = (ctx:mmContext, node:childNode):Js_json.t => {
    switch node {
        | Subtree(subtree) => syntaxTreeNodeToJson(ctx, subtree)
        | Symbol({sym, isVar}) => {
            Js_dict.fromArray([
                ("nodeType", "sym"->Js_json.string),
                ("sym", sym->Js_json.string),
                ("isVar", isVar->Js_json.boolean),
            ])->Js_json.object_ 
        }
    }
}

let syntaxTreeToJson = (state:editorState, stmt:userStmt):Js_json.t => {
    switch state.wrkCtx {
        | None => Js_json.null
        | Some(wrkCtx) => {
            switch stmt.cont {
                | Text(_) => Js_json.null
                | Tree({exprTyp, root}) => {
                    Js_dict.fromArray([
                        ("exprType", exprTyp->Js_json.string),
                        ("root", syntaxTreeNodeToJson(wrkCtx, root)),
                    ])->Js_json.object_
                }
            }
        }
    }
}

let getAllSteps = (~state:editorState):Js_json.t => {
    state.stmts->Js.Array2.map(stmt => {
        Js_dict.fromArray([
            ("id", stmt.id->Js_json.string),
            ("status", 
                switch stmt.proofStatus {
                    | None => Js_json.null
                    | Some(proofStatus) => {
                        switch proofStatus {
                            | Ready => "v"
                            | Waiting => "~"
                            | NoJstf => "?"
                            | JstfIsIncorrect => "x"
                        }->Js_json.string
                    }
                }
            ),
            ("label", stmt.label->Js_json.string),
            ("isHyp", (stmt.typ == E)->Js_json.boolean),
            ("isGoal", stmt.isGoal->Js_json.boolean),
            ("jstfText", stmt.jstfText->Js_json.string),
            (
                "jstf", 
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
            ("tree", syntaxTreeToJson(state, stmt)),
            ("stmtErr", 
                switch stmt.stmtErr {
                    | None => Js_json.null
                    | Some({ code, msg }) => {
                        Js_dict.fromArray([
                            ("code", code->Belt.Int.toFloat->Js_json.number),
                            ("msg", msg->Js_json.string),
                        ])->Js_json.object_
                    }
                }
            ),
            ("unifErr", stmt.unifErr->Belt.Option.map(Js_json.string)->Belt.Option.getWithDefault(Js_json.null)),
            ("syntaxErr", stmt.syntaxErr->Belt.Option.map(Js_json.string)->Belt.Option.getWithDefault(Js_json.null)),
        ])->Js_json.object_
    })->Js.Json.array
}

let getEditorState = (~state:editorState):promise<result<Js_json.t,string>> => {
    let stmtIdToLabel = Belt_HashMapString.fromArray(
        state.stmts->Js_array2.map(stmt => (stmt.id, stmt.label))
    )
    promiseResolved(Ok(
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
    ))
}

let getTokenType = (
    ~paramsJson:Js_json.t,
    ~state:editorState,
):promise<result<Js_json.t,string>> => {
    switch state.wrkCtx {
        | None => promiseResolved(Error("Cannot determine token type because the editor contains errors."))
        | Some(wrkCtx) => {
            switch Js_json.decodeString(paramsJson) {
                | None => promiseResolved(Error("The parameter of getTokenType() must me a string."))
                | Some(token) => {
                    promiseResolved(Ok(
                        switch wrkCtx->getTokenType(token) {
                            | None => Js_json.null
                            | Some(tokenType) => {
                                switch tokenType {
                                    | C => "c"
                                    | V => "v"
                                    | F => "f"
                                    | E => "e"
                                    | A => "a"
                                    | P => "p"
                                }->Js_json.string
                            }
                        }
                    ))
                }
            }
        }
    }
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
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
):promise<result<Js_json.t,string>> => {
    if (!canStartProvingBottomUp) {
        promiseResolved(Error(
            "Cannot start proving bottom-up because either there are syntax errors in the editor" 
                ++ " or edit is in progress."
        ))
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
            | Error(msg) => promiseResolved(Error(msg))
            | Ok(apiParams) => {
                switch state.stmts->Js.Array2.find(stmt => stmt.label == apiParams.stepToProve) {
                    | None => promiseResolved(Error(`Cannot find a step with label '${apiParams.stepToProve}'`))
                    | Some(stmtToProve) => {
                        switch state->labelsToExprs(apiParams.args0) {
                            | Error(msg) => promiseResolved(Error(msg))
                            | Ok(args0) => {
                                switch state->labelsToExprs(apiParams.args1) {
                                    | Error(msg) => promiseResolved(Error(msg))
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
                                                | None => Ok(Js_json.null)
                                                | Some(proved) => Ok(proved->Js_json.boolean)
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

let unifyAll = (
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
):promise<result<Js_json.t,string>> => {
    if (!canStartUnifyAll) {
        promiseResolved(Error(
            "Cannot start \"Unify All\" because either there are syntax errors in the editor or edit is in progress."
        ))
    } else {
        startUnifyAll()->promiseMap(_ => Ok(Js_json.null))
    }
}

let validateStepType = (typ:option<string>):result<option<string>,string> => {
    switch typ {
        | None => Ok(typ)
        | Some(typ) => {
            if (typ == "h" || typ == "p" || typ == "g") {
                Ok(Some(typ))
            } else {
                Error(`Step type must be one of: h,p,g.`)
            }
        }
    }
}

type addStepInputParams = {
    label: option<string>,
    typ: option<string>,
    stmt: option<string>,
    jstf: option<string>,
}
type addStepsInputParams = {
    atIdx: option<int>,
    steps: array<addStepInputParams>,
    vars: option<array<array<string>>>
}
let addSteps = (
    ~paramsJson:Js_json.t,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
):promise<result<Js_json.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<addStepsInputParams,string> = fromJson(paramsJson, asObj(_, d=>{
        {
            atIdx: d->intOpt("atIdx", ()),
            steps: d->arr("steps", asObj(_, d=>{
                {
                    label: d->strOpt("label", ()),
                    typ: d->strOpt("type", ~validator=validateStepType, ()),
                    stmt: d->strOpt("stmt", ()),
                    jstf: d->strOpt("jstf", ()),
                }
            }, ()), ()),
            vars: d->arrOpt("vars", asArr(_, asStr(_, ()), ()), ()),
        }
    }, ()), ())
    switch parseResult {
        | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
        | Ok(parseResult) => {
            if (
                parseResult.vars->Belt.Option.map(vars => vars->Js_array2.some(a => a->Js_array2.length != 2))
                    ->Belt_Option.getWithDefault(false)
            ) {
                promiseResolved(Error("Each sub-array of the 'vars' array must consist of two elements."))
            } else {
                setState(st => {
                    let steps = parseResult.steps->Js_array2.map(step => {
                        {
                            id: None,
                            label: step.label,
                            typ: step.typ->Belt.Option.map(userStmtTypeExtendedFromStrExn),
                            cont: step.stmt,
                            jstf: step.jstf,
                        }
                    })
                    let vars = parseResult.vars
                        ->Belt.Option.map(vars => vars->Js_array2.map(var => (var[0], Some(var[1]))))
                    switch st->addSteps(~atIdx=?parseResult.atIdx, ~steps, ~vars?, ()) {
                        | Error(msg) => Error(msg)
                        | Ok((st,stmtIds)) => {
                            let stmtIdToLabel = Belt_HashMapString.fromArray(
                                st.stmts->Js_array2.map(stmt => (stmt.id, stmt.label))
                            )
                            Ok(
                                st,
                                stmtIds->Js_array2.map(stmtId => {
                                    stmtIdToLabel->Belt_HashMapString.get(stmtId)->Belt.Option.getExn->Js_json.string
                                })->Js_json.array
                            )
                        }
                    }
                })
            }
        }
    }
}

type substituteInputParams = {
    what: string,
    with_: string,
}
let substitute = (
    ~paramsJson:Js_json.t,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
):promise<result<Js_json.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<substituteInputParams,string> = fromJson(paramsJson, asObj(_, d=>{
        {
            what: d->str("what", ()),
            with_: d->str("with", ()),
        }
    }, ()), ())
    switch parseResult {
        | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
        | Ok(parseResult) => {
            setState(st => {
                st->substitute(~what=parseResult.what, ~with_=parseResult.with_)
                    ->Belt.Result.map(st => (st,Js_json.null))
            })
        }
    }
}

type updateStepInputParams = {
    label: string,
    typ: option<string>,
    stmt: option<string>,
    jstf: option<string>,
}
let updateSteps = (
    ~paramsJson:Js_json.t,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
):promise<result<Js_json.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<array<updateStepInputParams>,string> = fromJson(paramsJson, asArr(_, asObj(_, d=>{
        {
            label: d->str("label", ()),
            typ: d->strOpt("type", ~validator=validateStepType, ()),
            stmt: d->strOpt("stmt", ()),
            jstf: d->strOpt("jstf", ()),
        }
    }, ()), ()), ())
    switch parseResult {
        | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
        | Ok(inputSteps) => {
            setState(st => {
                let labelToStmtId = st.stmts->Js_array2.map(stmt => (stmt.label,stmt.id))->Belt_HashMapString.fromArray
                let stepWithoutId = inputSteps
                    ->Js_array2.find(step => labelToStmtId->Belt_HashMapString.get(step.label)->Belt.Option.isNone)
                switch stepWithoutId {
                    | Some(step) => Error(`Cannot find step with label '${step.label}'`)
                    | None => {
                        let steps = inputSteps->Js_array2.map(step => {
                            {
                                id: labelToStmtId->Belt_HashMapString.get(step.label),
                                label: None,
                                typ: step.typ->Belt.Option.map(userStmtTypeExtendedFromStrExn),
                                cont: step.stmt,
                                jstf: step.jstf,
                            }
                        })
                        switch st->updateSteps(steps) {
                            | Error(msg) => Error(msg)
                            | Ok(st) => Ok( st, true->Js_json.boolean )
                        }
                    }
                }
            })
        }
    }
}

let logApiCallsToConsole = ref(false)

let setLogApiCallsToConsole = (params:Js_json.t):promise<result<Js_json.t,string>> => {
    switch Js_json.decodeBoolean(params) {
        | None => promiseResolved(Error("The parameter of setLogApiCallsToConsole() must me a boolean."))
        | Some(bool) => {
            logApiCallsToConsole := bool
            promiseResolved(Ok(Js_json.null))
        }
    }
}

let setLogApiCallsToConsoleRef:ref<option<api>> = ref(None)
let getStateRef:ref<option<api>> = ref(None)
let proveBottomUpRef:ref<option<api>> = ref(None)
let unifyAllRef:ref<option<api>> = ref(None)
let addStepsRef:ref<option<api>> = ref(None)
let updateStepsRef:ref<option<api>> = ref(None)
let getTokenTypeRef:ref<option<api>> = ref(None)
let substituteRef:ref<option<api>> = ref(None)

let makeApiFuncRef = (ref:ref<option<api>>):api => {
    params => {
        switch ref.contents {
            | None => Js.Exn.raiseError("api function is not defined")
            | Some(func) => func(params)
        }
    }
}

let api = {
    "setLogApiCallsToConsole": makeApiFuncRef(setLogApiCallsToConsoleRef),
    "editor": {
        "getState": makeApiFuncRef(getStateRef),
        "proveBottomUp": makeApiFuncRef(proveBottomUpRef),
        "unifyAll": makeApiFuncRef(unifyAllRef),
        "addSteps": makeApiFuncRef(addStepsRef),
        "updateSteps": makeApiFuncRef(updateStepsRef),
        "getTokenType": makeApiFuncRef(getTokenTypeRef),
        "substitute": makeApiFuncRef(substituteRef),
    },
}

let apiCallCnt = ref(0)

let makeApiFunc = (name:string, func:Js_json.t=>promise<result<Js_json.t,string>>):api => {
    params => {
        apiCallCnt := apiCallCnt.contents + 1
        let apiCallId = apiCallCnt.contents
        if (logApiCallsToConsole.contents) {
            Js.Console.log2(`[${apiCallId->Belt.Int.toString}] <<< ${name}`, params)
        }
        func(params)->promiseMap(res => {
            let resp = switch res {
                | Error(msg) => errResp(msg)
                | Ok(json) => okResp(json)
            }
            if (logApiCallsToConsole.contents) {
                Js.Console.log2(`[${apiCallId->Belt.Int.toString}] >>> `, resp)
            }
            resp
        })
    }
}

let updateEditorApi = (
    ~state:editorState,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
):unit => {
    setLogApiCallsToConsoleRef := Some(makeApiFunc("setLogApiCallsToConsole", setLogApiCallsToConsole))
    getStateRef := Some(makeApiFunc("editor.getState", _ => getEditorState(~state)))
    proveBottomUpRef := Some(makeApiFunc("editor.proveBottomUp", params => {
        proveBottomUp(
            ~paramsJson=params,
            ~state, 
            ~canStartProvingBottomUp,
            ~startProvingBottomUp,
        )
    }))
    unifyAllRef := Some(makeApiFunc("editor.unifyAll", _ => {
        unifyAll(
            ~canStartUnifyAll,
            ~startUnifyAll,
        )
    }))
    addStepsRef := Some(makeApiFunc("editor.addSteps", params => {
        addSteps(
            ~paramsJson=params,
            ~setState,
        )
    }))
    updateStepsRef := Some(makeApiFunc("editor.updateSteps", params => {
        updateSteps(
            ~paramsJson=params,
            ~setState,
        )
    }))
    getTokenTypeRef := Some(makeApiFunc("editor.getTokenType", params => {
        getTokenType(
            ~paramsJson=params,
            ~state,
        )
    }))
    substituteRef := Some(makeApiFunc("editor.substitute", params => {
        substitute(
            ~paramsJson=params,
            ~setState,
        )
    }))
}