open MM_wrk_editor
open Expln_utils_promise
open Common
open MM_context
open MM_syntax_tree
open MM_wrk_editor_substitution
open MM_substitution
open MM_parser
open MM_apply_asrt_matcher

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

let rec syntaxTreeNodeToJson = (node:syntaxTreeNode, ctxConstIntToSymExn:int=>string):Js_json.t => {
    Js_dict.fromArray([
        ("id", node.id->Belt.Int.toFloat->Js_json.number),
        ("nodeType", "expr"->Js_json.string),
        ("exprType", ctxConstIntToSymExn(node.typ)->Js_json.string),
        ("label", node.label->Js_json.string),
        ("children", node.children->Js.Array2.map(childNodeToJson(_, ctxConstIntToSymExn))->Js_json.array ),
    ])->Js_json.object_ 
} and childNodeToJson = (node:childNode, ctxConstIntToSymExn:int=>string):Js_json.t => {
    switch node {
        | Subtree(subtree) => syntaxTreeNodeToJson(subtree, ctxConstIntToSymExn)
        | Symbol({id, sym, isVar}) => {
            Js_dict.fromArray([
                ("id", id->Belt.Int.toFloat->Js_json.number),
                ("nodeType", "sym"->Js_json.string),
                ("sym", sym->Js_json.string),
                ("isVar", isVar->Js_json.boolean),
            ])->Js_json.object_ 
        }
    }
}

let syntaxTreeToJson = (stmt:userStmt, ctxConstIntToSymExn:int=>string):Js_json.t => {
    switch stmt.cont {
        | Text(_) => Js_json.null
        | Tree({exprTyp, root}) => {
            Js_dict.fromArray([
                ("exprType", exprTyp->Js_json.string),
                ("root", syntaxTreeNodeToJson(root, ctxConstIntToSymExn)),
            ])->Js_json.object_
        }
    }
}

let getSelectedFragmentId = (stmt:userStmt):Js_json.t => {
    switch stmt.cont {
        | Text(_) => Js_json.null
        | Tree(stmtContTreeData) => {
            switch getSelectedSubtree(stmtContTreeData) {
                | None => Js_json.null
                | Some(node) => {
                    switch node {
                        | Subtree({id}) | Symbol({id}) => id->Belt.Int.toFloat->Js_json.number
                    }
                }
            }
        }
    }
}

let stmtToJson = (stmt:userStmt, ctxConstIntToSymExn:option<int=>string>):Js_json.t => {
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
        ("isBkm", stmt.isBkm->Js_json.boolean),
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
        ("tree", 
            switch ctxConstIntToSymExn {
                | None => Js_json.null
                | Some(ctxConstIntToSymExn) => syntaxTreeToJson(stmt, ctxConstIntToSymExn)
            }
        ),
        ("fragId", getSelectedFragmentId(stmt)),
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
}

let getAllSteps = (~state:editorState):Js_json.t => {
    let ctxConstIntToSymExn = state.wrkCtx->Belt_Option.map(wrkCtx => wrkCtx->ctxIntToSymExn)
    state.stmts->Js.Array2.map(stmtToJson(_, ctxConstIntToSymExn))->Js.Json.array
}

let stateCached = ref(None)
let stateJsonCached = ref(None)
let getEditorState = (~state:editorState):promise<result<Js_json.t,string>> => {
    let canUseCachedValue = switch stateCached.contents {
        | None => false
        | Some(stateCached) => stateCached === state
    }
    if (canUseCachedValue) {
        promiseResolved(Ok(stateJsonCached.contents->Belt_Option.getExn))
    } else {
        let stmtIdToLabel = Belt_HashMapString.fromArray(
            state.stmts->Js_array2.map(stmt => (stmt.id, stmt.label))
        )
        let stateJson = Js_dict.fromArray([
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
                    disjLine->Js.String2.split(" ")
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
        stateCached := Some(state)
        stateJsonCached := Some(stateJson)
        promiseResolved(Ok(stateJson))
    }
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

type apiApplyAsrtResultHypMatcher = {
    label: option<string>,
    idx: option<int>,
    pat: string,
}

type apiApplyAsrtResultMatcher = {
    res: option<string>,
    hyps: array<apiApplyAsrtResultHypMatcher>,
}

let apiMatcherToMatcher = (
    ~ctx:mmContext,
    ~matcher:apiApplyAsrtResultMatcher,
):result<applyAsrtResultMatcher,string> => {
    if (matcher.res->Belt.Option.isNone && matcher.hyps->Js_array2.length == 0) {
        Error("'res' and 'hyps' must not be empty at the same time.")
    } else {
        let hypMatchers = matcher.hyps->Js_array2.reduce((res,hypMatcher) => {
                switch res {
                    | Error(_) => res
                    | Ok(hypMatchers) => {
                        switch hypMatcher.label {
                            | Some(label) => {
                                switch hypMatcher.idx {
                                    | Some(_) => {
                                        Error(
                                            "Only one of 'label' or 'idx' must be specified" 
                                                ++ " for each hypothesis matcher"
                                        )
                                    }
                                    | None => {
                                        hypMatchers->Js_array2.push(Label(label))->ignore
                                        res
                                    }
                                }
                            }
                            | None => {
                                switch hypMatcher.idx {
                                    | Some(idx) => {
                                        hypMatchers->Js_array2.push(Idx(idx))->ignore
                                        res
                                    }
                                    | None => {
                                        Error(
                                            "Either 'label' or 'idx' must be specified for each hypothesis matcher"
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            }, Ok([])
        )
        switch hypMatchers {
            | Error(msg) => Error(msg)
            | Ok(hypMatchers) => {
                try {
                    let overrideHyps = matcher.hyps->Js_array2.map(hypMatcher => {
                        ctx->ctxStrToIntsExn(hypMatcher.pat)
                    })
                    let frame = createFrame( 
                        ~ctx, ~ord=0, ~isAxiom=true, ~label="###temp_asrt###", ~proof=None, 
                        ~skipFirstSymCheck=true, ~skipDisj=true, 
                        ~overrideHyps,
                        ~exprStr = switch matcher.res {
                            | Some(res) => res
                            | None => matcher.hyps[0].pat
                        }->getSpaceSeparatedValuesAsArray,
                        ()
                    )
                    Ok(
                        {
                            frm: frame->prepareFrmSubsDataForFrame,
                            matchAsrt: matcher.res->Belt.Option.isSome,
                            hypMatchers,
                        }
                    )
                } catch {
                    | MmException({msg}) => Error(msg)
                    | Js.Exn.Error(exn) => Error(exn->Js.Exn.message->Belt_Option.getWithDefault("Unknown error."))
                }
            }
        }
    }
}

let optArrayToMatchers = (
    ~state:editorState,
    ~matches:option<array<apiApplyAsrtResultMatcher>>,
):result<option<array<applyAsrtResultMatcher>>,string> => {
    switch matches {
        | None => Ok(None)
        | Some(matches) => {
            switch state.wrkCtx {
                | None => Error("Error: cannot parse patters to match")
                | Some(wrkCtx) => {
                    matches->Js_array2.reduce(
                        (res,matcher) => {
                            switch res {
                                | Error(_) => res
                                | Ok(res) => {
                                    switch apiMatcherToMatcher( ~ctx=wrkCtx, ~matcher, ) {
                                        | Error(msg) => Error(msg)
                                        | Ok(parsedMatcher) => {
                                            res->Js_array2.push(parsedMatcher)->ignore
                                            Ok(res)
                                        }
                                    }
                                }
                            }
                        },
                        Ok([])
                    )->Belt_Result.map(arr => Some(arr))
                }
            }
        }
    }
}

let lastPreCtxV = ref(-100)
let sortedFrames = ref(Belt_HashMapString.make(~hintSize=0))
let sortFrames = (st:editorState, frames:array<string>):array<string> => {
    if (lastPreCtxV.contents != st.preCtxV) {
        lastPreCtxV := st.preCtxV
        sortedFrames := Belt_HashMapString.make(~hintSize=64)
    }
    let framesStr = frames->Js_array2.joinWith(" ")
    switch sortedFrames.contents->Belt_HashMapString.get(framesStr) {
        | Some(sorted) => sorted
        | None => {
            let sorted = MM_substitution.sortFrames(st.frms, frames)
            sortedFrames.contents->Belt_HashMapString.set(framesStr, sorted)
            sorted
        }
    }
}

type apiBottomUpProverFrameParams = {
    minDist:option<int>,
    maxDist:option<int>,
    matches:option<array<apiApplyAsrtResultMatcher>>,
    framesToUse:option<array<string>>,
    stepsToUse:array<string>,
    allowNewDisjointsForExistingVariables:bool,
    allowNewSteps:bool,
    allowNewVariables:bool,
    statementLengthRestriction:string,
    maxNumberOfBranches:option<int>,
}
type proveBottomUpApiParams = {
    delayBeforeStartMs:option<int>,
    stepToProve:string,
    maxSearchDepth:int,
    debugLevel:option<int>,
    selectFirstFoundProof:option<bool>,
    frameParams: array<apiBottomUpProverFrameParams>,
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
                maxSearchDepth: d->int("maxSearchDepth", ()),
                selectFirstFoundProof: d->boolOpt("selectFirstFoundProof", ()),
                frameParams: d->arr("frameParameters", asObj(_, d=>{
                    {
                        minDist: d->intOpt("minDist", ()),
                        maxDist: d->intOpt("maxDist", ()),
                        matches: d->arrOpt("matches", asObj(_, d=>{
                            {
                                res: d->strOpt("res", ()),
                                hyps: d->arr("hyps", asObj(_, d=>{
                                    {
                                        label: d->strOpt("label", ()),
                                        idx: d->intOpt("idx", ()),
                                        pat: d->str("pat", ()),
                                        
                                    }
                                }, ()), ~default = () => [], ()),
                            }
                        }, ()), ()),
                        framesToUse: d->arrOpt("frames", asStr(_, ()), ()),
                        stepsToUse: d->arr("stepsToDeriveFrom", asStr(_, ()), ()),
                        allowNewDisjointsForExistingVariables: d->bool("allowNewDisjointsForExistingVariables", ()),
                        allowNewSteps: d->bool("allowNewSteps", ()),
                        allowNewVariables: d->bool("allowNewVariables", ()),
                        statementLengthRestriction: d->str("statementLengthRestriction", ~validator = str => {
                            switch MM_provers.lengthRestrictFromStr(str) {
                                | Some(_) => Ok(str)
                                | None => Error(`statementLengthRestriction must be one of: No, LessEq, Less.`)
                            }
                        }, ()),
                        maxNumberOfBranches: d->intOpt("maxNumberOfBranches", ()),
                        
                    }
                }, ()), ()),
            }
        }, ()), ())
        switch parseResult {
            | Error(msg) => promiseResolved(Error(msg))
            | Ok(apiParams) => {
                switch state.stmts->Js.Array2.find(stmt => stmt.label == apiParams.stepToProve) {
                    | None => promiseResolved(Error(`Cannot find a step with the label '${apiParams.stepToProve}'`))
                    | Some(stmtToProve) => {
                        let args = apiParams.frameParams->Js_array2.reduce(
                            (res,frameParams) => {
                                switch res {
                                    | Error(msg) => Error(msg)
                                    | Ok(args) => {
                                        switch state->labelsToExprs(frameParams.stepsToUse) {
                                            | Error(msg) => Error(msg)
                                            | Ok(exprs) => {
                                                args->Js_array2.push(exprs)->ignore
                                                Ok(args)
                                            }
                                        }
                                    }
                                }
                            },
                            Ok([])
                        )
                        switch args {
                            | Error(msg) => promiseResolved(Error(msg))
                            | Ok(args) => {
                                let matches = apiParams.frameParams->Js_array2.reduce(
                                    (res,frameParams) => {
                                        switch res {
                                            | Error(msg) => Error(msg)
                                            | Ok(matches) => {
                                                switch optArrayToMatchers(
                                                    ~state, 
                                                    ~matches=frameParams.matches, 
                                                ) {
                                                    | Error(msg) => Error(msg)
                                                    | Ok(frms) => {
                                                        matches->Js_array2.push(frms)->ignore
                                                        Ok(matches)
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    Ok([])
                                )
                                switch matches {
                                    | Error(msg) => promiseResolved(Error(msg))
                                    | Ok(matches) => {
                                        startProvingBottomUp({
                                            delayBeforeStartMs:
                                                apiParams.delayBeforeStartMs->Belt_Option.getWithDefault(1000),
                                            stmtId: stmtToProve.id,
                                            debugLevel: apiParams.debugLevel->Belt_Option.getWithDefault(0),
                                            selectFirstFoundProof:
                                                apiParams.selectFirstFoundProof->Belt_Option.getWithDefault(false),
                                            bottomUpProverParams: {
                                                maxSearchDepth: apiParams.maxSearchDepth,
                                                frameParams: apiParams.frameParams->Js_array2.mapi(
                                                    (frameParams,i):MM_provers.bottomUpProverFrameParams => {
                                                        {
                                                            minDist: frameParams.minDist,
                                                            maxDist: frameParams.maxDist,
                                                            matches: matches[i],
                                                            frmsToUse: frameParams.framesToUse
                                                                ->Belt.Option.map(sortFrames(state,_)),
                                                            args: args[i],
                                                            allowNewDisjForExistingVars: frameParams.allowNewDisjointsForExistingVariables,
                                                            allowNewStmts: frameParams.allowNewSteps,
                                                            allowNewVars: frameParams.allowNewVariables,
                                                            lengthRestrict: frameParams.statementLengthRestriction->MM_provers.lengthRestrictFromStrExn,
                                                            maxNumberOfBranches: frameParams.maxNumberOfBranches,
                                                        }
                                                    }
                                                )
                                            }
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

let mergeDuplicatedSteps = (
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
):promise<result<Js_json.t,string>> => {
    setState(st => {
        let (st,renames) = st->autoMergeDuplicatedStatements(~selectFirst=true)
        let renamesJson = renames->Js.Array2.map(((from,to_)) => {
            [from->Js_json.string, to_->Js_json.string]->Js_json.array
        })->Js_json.array
        Ok( st, renamesJson )
    })
}

let editorSetContIsHidden = (
    ~params:Js_json.t,
    ~setEditorContIsHidden:bool=>promise<unit>,
):promise<result<Js_json.t,string>> => {
    switch Js_json.decodeBoolean(params) {
        | None => promiseResolved(Error("The parameter of setContentIsHidden() must me a boolean."))
        | Some(bool) => setEditorContIsHidden(bool)->promiseMap(_ => Ok(Js_json.null))
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

let tokenTypeToStr = (tokenType:tokenType):string => {
    switch tokenType {
        | C => "constant"
        | V => "variable"
        | F => "label of a floating"
        | E => "label of an essential hypothesis"
        | A => "label of an axiom"
        | P => "label of a theorem"
    }
}

let validateVarNamesAreUnique = (vars: option<array<array<string>>>):result<unit,string> => {
    switch vars {
        | None => Ok(())
        | Some(vars) => {
            let allVarNames = vars->Js_array2.filter(var => var->Js_array2.length > 1)
                ->Js_array2.map(var => var[1])
            let uniqueVarNames = allVarNames->Belt_HashSetString.fromArray
            if (allVarNames->Js_array2.length == uniqueVarNames->Belt_HashSetString.size) {
                Ok(())
            } else {
                Error( `Cannot create variables because two or more variables have same name.` )
            }
        }
    }
}

let validateVarNamesNotPresentInCtx = (st:editorState, vars: option<array<array<string>>>):result<unit,string> => {
    switch vars {
        | None => Ok(())
        | Some(vars) => {
            switch st.wrkCtx {
                | None => Error("Cannot add new variables because of errors in the editor.")
                | Some(wrkCtx) => {
                    let definedVars:array<(string,tokenType)> = vars->Js_array2.filter(var => var->Js_array2.length > 1)
                        ->Js_array2.map(var => var[1])
                        ->Js_array2.map(varName => (varName, wrkCtx->MM_context.getTokenType(varName)))
                        ->Js_array2.filter(((_,tokenTypeOpt)) => tokenTypeOpt->Belt_Option.isSome)
                        ->Js_array2.map(((varName,tokenTypeOpt)) => (varName,tokenTypeOpt->Belt_Option.getExn))
                    if (definedVars->Js_array2.length == 0) {
                        Ok(())
                    } else {
                        let varTypes = definedVars
                            ->Js.Array2.map(((varName,tokenType)) => `${varName} is a ${tokenType->tokenTypeToStr}`)
                            ->Js.Array2.joinWith("; ")
                        Error(
                            `Cannot create variables because names of some of them are already in use: ${varTypes}.`
                        )
                    }
                }
            }
        }
    }
}

type addStepInputParams = {
    label: option<string>,
    typ: option<string>,
    stmt: option<string>,
    jstf: option<string>,
    isBkm: bool,
}
type addStepsInputParams = {
    atIdx: option<int>,
    steps: array<addStepInputParams>,
    vars: option<array<array<string>>>
}
let addSteps = (
    ~state:editorState,
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
                    isBkm: d->bool("isBkm", ~default=()=>false, ()),
                }
            }, ()), ()),
            vars: d->arrOpt("vars", asArr(_, asStr(_, ~validator=str=>{
                let trimed = str->Js_string2.trim
                if (trimed == "") {
                    Error("Variable type and name must not be empty")
                } else {
                    Ok(trimed)
                }
            }, ()), ()), ()),
        }
    }, ()), ())
    switch parseResult {
        | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
        | Ok(parseResult) => {
            if (
                parseResult.vars->Belt.Option.map(vars => vars->Js_array2.some(a => a->Js_array2.length != 2))
                    ->Belt_Option.getWithDefault(false)
            ) {
                promiseResolved(
                    Error("Each sub-array of the 'vars' array must consist of two elements: [type,varName].")
                )
            } else {
                switch validateVarNamesAreUnique(parseResult.vars) {
                    | Error(msg) => promiseResolved(Error(msg))
                    | Ok(_) => {
                        switch validateVarNamesNotPresentInCtx(state, parseResult.vars) {
                            | Error(msg) => promiseResolved(Error(msg))
                            | Ok(_) => {
                                let steps = parseResult.steps->Js_array2.map(step => {
                                    {
                                        id: None,
                                        label: step.label,
                                        typ: step.typ->Belt.Option.map(userStmtTypeExtendedFromStrExn),
                                        cont: step.stmt,
                                        jstf: step.jstf,
                                        isBkm: Some(step.isBkm),
                                    }
                                })
                                let vars = parseResult.vars
                                    ->Belt.Option.map(vars => vars->Js_array2.map(var => (var[0], Some(var[1]))))
                                setState(st => {
                                    let dontAddVariablesToContext = 
                                        switch validateVarNamesNotPresentInCtx(st, parseResult.vars) {
                                            | Error(_) => {
                                                /*
                                                editorState is not immutable because it keeps reference to a mutable 
                                                mmContext. But React may call this setter multiple times especially in 
                                                React.StrictMode, because states must be immutable. This results in a 
                                                runtime error when adding the same variable second time. But since it 
                                                was checked that variables are not duplicated before this setter, 
                                                it is safe to skip adding of variables (it means variables have been 
                                                already added).
                                                */
                                                true
                                            }
                                            | Ok(_) => false
                                        }
                                    switch st->addSteps(
                                        ~atIdx=?parseResult.atIdx, ~steps, ~vars?, ~dontAddVariablesToContext, ()
                                    ) {
                                        | Error(msg) => Error(msg)
                                        | Ok((st,stmtIds)) => {
                                            let stmtIdToLabel = Belt_HashMapString.fromArray(
                                                st.stmts->Js_array2.map(stmt => (stmt.id, stmt.label))
                                            )
                                            Ok(
                                                st,
                                                stmtIds->Js_array2.map(stmtId => {
                                                    stmtIdToLabel->Belt_HashMapString.get(stmtId)
                                                        ->Belt.Option.getExn->Js_json.string
                                                })->Js_json.array
                                            )
                                        }
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
            with_: d->str("with_", ()),
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
    isBkm: option<bool>,
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
            isBkm: d->boolOpt("isBkm", ()),
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
                                isBkm: step.isBkm,
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

let deleteSteps = (
    ~params:Js_json.t,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
):promise<result<Js_json.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<array<string>,string> = fromJson(params, asArr(_, asStr(_, ()), ()), ())
    switch parseResult {
        | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
        | Ok(stepLabelsToDelete) => {
            setState(st => {
                let labelToStmtId = st.stmts->Js_array2.map(stmt => (stmt.label,stmt.id))->Belt_HashMapString.fromArray
                let stepIdsToDelete = stepLabelsToDelete
                    ->Js_array2.map(label => labelToStmtId->Belt_HashMapString.get(label))
                    ->Js_array2.filter(Belt.Option.isSome)
                    ->Js_array2.map(Belt.Option.getExn)
                Ok( st->deleteStmts(stepIdsToDelete), Js_json.null )
            })
        }
    }
}

let editorBuildSyntaxTrees = (
    ~params:Js_json.t,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~state:editorState,
):promise<result<Js_json.t,string>> => {
    switch state.wrkCtx {
        | None => promiseResolved(Error( "Cannot build syntax trees because there are errors in the editor." ))
        | Some(wrkCtx) => {
            open Expln_utils_jsonParse
            let parseResult:result<array<string>,string> = fromJson(params, asArr(_, asStr(_, ()), ()), ())
            switch parseResult {
                | Error(msg) => promiseResolved(Error(`Could not parse input parameters: ${msg}`))
                | Ok(exprs) => {
                    switch buildSyntaxTrees(exprs) {
                        | Error(msg) => promiseResolved(Error(msg))
                        | Ok(syntaxTrees) => {
                            promiseResolved(
                                Ok(
                                    syntaxTrees->Js_array2.map(syntaxTree => {
                                        switch syntaxTree {
                                            | Error(msg) => {
                                                Js_dict.fromArray([
                                                    ("err", msg->Js_json.string),
                                                    ("tree", Js_json.null),
                                                ])->Js_json.object_ 
                                            }
                                            | Ok(syntaxTree) => {
                                                Js_dict.fromArray([
                                                    ("err", Js_json.null),
                                                    ("tree", syntaxTreeNodeToJson(syntaxTree, wrkCtx->ctxIntToSymExn)),
                                                ])->Js_json.object_ 
                                            }
                                        }
                                    })->Js_json.array
                                )
                            )
                        }
                    }
                }
            }
        }
    }
}

let logApiCallsToConsole = ref(false)

let setLogApiCallsToConsole = (params:Js_json.t):promise<result<Js_json.t,string>> => {
    switch Js_json.decodeBoolean(params) {
        | None => promiseResolved(Error("The parameter of setLogApiCallsToConsole() must be a boolean."))
        | Some(bool) => {
            logApiCallsToConsole := bool
            promiseResolved(Ok(Js_json.null))
        }
    }
}

let apiShowInfoMsg = (params:Js_json.t, showInfoMsg:string=>unit):promise<result<Js_json.t,string>> => {
    switch Js_json.decodeString(params) {
        | None => promiseResolved(Error("The parameter of showInfoMsg() must be a string."))
        | Some(msg) => {
            showInfoMsg(msg)
            promiseResolved(Ok(Js_json.null))
        }
    }
}

let apiShowErrMsg = (params:Js_json.t, showErrMsg:string=>unit):promise<result<Js_json.t,string>> => {
    switch Js_json.decodeString(params) {
        | None => promiseResolved(Error("The parameter of showErrMsg() must be a string."))
        | Some(msg) => {
            showErrMsg(msg)
            promiseResolved(Ok(Js_json.null))
        }
    }
}

let setLogApiCallsToConsoleRef:ref<option<api>> = ref(None)
let apiShowInfoMsgRef:ref<option<api>> = ref(None)
let apiShowErrMsgRef:ref<option<api>> = ref(None)
let getStateRef:ref<option<api>> = ref(None)
let proveBottomUpRef:ref<option<api>> = ref(None)
let unifyAllRef:ref<option<api>> = ref(None)
let addStepsRef:ref<option<api>> = ref(None)
let updateStepsRef:ref<option<api>> = ref(None)
let deleteStepsRef:ref<option<api>> = ref(None)
let getTokenTypeRef:ref<option<api>> = ref(None)
let substituteRef:ref<option<api>> = ref(None)
let mergeDuplicatedStepsRef:ref<option<api>> = ref(None)
let editorSetContIsHiddenRef:ref<option<api>> = ref(None)
let editorBuildSyntaxTreesRef:ref<option<api>> = ref(None)

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
    "showInfoMsg": makeApiFuncRef(apiShowInfoMsgRef),
    "showErrMsg": makeApiFuncRef(apiShowErrMsgRef),
    "editor": {
        "getState": makeApiFuncRef(getStateRef),
        "proveBottomUp": makeApiFuncRef(proveBottomUpRef),
        "unifyAll": makeApiFuncRef(unifyAllRef),
        "addSteps": makeApiFuncRef(addStepsRef),
        "updateSteps": makeApiFuncRef(updateStepsRef),
        "deleteSteps": makeApiFuncRef(deleteStepsRef),
        "getTokenType": makeApiFuncRef(getTokenTypeRef),
        "substitute": makeApiFuncRef(substituteRef),
        "mergeDuplicatedSteps": makeApiFuncRef(mergeDuplicatedStepsRef),
        "setContentIsHidden": makeApiFuncRef(editorSetContIsHiddenRef),
        "buildSyntaxTrees": makeApiFuncRef(editorBuildSyntaxTreesRef),
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
    ~showInfoMsg:string=>unit,
    ~showErrMsg:string=>unit,
    ~setState:(editorState=>result<(editorState,Js_json.t),string>)=>promise<result<Js_json.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
):unit => {
    setLogApiCallsToConsoleRef := Some(makeApiFunc("setLogApiCallsToConsole", setLogApiCallsToConsole))
    apiShowInfoMsgRef := Some(makeApiFunc("showInfoMsg", params => apiShowInfoMsg(params, showInfoMsg)))
    apiShowErrMsgRef := Some(makeApiFunc("showErrMsg", params => apiShowErrMsg(params, showErrMsg)))
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
        unifyAll( ~canStartUnifyAll, ~startUnifyAll, )
    }))
    addStepsRef := Some(makeApiFunc("editor.addSteps", params => {
        addSteps( ~state, ~paramsJson=params, ~setState, )
    }))
    updateStepsRef := Some(makeApiFunc("editor.updateSteps", params => {
        updateSteps( ~paramsJson=params, ~setState, )
    }))
    deleteStepsRef := Some(makeApiFunc("editor.deleteSteps", params => {
        deleteSteps( ~params, ~setState, )
    }))
    getTokenTypeRef := Some(makeApiFunc("editor.getTokenType", params => {
        getTokenType( ~paramsJson=params, ~state, )
    }))
    substituteRef := Some(makeApiFunc("editor.substitute", params => {
        substitute( ~paramsJson=params, ~setState, )
    }))
    mergeDuplicatedStepsRef := Some(makeApiFunc("editor.mergeDuplicatedSteps", _ => {
        mergeDuplicatedSteps( ~setState, )
    }))
    editorSetContIsHiddenRef := Some(makeApiFunc("editor.setContentIsHidden", params => {
        editorSetContIsHidden( ~params, ~setEditorContIsHidden, )
    }))
    editorBuildSyntaxTreesRef := Some(makeApiFunc("editor.buildSyntaxTrees", params => {
        editorBuildSyntaxTrees( ~params, ~buildSyntaxTrees, ~state, )
    }))
}