open MM_wrk_editor
open Common
open MM_context
open MM_syntax_tree
open MM_wrk_editor_substitution
open MM_substitution
open MM_apply_asrt_matcher_type
open MM_api

let rec syntaxTreeNodeToJson = (node:syntaxTreeNode, ctxConstIntToSymExn:int=>string):JSON.t => {
    Dict.fromArray([
        ("id", node.id->Belt.Int.toFloat->JSON.Encode.float),
        ("nodeType", "expr"->JSON.Encode.string),
        ("exprType", ctxConstIntToSymExn(node.typ)->JSON.Encode.string),
        ("label", node.label->JSON.Encode.string),
        ("children", node.children->Array.map(childNodeToJson(_, ctxConstIntToSymExn))->JSON.Encode.array ),
    ])->JSON.Encode.object 
} and childNodeToJson = (node:childNode, ctxConstIntToSymExn:int=>string):JSON.t => {
    switch node {
        | Subtree(subtree) => syntaxTreeNodeToJson(subtree, ctxConstIntToSymExn)
        | Symbol({id, sym, isVar}) => {
            Dict.fromArray([
                ("id", id->Belt.Int.toFloat->JSON.Encode.float),
                ("nodeType", "sym"->JSON.Encode.string),
                ("sym", sym->JSON.Encode.string),
                ("isVar", isVar->JSON.Encode.bool),
            ])->JSON.Encode.object 
        }
    }
}

let syntaxTreeToJson = (stmt:userStmt, ctxConstIntToSymExn:int=>string):JSON.t => {
    switch stmt.cont {
        | Text(_) => JSON.Encode.null
        | Tree({exprTyp, root}) => {
            Dict.fromArray([
                ("exprType", exprTyp->JSON.Encode.string),
                ("root", syntaxTreeNodeToJson(root, ctxConstIntToSymExn)),
            ])->JSON.Encode.object
        }
    }
}

let getSelectedFragmentId = (stmt:userStmt):JSON.t => {
    switch stmt.cont {
        | Text(_) => JSON.Encode.null
        | Tree(stmtContTreeData) => {
            switch getSelectedSubtree(stmtContTreeData) {
                | None => JSON.Encode.null
                | Some(node) => {
                    switch node {
                        | Subtree({id}) | Symbol({id}) => id->Belt.Int.toFloat->JSON.Encode.float
                    }
                }
            }
        }
    }
}

let stmtToJson = (stmt:userStmt, ctxConstIntToSymExn:option<int=>string>):JSON.t => {
    Dict.fromArray([
        ("id", stmt.id->JSON.Encode.string),
        ("status", 
            switch stmt.proofStatus {
                | None => JSON.Encode.null
                | Some(proofStatus) => {
                    switch proofStatus {
                        | Ready => "v"
                        | Waiting => "~"
                        | NoJstf => "?"
                        | JstfIsIncorrect => "x"
                    }->JSON.Encode.string
                }
            }
        ),
        ("label", stmt.label->JSON.Encode.string),
        ("isHyp", (stmt.typ == E)->JSON.Encode.bool),
        ("isGoal", stmt.isGoal->JSON.Encode.bool),
        ("isBkm", stmt.isBkm->JSON.Encode.bool),
        ("jstfText", stmt.jstfText->JSON.Encode.string),
        (
            "jstf", 
            stmt.jstfText->MM_wrk_editor.parseJstf->Belt.Result.mapWithDefault(
                JSON.Encode.null, 
                (jstf:option<MM_statements_dto.jstf>) => {
                    switch jstf {
                        | None => JSON.Encode.null
                        | Some(jstf) => {
                            Dict.fromArray([
                                ("args", jstf.args->Array.map(JSON.Encode.string(_))->JSON.Encode.array),
                                ("asrt", jstf.label->JSON.Encode.string),
                            ])->JSON.Encode.object
                        }
                    }
                }
            )
        ),
        ("stmt", stmt.cont->MM_wrk_editor.contToStr->JSON.Encode.string),
        ("tree", 
            switch ctxConstIntToSymExn {
                | None => JSON.Encode.null
                | Some(ctxConstIntToSymExn) => syntaxTreeToJson(stmt, ctxConstIntToSymExn)
            }
        ),
        ("fragId", getSelectedFragmentId(stmt)),
        ("stmtErr", 
            switch stmt.stmtErr {
                | None => JSON.Encode.null
                | Some({ code, msg }) => {
                    Dict.fromArray([
                        ("code", code->Belt.Int.toFloat->JSON.Encode.float),
                        ("msg", msg->JSON.Encode.string),
                    ])->JSON.Encode.object
                }
            }
        ),
        ("unifErr", stmt.unifErr->Belt.Option.map(JSON.Encode.string)->Belt.Option.getWithDefault(JSON.Encode.null)),
        ("syntaxErr", stmt.syntaxErr->Belt.Option.map(JSON.Encode.string)->Belt.Option.getWithDefault(JSON.Encode.null)),
    ])->JSON.Encode.object
}

let getAllSteps = (~state:editorState):JSON.t => {
    let ctxConstIntToSymExn = state.wrkCtx->Belt_Option.map(wrkCtx => ctxIntToSymExn(wrkCtx, _))
    state.stmts->Array.map(stmtToJson(_, ctxConstIntToSymExn))->JSON.Encode.array
}

let stateCached:Belt_HashMapInt.t<editorState> = Belt_HashMapInt.make(~hintSize=4)
let stateJsonCached:Belt_HashMapInt.t<JSON.t> = Belt_HashMapInt.make(~hintSize=4)
let getEditorState = (~editorId:int, ~state:editorState):promise<result<JSON.t,string>> => {
    let cachedResult = switch stateCached->Belt_HashMapInt.get(editorId) {
        | None => None
        | Some(stateCached) => {
            if (stateCached === state) {
                stateJsonCached->Belt_HashMapInt.get(editorId)
            } else {
                None
            }
        }
    }
    switch cachedResult {
        | Some(cachedResult) => Promise.resolve(Ok(cachedResult))
        | None => {
            let stmtIdToLabel = Belt_HashMapString.fromArray(
                state.stmts->Array.map(stmt => (stmt.id, stmt.label))
            )
            let stateJson = Dict.fromArray([
                ("editorId", editorId->JSON.Encode.int),
                ("descr", state.descr->JSON.Encode.string),
                ("varsText", state.varsText->JSON.Encode.string),
                ("varsErr", state.varsErr->Belt.Option.map(JSON.Encode.string)->Belt_Option.getWithDefault(JSON.Encode.null)),
                ("vars", 
                    switch MM_wrk_ctx_data.textToVarDefs(state.varsText) {
                        | Error(_) => JSON.Encode.null
                        | Ok(varDefs) => {
                            varDefs->Array.map(varDef => {
                                varDef->Array.map(JSON.Encode.string(_))->JSON.Encode.array
                            })->JSON.Encode.array
                        }
                    }
                ),
                ("disjText", state.disjText->JSON.Encode.string),
                ("disjErr", state.disjErr->Belt.Option.map(JSON.Encode.string)->Belt_Option.getWithDefault(JSON.Encode.null)),
                ("disj",
                    state.disjText->multilineTextToNonEmptyLines->Array.map(disjLine => {
                        disjLine->String.split(" ")
                            ->Array.map(String.trim(_))
                            ->Array.filter(str => str != "")
                            ->Array.map(JSON.Encode.string(_))
                            ->JSON.Encode.array
                    })->JSON.Encode.array
                ),
                ("steps", getAllSteps(~state)),
                ("selectedSteps", 
                    state.checkedStmtIds->Array.map(((stmtId,_)) => stmtIdToLabel->Belt_HashMapString.get(stmtId))
                        ->Array.filter(Belt.Option.isSome(_))
                        ->Array.map(labelOpt => labelOpt->Belt.Option.getExn->JSON.Encode.string)
                        ->JSON.Encode.array
                ),
            ])->JSON.Encode.object
            stateCached->Belt_HashMapInt.set(editorId, state)
            stateJsonCached->Belt_HashMapInt.set(editorId, stateJson)
            Promise.resolve(Ok(stateJson))
        }
    }
}

let getTokenType = (
    ~paramsJson:apiInput,
    ~state:editorState,
):promise<result<JSON.t,string>> => {
    switch state.wrkCtx {
        | None => Promise.resolve(Error("Cannot determine token type because the editor contains errors."))
        | Some(wrkCtx) => {
            open Expln_utils_jsonParse
            let parseResult:result<{"tokens":array<string>},string> = fromJson(paramsJson->apiInputToJson, asObj(_, d=>{
                {
                    "tokens":d->arr("tokens", asStr(_))
                }
            }))
            switch parseResult {
                | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
                | Ok(params) => {
                    Promise.resolve(Ok(
                        params["tokens"]->Array.map(token => {
                            switch wrkCtx->getTokenType(token) {
                                | None => JSON.Encode.null
                                | Some(tokenType) => {
                                    switch tokenType {
                                        | C => "c"
                                        | V => "v"
                                        | F => "f"
                                        | E => "e"
                                        | A => "a"
                                        | P => "p"
                                    }->JSON.Encode.string
                                }
                            }
                        })->JSON.Encode.array
                    ))
                }
            }
        }
    }
}

let labelsToExprs = (st:editorState, labels:array<string>):result<array<MM_context.expr>,string> => {
    labels->Array.reduce(
        Ok([]),
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
                                    arr->Array.push(expr)
                                    res
                                }
                            }
                        }
                    }
                }
            }
        }
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
    if (matcher.res->Belt.Option.isNone && matcher.hyps->Array.length == 0) {
        Error("'res' and 'hyps' must not be empty at the same time.")
    } else {
        let hypMatchers = matcher.hyps->Array.reduce(Ok([]), (res,hypMatcher) => {
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
                                        hypMatchers->Array.push(Label(label))
                                        res
                                    }
                                }
                            }
                            | None => {
                                switch hypMatcher.idx {
                                    | Some(idx) => {
                                        hypMatchers->Array.push(Idx(idx))
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
            }
        )
        switch hypMatchers {
            | Error(msg) => Error(msg)
            | Ok(hypMatchers) => {
                try {
                    let overrideHyps = matcher.hyps->Array.map(hypMatcher => {
                        ctx->ctxStrToIntsExn(hypMatcher.pat)
                    })
                    let frame = createFrame( 
                        ~ctx, ~ord=0, ~isAxiom=true, ~label="###temp_asrt###", ~proof=None, 
                        ~skipFirstSymCheck=true, ~skipDisj=true, 
                        ~overrideHyps,
                        ~exprStr = switch matcher.res {
                            | Some(res) => res
                            | None => (matcher.hyps->Array.getUnsafe(0)).pat
                        }->getSpaceSeparatedValuesAsArray
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
                    | Exn.Error(exn) => Error(exn->Exn.message->Belt_Option.getWithDefault("Unknown error."))
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
                    matches->Array.reduce(
                        Ok([]),
                        (res,matcher) => {
                            switch res {
                                | Error(_) => res
                                | Ok(res) => {
                                    switch apiMatcherToMatcher( ~ctx=wrkCtx, ~matcher, ) {
                                        | Error(msg) => Error(msg)
                                        | Ok(parsedMatcher) => {
                                            res->Array.push(parsedMatcher)
                                            Ok(res)
                                        }
                                    }
                                }
                            }
                        }
                    )->Belt_Result.map(arr => Some(arr))
                }
            }
        }
    }
}

let lastPreCtxV = ref(-100)
let sortedFrames = Belt_HashMapString.make(~hintSize=64)
let sortFrames = (st:editorState, frames:array<string>):array<string> => {
    if (lastPreCtxV.contents != st.preCtxData.ctxV.ver) {
        lastPreCtxV := st.preCtxData.ctxV.ver
        sortedFrames->Belt_HashMapString.clear
    }
    let framesStr = frames->Array.joinUnsafe(" ")
    switch sortedFrames->Belt_HashMapString.get(framesStr) {
        | Some(sorted) => sorted
        | None => {
            let sorted = MM_substitution.sortFrames(st.preCtxData.frms, frames)
            sortedFrames->Belt_HashMapString.set(framesStr, sorted)
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
    bottomUpProverParams: MM_bottom_up_prover_params.bottomUpProverParams,
    selectFirstFoundProof:bool,
}
let proveBottomUp = (
    ~paramsJson:apiInput,
    ~state:editorState,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
):promise<result<JSON.t,string>> => {
    if (!canStartProvingBottomUp) {
        Promise.resolve(Error(
            "Cannot start proving bottom-up because either there are syntax errors in the editor" 
                ++ " or edit is in progress."
        ))
    } else {
        open Expln_utils_jsonParse
        let parseResult:result<proveBottomUpApiParams,string> = fromJson(paramsJson->apiInputToJson, asObj(_, d=>{
            {
                delayBeforeStartMs: d->intOpt("delayBeforeStartMs"),
                stepToProve: d->str("stepToProve"),
                debugLevel: d->intOpt("debugLevel"),
                maxSearchDepth: d->int("maxSearchDepth"),
                selectFirstFoundProof: d->boolOpt("selectFirstFoundProof"),
                frameParams: d->arr("assertionParams", asObj(_, d=>{
                    {
                        minDist: d->intOpt("minDist"),
                        maxDist: d->intOpt("maxDist"),
                        matches: d->arrOpt("matches", asObj(_, d=>{
                            {
                                res: d->strOpt("res"),
                                hyps: d->arr("hyps", asObj(_, d=>{
                                    {
                                        label: d->strOpt("label"),
                                        idx: d->intOpt("idx"),
                                        pat: d->str("pat"),
                                        
                                    }
                                }), ~default = () => []),
                            }
                        })),
                        framesToUse: d->arrOpt("assertionsToUse", asStr(_)),
                        stepsToUse: d->arr("stepsToDeriveFrom", asStr(_)),
                        allowNewDisjointsForExistingVariables: d->bool("allowNewDisjointsForExistingVariables"),
                        allowNewSteps: d->bool("allowNewSteps"),
                        allowNewVariables: d->bool("allowNewVariables"),
                        statementLengthRestriction: d->str("statementLengthRestriction", ~validator = str => {
                            switch MM_bottom_up_prover_params.lengthRestrictFromStr(str) {
                                | Some(_) => Ok(str)
                                | None => Error(`statementLengthRestriction must be one of: No, LessEq, Less.`)
                            }
                        }),
                        maxNumberOfBranches: d->intOpt("maxNumberOfBranches"),
                        
                    }
                })),
            }
        }))
        switch parseResult {
            | Error(msg) => Promise.resolve(Error(msg))
            | Ok(apiParams) => {
                switch state.stmts->Array.find(stmt => stmt.label == apiParams.stepToProve) {
                    | None => Promise.resolve(Error(`Cannot find a step with the label '${apiParams.stepToProve}'`))
                    | Some(stmtToProve) => {
                        let args = apiParams.frameParams->Array.reduce(
                            Ok([]),
                            (res,frameParams) => {
                                switch res {
                                    | Error(msg) => Error(msg)
                                    | Ok(args) => {
                                        switch state->labelsToExprs(frameParams.stepsToUse) {
                                            | Error(msg) => Error(msg)
                                            | Ok(exprs) => {
                                                args->Array.push(exprs)
                                                Ok(args)
                                            }
                                        }
                                    }
                                }
                            }
                        )
                        switch args {
                            | Error(msg) => Promise.resolve(Error(msg))
                            | Ok(args) => {
                                let matches = apiParams.frameParams->Array.reduce(
                                    Ok([]),
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
                                                        matches->Array.push(frms)
                                                        Ok(matches)
                                                    }
                                                }
                                            }
                                        }
                                    }
                                )
                                switch matches {
                                    | Error(msg) => Promise.resolve(Error(msg))
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
                                                frameParams: apiParams.frameParams->Array.mapWithIndex(
                                                    (frameParams,i):MM_bottom_up_prover_params.bottomUpProverFrameParams => {
                                                        {
                                                            minDist: frameParams.minDist,
                                                            maxDist: frameParams.maxDist,
                                                            matches: matches->Array.getUnsafe(i),
                                                            frmsToUse: frameParams.framesToUse
                                                                ->Belt.Option.map(sortFrames(state,_)),
                                                            deriveFrom: args->Array.getUnsafe(i),
                                                            allowNewDisjForExistingVars: frameParams.allowNewDisjointsForExistingVariables,
                                                            allowNewStmts: frameParams.allowNewSteps,
                                                            allowNewVars: frameParams.allowNewVariables,
                                                            lengthRestrict: 
                                                                frameParams.statementLengthRestriction
                                                                    ->MM_bottom_up_prover_params.lengthRestrictFromStrExn,
                                                            maxNumberOfBranches: frameParams.maxNumberOfBranches,
                                                        }
                                                    }
                                                ),
                                                updateParams:None,
                                            }
                                        })->Promise.thenResolve(proved => {
                                            switch proved {
                                                | None => Ok(JSON.Encode.null)
                                                | Some(proved) => Ok(proved->JSON.Encode.bool)
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
):promise<result<JSON.t,string>> => {
    if (!canStartUnifyAll) {
        Promise.resolve(Ok(JSON.Encode.bool(false)))
    } else {
        startUnifyAll()->Promise.thenResolve(_ => Ok(JSON.Encode.bool(true)))
    }
}

let mergeDuplicatedSteps = (
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    setState(st => {
        let (st,renames) = st->autoMergeDuplicatedStatements
        let renamesJson = renames->Array.map(((from,to_)) => {
            [from->JSON.Encode.string, to_->JSON.Encode.string]->JSON.Encode.array
        })->JSON.Encode.array
        Ok( st, renamesJson )
    })
}

let editorSetContIsHidden = (
    ~params:apiInput,
    ~setEditorContIsHidden:bool=>promise<unit>,
):promise<result<JSON.t,string>> => {
    switch params->apiInputToJson->JSON.Decode.bool {
        | None => Promise.resolve(Error("The parameter of setContentIsHidden() must me a boolean."))
        | Some(bool) => setEditorContIsHidden(bool)->Promise.thenResolve(_ => Ok(JSON.Encode.null))
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
            let allVarNames = vars->Array.filter(var => var->Array.length > 1)
                ->Array.map(var => var->Array.getUnsafe(1))
            let uniqueVarNames = allVarNames->Belt_HashSetString.fromArray
            if (allVarNames->Array.length == uniqueVarNames->Belt_HashSetString.size) {
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
                    let definedVars:array<(string,tokenType)> = vars->Array.filter(var => var->Array.length > 1)
                        ->Array.map(var => var->Array.getUnsafe(1))
                        ->Array.map(varName => (varName, wrkCtx->MM_context.getTokenType(varName)))
                        ->Array.filter(((_,tokenTypeOpt)) => tokenTypeOpt->Belt_Option.isSome)
                        ->Array.map(((varName,tokenTypeOpt)) => (varName,tokenTypeOpt->Belt_Option.getExn))
                    if (definedVars->Array.length == 0) {
                        Ok(())
                    } else {
                        let varTypes = definedVars
                            ->Array.map(((varName,tokenType)) => `${varName} is a ${tokenType->tokenTypeToStr}`)
                            ->Array.joinUnsafe("; ")
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
    ~paramsJson:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<addStepsInputParams,string> = fromJson(paramsJson->apiInputToJson, asObj(_, d=>{
        {
            atIdx: d->intOpt("atIdx"),
            steps: d->arr("steps", asObj(_, d=>{
                {
                    label: d->strOpt("label"),
                    typ: d->strOpt("type", ~validator=validateStepType),
                    stmt: d->strOpt("stmt"),
                    jstf: d->strOpt("jstf"),
                    isBkm: d->bool("isBkm", ~default=()=>false),
                }
            })),
            vars: d->arrOpt("vars", asArr(_, asStr(_, ~validator=str=>{
                let trimed = str->String.trim
                if (trimed == "") {
                    Error("Variable type and name must not be empty")
                } else {
                    Ok(trimed)
                }
            }))),
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(parseResult) => {
            if (
                parseResult.vars->Belt.Option.map(vars => vars->Array.some(a => a->Array.length != 2))
                    ->Belt_Option.getWithDefault(false)
            ) {
                Promise.resolve(
                    Error("Each sub-array of the 'vars' array must consist of two elements: [type,varName].")
                )
            } else {
                switch validateVarNamesAreUnique(parseResult.vars) {
                    | Error(msg) => Promise.resolve(Error(msg))
                    | Ok(_) => {
                        switch validateVarNamesNotPresentInCtx(state, parseResult.vars) {
                            | Error(msg) => Promise.resolve(Error(msg))
                            | Ok(_) => {
                                let steps = parseResult.steps->Array.map(step => {
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
                                    ->Belt.Option.map(vars => vars->Array.map(var => (var->Array.getUnsafe(0), Some(var->Array.getUnsafe(1)))))
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
                                        ~atIdx=?parseResult.atIdx, ~steps, ~vars?, ~dontAddVariablesToContext
                                    ) {
                                        | Error(msg) => Error(msg)
                                        | Ok((st,stmtIds)) => {
                                            let stmtIdToLabel = Belt_HashMapString.fromArray(
                                                st.stmts->Array.map(stmt => (stmt.id, stmt.label))
                                            )
                                            Ok(
                                                st,
                                                stmtIds->Array.map(stmtId => {
                                                    stmtIdToLabel->Belt_HashMapString.get(stmtId)
                                                        ->Belt.Option.getExn->JSON.Encode.string
                                                })->JSON.Encode.array
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

type updateStepInputParams = {
    label: string,
    typ: option<string>,
    stmt: option<string>,
    jstf: option<string>,
    isBkm: option<bool>,
}
let updateSteps = (
    ~paramsJson:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<{"steps":array<updateStepInputParams>},string> = 
        fromJson(paramsJson->apiInputToJson, asObj(_, d=>{
            {
                "steps":d->arr("steps", asObj(_, d=>{
                    {
                        label: d->str("label"),
                        typ: d->strOpt("type", ~validator=validateStepType),
                        stmt: d->strOpt("stmt"),
                        jstf: d->strOpt("jstf"),
                        isBkm: d->boolOpt("isBkm"),
                    }
                }))
            }
        }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(inputParams) => {
            let inputSteps = inputParams["steps"]
            setState(st => {
                let labelToStmtId = st.stmts->Array.map(stmt => (stmt.label,stmt.id))->Belt_HashMapString.fromArray
                let stepWithoutId = inputSteps
                    ->Array.find(step => labelToStmtId->Belt_HashMapString.get(step.label)->Belt.Option.isNone)
                switch stepWithoutId {
                    | Some(step) => Error(`Cannot find step with label '${step.label}'`)
                    | None => {
                        let steps = inputSteps->Array.map(step => {
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
                            | Ok(st) => Ok( st, true->JSON.Encode.bool )
                        }
                    }
                }
            })
        }
    }
}

type renameStepsInputParams = {
    renaming: array<array<string>>
}
let apiRenameSteps = (
    ~params:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<renameStepsInputParams,string> = fromJson(params->apiInputToJson, asObj(_, d=>{
        {
            renaming: d->arr("renaming", asArr(_, asStr(_, ~validator=str=>{
                let trimed = str->String.trim
                if (trimed == "") {
                    Error("Labels must not be empty.")
                } else {
                    Ok(trimed)
                }
            }))),
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => {
            if ( params.renaming->Array.some(pair => pair->Array.length != 2) ) {
                Promise.resolve(
                    Error("Each sub-array of the 'renaming' array must consist of two labels: [renameFrom,renameTo].")
                )
            } else {
                setState(st => {
                    switch params.renaming->Array.reduce(Ok(st), @warning("-8")(st,([fromLabel,toLabel])) => {
                        switch st {
                            | Error(msg) => Error(msg)
                            | Ok(st) => {
                                switch st->editorGetStmtByLabel(fromLabel) {
                                    | None => Error(`Cannot find a step with label ${fromLabel}`)
                                    | Some(stmtToRename) => st->renameStmt(stmtToRename.id, toLabel)
                                }
                            }
                        }
                    }) {
                        | Error(msg) => Error(msg)
                        | Ok(st) => Ok((st,JSON.Encode.null))
                    }
                })
            }
        }
    }
}

type substituteInputParams = {
    what: string,
    with_: string,
    method: string,
}
let substitute = (
    ~paramsJson:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<substituteInputParams,string> = fromJson(paramsJson->apiInputToJson, asObj(_, d=>{
        {
            what: d->str("what"),
            with_: d->str("with_"),
            method: d->str("method", ~validator=m=>{
                if (m!="m" && m!="u") {
                    Error("'method' must be 'm' (for matching) or 'u' (for unification).")
                } else {
                    Ok(m)
                }
            }),
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => {
            setState(st => {
                st->substitute(~what=params.what, ~with_=params.with_, ~useMatching=params.method=="m")
                    ->Belt.Result.map(st => (st,JSON.Encode.null))
            })
        }
    }
}

type setDisjointsInputParams = {
    disj: array<array<string>>,
}
let setDisjoints = (
    ~apiInput:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<setDisjointsInputParams,string> = fromJson(apiInput->apiInputToJson, asObj(_, d=>{
        {
            disj: d->arr("disj", asArr(_, asStr(_))),
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => {
            setState(st => {
                Ok((
                    st->completeDisjEditMode(
                        params.disj->Array.map(line => {
                            line->Array.join(" ")
                        })->Array.join("\n")
                    ),
                    JSON.Encode.null
                ))
            })
        }
    }
}

type setDescriptionInputParams = {
    descr: string,
}
let setDescription = (
    ~apiInput:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<setDescriptionInputParams,string> = fromJson(apiInput->apiInputToJson, asObj(_, d=>{
        {
            descr: d->str("descr"),
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => {
            setState(st => {
                Ok((
                    st->completeDescrEditMode( params.descr ),
                    JSON.Encode.null
                ))
            })
        }
    }
}

let resetEditorContent = (
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    setState(st => Ok(( st->resetEditorContent, JSON.Encode.null )))
}

let deleteSteps = (
    ~params:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<{"labels":array<string>},string> = fromJson(params->apiInputToJson, asObj(_, d=>{
        {
            "labels":d->arr("labels", asStr(_))
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(stepLabelsToDelete) => {
            setState(st => {
                let labelToStmtId = st.stmts->Array.map(stmt => (stmt.label,stmt.id))->Belt_HashMapString.fromArray
                let stepIdsToDelete = stepLabelsToDelete["labels"]
                    ->Array.map(label => labelToStmtId->Belt_HashMapString.get(label))
                    ->Array.filter(Belt.Option.isSome(_))
                    ->Array.map(Belt.Option.getExn(_))
                Ok( st->deleteStmts(stepIdsToDelete), JSON.Encode.null )
            })
        }
    }
}

let apiMarkStepsChecked = (
    ~params:apiInput,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
):promise<result<JSON.t,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<{"labels":array<string>},string> = fromJson(params->apiInputToJson, asObj(_, d=>{
        {
            "labels":d->arr("labels", asStr(_))
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => {
            let stepLabelsToCheck = params["labels"]
            setState(st => {
                let st = st->uncheckAllStmts
                let st = st.stmts->Array.reduce(st, (st, stmt) => {
                    if (stepLabelsToCheck->Array.includes(stmt.label)) {
                        st->toggleStmtChecked(stmt.id)
                    } else {
                        st
                    }
                })
                Ok( st, JSON.Encode.null )
            })
        }
    }
}

let apiAddAsrtByLabel = (
    ~params:apiInput,
    ~addAsrtByLabel: string=>promise<result<unit,string>>,
):promise<result<unit,string>> => {
    open Expln_utils_jsonParse
    let parseResult:result<{"asrtLabel":string},string> = fromJson(params->apiInputToJson, asObj(_, d=>{
        {
            "asrtLabel":d->str("asrtLabel")
        }
    }))
    switch parseResult {
        | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
        | Ok(params) => addAsrtByLabel(params["asrtLabel"])
    }
}

let editorBuildSyntaxTrees = (
    ~params:apiInput,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~state:editorState,
):promise<result<JSON.t,string>> => {
    switch state.wrkCtx {
        | None => Promise.resolve(Error( "Cannot build syntax trees because there are errors in the editor." ))
        | Some(wrkCtx) => {
            open Expln_utils_jsonParse
            let parseResult:result<{"exprs":array<string>},string> = fromJson(params->apiInputToJson, asObj(_, d=>{
                {
                    "exprs":d->arr("exprs", asStr(_))
                }
            }))
            switch parseResult {
                | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
                | Ok(params) => {
                    switch buildSyntaxTrees(params["exprs"]) {
                        | Error(msg) => Promise.resolve(Error(msg))
                        | Ok(syntaxTrees) => {
                            Promise.resolve(
                                Ok(
                                    syntaxTrees->Array.map(syntaxTree => {
                                        switch syntaxTree {
                                            | Error(msg) => {
                                                Dict.fromArray([
                                                    ("err", msg->JSON.Encode.string),
                                                    ("tree", JSON.Encode.null),
                                                ])->JSON.Encode.object 
                                            }
                                            | Ok(syntaxTree) => {
                                                Dict.fromArray([
                                                    ("err", JSON.Encode.null),
                                                    ("tree", syntaxTreeNodeToJson(syntaxTree, ctxIntToSymExn(wrkCtx, _))),
                                                ])->JSON.Encode.object 
                                            }
                                        }
                                    })->JSON.Encode.array
                                )
                            )
                        }
                    }
                }
            }
        }
    }
}

let findAsrtsByUnif = (
    ~wrkCtx:mmContext,
    ~asrtSyntaxTrees:Belt_HashMapString.t<syntaxTreeNode>,
    ~asrtLabels:array<string>,
    ~exprSyntaxTrees:array<result<(int,syntaxTreeNode),string>>,
    ~isMetavar:string=>bool,
):array<result<array<string>,string>> => {
    let frames:Belt_MapString.t<frame> = wrkCtx->getAllFrames
    let frames:array<frame> = asrtLabels->Array.map(asrtLabel => frames->Belt_MapString.get(asrtLabel))
        ->Array.filter(Option.isSome)
        ->Array.map(Option.getExn(_, ~message="MM_api_editor.findAsrtsByUnif.1"))
        ->Array.filter(frame => asrtSyntaxTrees->Belt_HashMapString.has(frame.label))
    let ctxDisj = wrkCtx->getAllDisj
    let foundSubs = MM_asrt_syntax_tree.unifSubsMake()
    exprSyntaxTrees->Array.map(exprSyntaxTree => {
        switch exprSyntaxTree {
            | Error(msg) => Error(msg)
            | Ok((typ,exprSyntaxTree)) => {
                Ok(
                    frames->Array.filter(frame => {
                        MM_asrt_syntax_tree.unifSubsReset(foundSubs)
                        frame.asrt->Array.getUnsafe(0) == typ && MM_asrt_syntax_tree.unify(
                            ~asrtDisj=frame.disj,
                            ~ctxDisj,
                            ~asrtExpr=asrtSyntaxTrees->Belt_HashMapString.get(frame.label)
                                ->Option.getExn(~message="MM_api_editor.findAsrtsByUnif.2"),
                            ~ctxExpr=exprSyntaxTree,
                            ~isMetavar,
                            ~foundSubs,
                        )
                    })->Array.map(frame => frame.label)
                )
            }
        }
    })
}

let statementsToExpressions = (
    ~wrkCtx:mmContext,
    ~stmts:array<string>,
): array<result<(int,string),string>> => {
    stmts->Array.map(stmt => {
        let syms = getSpaceSeparatedValuesAsArray(stmt)
        let len = syms->Array.length
        if (len < 2) {
            Error("The statement length must be at least 2 symbols.")
        } else {
            switch wrkCtx->ctxSymToInt(syms->Array.getUnsafe(0)) {
                | None => Error(`Unrecognized symbol '${syms->Array.getUnsafe(0)}'.`)
                | Some(typ) => Ok((typ, syms->Array.sliceToEnd(~start=1)->Array.join(" ")))
            }
        }
    })
}

type apiFindAsrtsByUnifInpParams = {
    stmts:array<string>,
    asrtLabels:option<array<string>>
}

let apiFindAsrtsByUnif = (
    ~params:apiInput,
    ~state:editorState,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~getAsrtSyntaxTrees:()=>promise<Belt_HashMapString.t<syntaxTreeNode>>,
    ~unifMetavarPrefix:string,
):promise<result<JSON.t,string>> => {
    let isMetavar = String.startsWith(_, unifMetavarPrefix)
    switch state.wrkCtx {
        | None => Promise.resolve(Error(
            "Cannot build syntax trees for expressions because there are errors in the editor."
        ))
        | Some(wrkCtx) => {
            open Expln_utils_jsonParse
            let parseResult:result<apiFindAsrtsByUnifInpParams,string> = fromJson(params->apiInputToJson, asObj(_, d=>{
                {
                    stmts:d->arr("stmts", asStr(_)),
                    asrtLabels:d->arrOpt("asrtLabels", asStr(_)),
                }
            }))
            switch parseResult {
                | Error(msg) => Promise.resolve(Error(`Could not parse input parameters: ${msg}`))
                | Ok(params) => {
                    let exprs:array<result<(int,string),string>> = statementsToExpressions(~wrkCtx, ~stmts=params.stmts)
                    let exprToBuildSyntaxTreesFor = exprs->Array.map(expr => {
                        switch expr {
                            | Error(_) => ""
                            | Ok((_,str)) => str
                        }
                    })
                    switch buildSyntaxTrees(exprToBuildSyntaxTreesFor) {
                        | Error(msg) => Promise.resolve(Error(msg))
                        | Ok(exprSyntaxTrees) => {
                            getAsrtSyntaxTrees()->Promise.thenResolve(asrtSyntaxTrees => {
                                let asrtLabels = switch params.asrtLabels {
                                    | Some(asrtLabels) => asrtLabels
                                    | None => asrtSyntaxTrees->Belt_HashMapString.keysToArray
                                }
                                let exprSyntaxTrees:array<result<(int,syntaxTreeNode),string>> = 
                                    exprs->Array.mapWithIndex((expr,idx) => {
                                        switch expr {
                                            | Error(msg) => Error(msg)
                                            | Ok((typ,_)) => {
                                                exprSyntaxTrees->Array.getUnsafe(idx)
                                                    ->Result.map(exprSyntaxTree => (typ,exprSyntaxTree))
                                            }
                                        }
                                    })
                                Ok(
                                    findAsrtsByUnif(
                                        ~wrkCtx, ~asrtSyntaxTrees, ~asrtLabels, ~exprSyntaxTrees, ~isMetavar 
                                    )->Array.map(foundAstrLabelsPerExpr => {
                                        switch foundAstrLabelsPerExpr {
                                            | Error(msg) => {
                                                Dict.fromArray([
                                                    ("err", msg->JSON.Encode.string),
                                                ])->JSON.Encode.object 
                                            }
                                            | Ok(foundAsrtLabels) => {
                                                Dict.fromArray([
                                                    ("err", JSON.Encode.null),
                                                    (
                                                        "foundAsrtLabels", 
                                                        foundAsrtLabels
                                                            ->Array.map(JSON.Encode.string)
                                                            ->JSON.Encode.array
                                                    ),
                                                ])->JSON.Encode.object 
                                            }
                                        }
                                    })->JSON.Encode.array
                                )
                            })
                        }
                    }
                }
            }
        }
    }
}

type editorData = {
    editorId:int,
    unifMetavarPrefix:string,
    state:editorState,
    setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
    setEditorContIsHidden:bool=>promise<unit>,
    canStartProvingBottomUp:bool,
    startProvingBottomUp:proverParams=>promise<option<bool>>,
    canStartUnifyAll:bool,
    startUnifyAll:unit=>promise<unit>,
    buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    getAsrtSyntaxTrees:()=>promise<Belt_HashMapString.t<syntaxTreeNode>>,
    addAsrtByLabel: string=>promise<result<unit,string>>,
}

let makeSingleEditorApi = (editorData:editorData):singleEditorApi => {
    let editorId = editorData.editorId
    let unifMetavarPrefix = editorData.unifMetavarPrefix
    let state = editorData.state
    let setState = editorData.setState
    let setEditorContIsHidden = editorData.setEditorContIsHidden
    let canStartProvingBottomUp = editorData.canStartProvingBottomUp
    let startProvingBottomUp = editorData.startProvingBottomUp
    let canStartUnifyAll = editorData.canStartUnifyAll
    let startUnifyAll = editorData.startUnifyAll
    let buildSyntaxTrees = editorData.buildSyntaxTrees
    let getAsrtSyntaxTrees = editorData.getAsrtSyntaxTrees
    let addAsrtByLabel = editorData.addAsrtByLabel
    {
        "getState": makeApiFunc("editor.getState", _ => getEditorState(~editorId, ~state)),
        "proveBottomUp": makeApiFunc(
            "editor.proveBottomUp",
            params => proveBottomUp( ~paramsJson=params, ~state, ~canStartProvingBottomUp, ~startProvingBottomUp, )
        ),
        "unifyAll": makeApiFunc("editor.unifyAll", _ => unifyAll( ~canStartUnifyAll, ~startUnifyAll, )),
        "addSteps": makeApiFunc("editor.addSteps", params => addSteps( ~state, ~paramsJson=params, ~setState, )),
        "updateSteps": makeApiFunc("editor.updateSteps", params => updateSteps( ~paramsJson=params, ~setState, )),
        "renameSteps": makeApiFunc("editor.renameSteps", params => apiRenameSteps( ~params, ~setState, )),
        "deleteSteps": makeApiFunc("editor.deleteSteps", params => deleteSteps( ~params, ~setState, )),
        "markStepsChecked": makeApiFunc("editor.markStepsChecked", params => apiMarkStepsChecked( ~params, ~setState, )),
        "setDisjoints": makeApiFunc("editor.setDisjoints", apiInput => setDisjoints( ~apiInput, ~setState, )),
        "setDescription": makeApiFunc("editor.setDescription", apiInput => setDescription( ~apiInput, ~setState, )),
        "resetEditorContent": makeApiFunc("editor.resetEditorContent", _ => resetEditorContent(~setState, )),
        "getTokenType": makeApiFunc("editor.getTokenType", params => getTokenType( ~paramsJson=params, ~state, )),
        "substitute": makeApiFunc("editor.substitute", params => substitute( ~paramsJson=params, ~setState, )),
        "mergeDuplicatedSteps": makeApiFunc("editor.mergeDuplicatedSteps", _ => mergeDuplicatedSteps( ~setState, )),
        "setContentIsHidden": makeApiFunc(
            "editor.setContentIsHidden", 
            params => editorSetContIsHidden( ~params, ~setEditorContIsHidden, )
        ),
        "buildSyntaxTrees": makeApiFunc(
            "editor.buildSyntaxTrees", 
            params => editorBuildSyntaxTrees( ~params, ~buildSyntaxTrees, ~state, )
        ),
        "findAsrtsByUnif": makeApiFunc(
            "editor.apiFindAsrtsByUnif", 
            params => apiFindAsrtsByUnif(~params, ~state, ~buildSyntaxTrees, ~getAsrtSyntaxTrees, ~unifMetavarPrefix)
        ),
        "addAsrtByLabel": makeApiFunc("editor.addAsrtByLabel", params => apiAddAsrtByLabel( ~params, ~addAsrtByLabel, )),
    }
}

let editorsData: Belt_HashMapInt.t<editorData> = Belt_HashMapInt.make(~hintSize=1)

let lastOpenedEditorId:ref<option<int>> = ref(None)

let setLastOpenedEditorId = (editorId:int):unit => {
    lastOpenedEditorId := Some(editorId)
}

let editorApi = (editorId:option<int>):singleEditorApi => {
    switch editorId->Option.orElse(lastOpenedEditorId.contents) {
        | None => makeEmptySingleEditorApi("Cannot determine what editor tab to use.")
        | Some(editorId) => {
            switch editorsData->Belt_HashMapInt.get(editorId) {
                | None => makeEmptySingleEditorApi("Cannot find an editor with id=" ++ editorId->Int.toString)
                | Some(editorData) => makeSingleEditorApi(editorData)
            }
        }
    }
}

setEditorApi(editorApi)

let deleteEditor = (editorId:int):unit => {
    if (lastOpenedEditorId.contents == Some(editorId)) {
        lastOpenedEditorId := None
    }
    editorsData->Belt_HashMapInt.remove(editorId)
    stateCached->Belt_HashMapInt.remove(editorId)
    stateJsonCached->Belt_HashMapInt.remove(editorId)
}

let updateEditorData = (
    ~editorId:int,
    ~unifMetavarPrefix:string,
    ~state:editorState,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~getAsrtSyntaxTrees:()=>promise<Belt_HashMapString.t<syntaxTreeNode>>,
    ~addAsrtByLabel: string=>promise<result<unit,string>>,
):unit => {
    editorsData->Belt_HashMapInt.set(editorId, {
        editorId,
        unifMetavarPrefix,
        state,
        setState,
        setEditorContIsHidden,
        canStartProvingBottomUp,
        startProvingBottomUp,
        canStartUnifyAll,
        startUnifyAll,
        buildSyntaxTrees,
        getAsrtSyntaxTrees,
        addAsrtByLabel,
    })
}