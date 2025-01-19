open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_wrk_editor
open MM_context
open MM_substitution
open Expln_React_Modal
open MM_statements_dto
open MM_proof_tree
open MM_proof_tree_dto
open MM_wrk_unify
open MM_parenCounter
open MM_wrk_settings
open MM_cmp_root_stmts
open MM_apply_asrt_matcher_type
open Common
open MM_bottom_up_prover_params

type sortBy = NewStmtsNum | UnprovedStmtsNum | NumOfNewVars | AsrtLabel

type resultRendered = {
    idx:int,
    elem: reElem,
    isProved: bool,
    asrtLabel:string,
    numOfNewVars: int,
    numOfUnprovedStmts: int,
    numOfStmts: int,
}

type applyAsrtResultHypMatcherToShow = {
    label: option<string>,
    idx:option<int>,
    pat:string,
}

type applyAsrtResultMatcherToShow = {
    res: option<string>,
    hyps: array<applyAsrtResultHypMatcherToShow>,
}

type proverFrameParamsToShow = {
    @as("minDist") minDist: option<int>,
    @as("maxDist") maxDist: option<int>,
    @as("assertionsToUse") frmsToUse: option<array<string>>,
    @as("matches") matches: option<array<applyAsrtResultMatcherToShow>>,
    @as("stepsToDeriveFrom") deriveFrom: array<string>,
    @as("allowNewDisjointsForExistingVariables") allowNewDisjForExistingVars: bool,
    @as("allowNewStatements") allowNewStmts: bool,
    @as("allowNewVariables") allowNewVars: bool,
    @as("statementLengthRestriction") lengthRestrict: string,
    @as("maxNumberOfBranches") maxNumberOfBranches: option<int>,
}

type proverParamsToShow = {
    @as("stepToProve") stepToProve:string,
    @as("debugLevel") debugLevel:int,
    @as("customParams") customParams:option<customParams>,
    @as("maxSearchDepth") maxSearchDepth:int,
    @as("assertionParams") frameParams: array<proverFrameParamsToShow>,
    @as("updateParams") updateParams: option<string>,
}

type state = {
    rootUserStmts: array<userStmt>,
    rootStmts: array<rootStmt>,
    rootStmtsRendered: array<rootStmtRendered>,
    exprToProve:expr,
    title: reElem,

    initialParams: bottomUpProverParams,
    args0: array<bool>,
    args1: array<bool>,
    args1EqArgs0:bool,
    availableLabels: array<string>,
    label:option<string>,
    depth: int,
    depthStr: string,
    lengthRestrict: lengthRestrict,
    allowNewDisjForExistingVars: bool,
    allowNewStmts: bool,
    allowNewVars: bool,
    useDisc:bool,
    useDepr:bool,
    useTranDepr:bool,
    debugLevel:int,
    maxNumberOfBranchesStr:string,

    proverParamsToShow:option<proverParamsToShow>,
    tree: option<proofTreeDto>,
    warnings:array<string>,
    results: option<result<array<stmtsDto>,string>>,
    resultsRendered: option<array<resultRendered>>,
    sortBy: sortBy,
    resultsSorted: option<array<resultRendered>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultIdx: option<int>,
    resultForApiCallHasBeenSelected: bool,

    showApiParams:bool,
}

let getProofStatus = (stmt:rootStmtRendered):option<proofStatus> => {
    if (stmt.isHyp) {
        Some(Ready)
    } else {
        stmt.proofStatus
    }
}

let exprMayMatchAsrt = (
    ~expr:expr,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
):bool => {
    if (expr->Array.getUnsafe(0) != frm.frame.asrt->Array.getUnsafe(0)) {
        false
    } else {
        let res = ref(false)
        iterateSubstitutions(
            ~frmExpr = frm.frame.asrt,
            ~expr,
            ~frmConstParts = frm.frmConstParts->Array.getUnsafe(frm.numOfHypsE), 
            ~constParts = frm.constParts->Array.getUnsafe(frm.numOfHypsE), 
            ~varGroups = frm.varGroups->Array.getUnsafe(frm.numOfHypsE),
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                res.contents = true
                Stop
            }
        )->ignore
        res.contents
    }
}

let getAvailableAsrtLabels = (
    ~frms: frms, 
    ~parenCnt: parenCnt, 
    ~exprToProve:expr,
) => {
    let availableAsrtLabels = []
    frms->frmsForEach(frm => {
        if ( exprMayMatchAsrt(~expr=exprToProve, ~frm, ~parenCnt) ) {
            availableAsrtLabels->Array.push(frm.frame.label)
        }
    })
    availableAsrtLabels
}

let makeInitialState = (
    ~rootUserStmts: array<userStmt>,
    ~frms: frms,
    ~parenCnt: parenCnt,
    ~initialParams: option<bottomUpProverParams>,
    ~initialDebugLevel: option<int>,
    ~allowedFrms:allowedFrms,
    ~bottomUpProverDefaults: bottomUpProverDefaults,
) => {
    let rootStmts = rootUserStmts->Array.map(userStmtToRootStmt)
    let rootStmtsLen = rootStmts->Array.length
    let maxRootStmtIdx = rootStmtsLen-1
    let exprToProve = (rootStmts->Array.getUnsafe(maxRootStmtIdx)).expr
    let possibleArgs = rootStmts->Array.filterWithIndex((_,i) => i < maxRootStmtIdx)->Array.map(stmt => stmt.expr)

    let params = switch initialParams {
        | Some(params) => params
        | None => bottomUpProverParamsMakeDefault(
            ~maxSearchDepth=bottomUpProverDefaults.searchDepth,
            ~lengthRestrict=bottomUpProverDefaults.lengthRestrict->lengthRestrictFromStr->Option.getOr(Less),
            ~allowNewDisjForExistingVars=bottomUpProverDefaults.allowNewDisjForExistingVars,
            ~allowNewStmts=bottomUpProverDefaults.allowNewStmts,
            ~allowNewVars=bottomUpProverDefaults.allowNewVars,
        )
    }

    let frameParams = params.frameParams
    let frameParamsLen = frameParams->Array.length

    {
        rootUserStmts,
        rootStmts,
        rootStmtsRendered: rootUserStmts->Array.filterWithIndex((_,i) => i < maxRootStmtIdx)->Array.map(stmt => {
            {
                id: stmt.id,
                expr: switch stmt.expr {
                    | None => raise(MmException({msg:`expr must be set on a root statement.`}))
                    | Some(expr) => expr
                },
                isHyp:stmt.typ == E,
                label:stmt.label,
                proofStatus:stmt.proofStatus,
                exprReElem: <span> {MM_cmp_user_stmt.rndContText(~stmtCont=stmt.cont)} </span>
            }
        }),
        exprToProve,
        title:
            <span>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {"Proving bottom-up "->React.string}
                </span>
                { MM_cmp_user_stmt.rndContText(~stmtCont=(rootUserStmts->Array.getUnsafe(maxRootStmtIdx)).cont) }
            </span>,

        initialParams:params,
        args0: possibleArgs->Array.map(possibleArg => {
            frameParamsLen > 0 && (frameParams->Array.getUnsafe(0)).deriveFrom->Array.some(arg => arg->exprEq(possibleArg))
        }),
        args1: possibleArgs->Array.map(possibleArg => {
            frameParamsLen > 1 && (frameParams->Array.getUnsafe(1)).deriveFrom->Array.some(arg => arg->exprEq(possibleArg))
        }),
        args1EqArgs0:false,
        availableLabels: getAvailableAsrtLabels( ~frms, ~parenCnt, ~exprToProve, ),
        label:
            if (frameParamsLen > 0) {
                (frameParams->Array.getUnsafe(0)).frmsToUse
                    ->Belt_Option.flatMap(arr => {
                        if (arr->Array.length > 0) {
                            Some(arr->Array.getUnsafe(0))
                        } else {
                            None
                        }
                    })
            } else {
                None
            },
        depthStr: params.maxSearchDepth->Belt_Int.toString,
        depth: params.maxSearchDepth,
        lengthRestrict:
            if (frameParamsLen > 1) {
                (frameParams->Array.getUnsafe(1)).lengthRestrict
            } else {
                Less
            },
        allowNewDisjForExistingVars:
            if (frameParamsLen > 0) {
                (frameParams->Array.getUnsafe(0)).allowNewDisjForExistingVars
            } else {
                true
            },
        allowNewStmts:
            if (frameParamsLen > 0) {
                (frameParams->Array.getUnsafe(0)).allowNewStmts
            } else {
                true
            },
        allowNewVars:
            if (frameParamsLen > 0) {
                (frameParams->Array.getUnsafe(0)).allowNewVars
            } else {
                false
            },
        useDisc:allowedFrms.inEssen.useDisc,
        useDepr:allowedFrms.inEssen.useDepr,
        useTranDepr:allowedFrms.inEssen.useTranDepr,
        debugLevel: initialDebugLevel
            ->Option.map(lvl => {
                if (0 <= lvl && lvl <= 2) {lvl} else {bottomUpProverDefaults.debugLevel}
            })->Option.getOr(bottomUpProverDefaults.debugLevel),
        maxNumberOfBranchesStr: 
            if (frameParamsLen > 0) {
                (frameParams->Array.getUnsafe(0)).maxNumberOfBranches->Belt_Option.map(Belt_Int.toString)->Belt.Option.getWithDefault("")
            } else {
                ""
            },

        proverParamsToShow: None,
        tree: None,
        warnings: [],
        results: None,
        resultsRendered: None,
        sortBy: NewStmtsNum,
        resultsSorted: None,
        resultsPerPage: 20,
        resultsMaxPage: 0,
        resultsPage: 0,
        checkedResultIdx: None,
        resultForApiCallHasBeenSelected: false,

        showApiParams:false,
    }
}

let setLabel = (st,label) => {
    {
        ...st,
        label
    }
}

let setDepthStr = (st,depthStr) => {
    {
        ...st,
        depthStr
    }
}

let setLengthRestrict = (st,lengthRestrict) => {
    {
        ...st,
        lengthRestrict
    }
}

let toggleAllowNewDisjForExistingVars = (st) => {
    {
        ...st,
        allowNewDisjForExistingVars: !st.allowNewDisjForExistingVars
    }
}

let toggleAllowNewStmts = (st) => {
    {
        ...st,
        allowNewStmts: !st.allowNewStmts
    }
}

let toggleAllowNewVars = (st) => {
    {
        ...st,
        allowNewVars: !st.allowNewVars
    }
}

let toggleArgs1EqArgs0 = (st) => {
    {
        ...st,
        args1EqArgs0: !st.args1EqArgs0
    }
}

let toggleUseDisc = (st) => { ...st, useDisc: !st.useDisc }
let toggleUseDepr = (st) => { ...st, useDepr: !st.useDepr }
let toggleUseTranDepr = (st) => { ...st, useTranDepr: !st.useTranDepr }

let setDebugLevel = (st,debugLevel) => {
    {
        ...st,
        debugLevel: if (0 <= debugLevel && debugLevel <= 2) {debugLevel} else {0}
    }
}

let toggleShowApiParams = (st) => {
    {
        ...st,
        showApiParams: !st.showApiParams
    }
}

let setResultForApiCallHasBeenSelected = (st) => {
    {
        ...st,
        resultForApiCallHasBeenSelected: true
    }
}

let nonDigitPattern = %re("/\D/g")

let setMaxNumberOfBranchesStr = (st, maxNumberOfBranchesStr) => {
    {
        ...st,
        maxNumberOfBranchesStr: maxNumberOfBranchesStr->String.replaceRegExp(nonDigitPattern, "")
    }
}

let selectAllArgs = args => args->Array.map(_ => true)
let unselectAllArgs = args => args->Array.map(_ => false)

let updateArgs0 = (st, args0) => { ...st, args0 }
let updateArgs1 = (st, args1) => { ...st, args1 }

let isStmtToShow = (
    ~stmt:stmtDto, 
    ~rootJstfs:Belt_HashMap.t<expr,option<jstf>,ExprHash.identity>
):bool => {
    switch rootJstfs->Belt_HashMap.get(stmt.expr) {
        | None => true
        | Some(None) => stmt.jstf->Belt_Option.isSome
        | Some(Some(rootJstf)) => {
            switch stmt.jstf {
                | None => false
                | Some(foundJstf) => !(rootJstf->jstfEq(foundJstf))
            }
        }
    }
}

let stmtsDtoToResultRendered = (
    ~stmtsDto:stmtsDto, 
    ~idx:int,
    ~isStmtToShow:stmtDto=>bool,
    ~getFrmLabelBkgColor: string=>option<string>,
):resultRendered => {
    let maxI = stmtsDto.stmts->Array.length - 1
    let stmtsToShow = stmtsDto.stmts->Array.filterWithIndex((stmt,i) => i == maxI || isStmtToShow(stmt))
    let elem = 
        <Col>
            {
                stmtsDto.newDisjStr->Array.map(disjStr => {
                    <span key=disjStr>
                        {disjStr->React.string}
                    </span>
                })->React.array
            }
            <table>
                <tbody>
                    {
                        stmtsToShow->Array.map(stmt => {
                            <tr key=stmt.exprStr>
                                <td>
                                    { React.string(stmt.label) }
                                </td>
                                <td style=ReactDOM.Style.make(~textAlign="right", ())>
                                    <nobr>
                                    {
                                        switch stmt.jstf {
                                            | None => React.null
                                            | Some({args, label}) => {
                                                <>
                                                    <span>
                                                        {React.string("[" ++ args->Array.joinUnsafe(" ") ++ " : ")}
                                                    </span>
                                                    <span
                                                        style=ReactDOM.Style.make(
                                                            ~backgroundColor=?getFrmLabelBkgColor(label), 
                                                            ~borderRadius="3px",
                                                            ()
                                                        )
                                                    >
                                                        {React.string(label)}
                                                    </span>
                                                    <span>
                                                        {React.string(" ]")}
                                                    </span>
                                                </>
                                            }
                                        }
                                    }
                                    </nobr>
                                </td>
                                <td style=ReactDOM.Style.make(~color= if (stmt.isProved) {"green"} else {"#565656"},
                                            ~fontWeight="bold", ())>
                                    {React.string( if (stmt.isProved) {"\u2713"} else {"?"} )}
                                </td>
                                <td> 
                                    {React.string(stmt.exprStr)}
                                </td>
                            </tr>
                        })
                        ->React.array
                    }
                </tbody>
            </table>
        </Col>
    let lastStmt = stmtsDto.stmts->Array.getUnsafe(stmtsDto.stmts->Array.length-1)
    {
        idx,
        elem,
        asrtLabel: lastStmt.jstf->Belt_Option.map(jstf => jstf.label)->Belt_Option.getWithDefault(""),
        numOfNewVars: stmtsDto.newVars->Array.length,
        numOfUnprovedStmts: stmtsDto.stmts->Array.reduce(
            0,
            (cnt,stmt) => cnt + if (stmt.isProved) {0} else {1}
        ),
        isProved: lastStmt.isProved,
        numOfStmts: stmtsToShow->Array.length,
    }
}

let compareByIsProved = Expln_utils_common.comparatorBy((res:resultRendered) => if (res.isProved) {0} else {1})
let compareByNumberOfStmts = Expln_utils_common.comparatorBy(res => res.numOfStmts)

let compareByNumOfUnprovedStmts = Expln_utils_common.comparatorBy(res => res.numOfUnprovedStmts)
let compareByNumOfNewStmts = Expln_utils_common.comparatorBy(res => res.numOfStmts)
let compareByNumOfNewVars = Expln_utils_common.comparatorBy(res => res.numOfNewVars)
let compareByAsrtLabel = (a:resultRendered,b:resultRendered) => {
    a.asrtLabel->String.localeCompare(b.asrtLabel)
}

let createComparator = (sortBy):Expln_utils_common.comparator<resultRendered> => {
    open Expln_utils_common
    let mainCmp = switch sortBy {
        | NewStmtsNum => compareByNumOfNewStmts
        | UnprovedStmtsNum => compareByNumOfUnprovedStmts
        | NumOfNewVars => compareByNumOfNewVars
        | AsrtLabel => compareByAsrtLabel
    }
    compareByIsProved
        ->comparatorAndThen(mainCmp)
        ->comparatorAndThen(compareByNumberOfStmts)
        ->comparatorAndThen(compareByAsrtLabel)
}

let sortResultsRendered = (resultsRendered, sortBy) => {
    resultsRendered->Belt_Option.map(resultsRendered =>
        resultsRendered->Array.copy->Expln_utils_common.sortInPlaceWith(createComparator(sortBy))
    )
}

let isTruncatedSearchErr = (src:exprSrcDto):bool => {
    switch src {
        | AssertionWithErr({err:TooManyCombinations(_)}) => true
        | _ => false
    }
}

let findWarnings = (tree:proofTreeDto):array<string> => {
    if (tree.nodes->Array.some(n => n.parents->Array.some(isTruncatedSearchErr))) {
        [
            "The search space was truncated due to some assertions produce too big search space. " ++ 
                "Use Logging level = 1 and then click 'Show Proof Tree' button to find those assertions."
        ]
    } else {
        []
    }
}

let setResults = (
    st,
    ~tree: option<proofTreeDto>,
    ~warnings:array<string>,
    ~results: option<result<array<stmtsDto>,string>>,
    ~getFrmLabelBkgColor: string=>option<string>,
) => {
    switch results {
        | None | Some(Error(_)) => {
            {
                ...st,
                tree: None,
                warnings,
                results,
                resultsRendered: None,
                resultsSorted: None,
                resultsMaxPage: 0,
                resultsPage: 0,
                checkedResultIdx: None,
            }
        }
        | Some(Ok(results)) => {
            let rootJstfs = st.rootStmts
                ->Array.map(stmt => (stmt.expr, stmt.jstf))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let isStmtToShow = stmt => isStmtToShow(~stmt, ~rootJstfs)
            let resultsRendered = Some(
                results->Array.mapWithIndex((dto,i) => {
                    stmtsDtoToResultRendered(~stmtsDto=dto, ~idx=i, ~isStmtToShow, ~getFrmLabelBkgColor)
                })
            )
            {
                ...st,
                tree,
                warnings,
                results:Some(Ok(results)),
                resultsRendered,
                resultsSorted: sortResultsRendered(resultsRendered, st.sortBy),
                resultsMaxPage: Math.Int.ceil(
                    results->Array.length->Belt_Int.toFloat /. st.resultsPerPage->Belt_Int.toFloat
                ),
                resultsPage: 1,
                checkedResultIdx: None,
            }
        }
    }
    
}

let setSortBy = (st,sortBy) => {
    {
        ...st,
        sortBy,
        resultsSorted: sortResultsRendered(st.resultsRendered, sortBy),
        resultsPage: 1,
        checkedResultIdx: None,
    }
}

let setPage = (st,page) => {
    {
        ...st,
        resultsPage: page,
    }
}

let toggleResultChecked = (st,idx) => {
    switch st.checkedResultIdx {
        | None => {
            {
                ...st,
                checkedResultIdx: Some(idx)
            }
        }
        | Some(checkedIdx) => {
            if (checkedIdx != idx) {
                {
                    ...st,
                    checkedResultIdx: Some(idx)
                }
            } else {
                {
                    ...st,
                    checkedResultIdx: None
                }
            }
        }
    }
}

let sortByToStr = sortBy => {
    switch sortBy {
        | NewStmtsNum => "NewStmtsNum"
        | UnprovedStmtsNum => "UnprovedStmtsNum"
        | NumOfNewVars => "NumOfNewVars"
        | AsrtLabel => "AsrtLabel"
    }
}

let sortByFromStr = str => {
    switch str {
        | "NewStmtsNum" => NewStmtsNum
        | "UnprovedStmtsNum" => UnprovedStmtsNum
        | "NumOfNewVars" => NumOfNewVars
        | "AsrtLabel" => AsrtLabel
        | _ => raise(MmException({msg:`Cannot convert value of '${str}' to a sortBy.`}))
    }
}

let rndCheckboxWithLabelAndBorder = (
    ~checked:bool,
    ~onChange:bool=>unit,
    ~label:string,
) => {
    <FormControlLabel
        control={
            <Checkbox
                checked
                onChange=evt2bool(onChange)
            />
        }
        label
        style=ReactDOM.Style.make(
            ~border="solid 1px lightgrey", 
            ~borderRadius="7px", 
            ~paddingRight="10px",
            ()
        )
    />
}

let startedForApiCalls:array<string> = []

@react.component
let make = (
    ~modalRef:modalRef,
    ~settingsVer:int,
    ~settings:settings,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~frms: frms,
    ~parenCnt: parenCnt,
    ~varsText: string,
    ~disjText: string,
    ~wrkCtx: mmContext,
    ~rootStmts: array<userStmt>,
    ~reservedLabels: array<string>,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~initialParams: option<bottomUpProverParams>=?,
    ~initialDebugLevel: option<int>=?,
    ~apiCallStartTime:option<Date.t>,
    ~delayBeforeStartMs:int,
    ~selectFirstFoundProof:option<bool>,
    ~onResultSelected:option<result<stmtsDto,string>>=>unit,
    ~onCancel:unit=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState( 
        ~rootUserStmts=rootStmts, ~frms, ~parenCnt, ~initialParams, ~initialDebugLevel, 
        ~allowedFrms=settings.allowedFrms, ~bottomUpProverDefaults=settings.bottomUpProverDefaults,
    ))

    let isApiCall = apiCallStartTime->Belt.Option.isSome

    let onlyOneResultIsAvailable = switch state.results {
        | None | Some(Error(_)) => false
        | Some(Ok(results)) => results->Array.length == 1
    }

    let actLabelUpdated = label => {
        setState(setLabel(_,label))
    }

    let actDepthUpdated = depthStr => {
        setState(setDepthStr(_,depthStr->String.replaceRegExp(nonDigitPattern, "")))
    }

    let actLengthRestrictUpdated = lengthRestrict => {
        setState(setLengthRestrict(_,lengthRestrict))
    }

    let actToggleAllowNewDisjForExistingVars = () => {
        setState(toggleAllowNewDisjForExistingVars)
    }

    let actToggleAllowNewStmts = () => {
        setState(toggleAllowNewStmts)
    }

    let actToggleAllowNewVars = () => {
        setState(toggleAllowNewVars)
    }

    let actToggleUseDisc = () => setState(toggleUseDisc)
    let actToggleUseDepr = () => setState(toggleUseDepr)
    let actToggleUseTranDepr = () => setState(toggleUseTranDepr)

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch frms->frmsGetByLabel(label) {
            | None => None
            | Some(frm) => {
                MM_react_common.getFrmLabelBkgColor(frm.frame, settings)
            }
        }
    }

    let actOnResultsReady = (treeDto:result<proofTreeDto,string>) => {
        let rootExprToLabel = state.rootStmts
            ->Array.map(stmt => (stmt.expr,stmt.label))
            ->Belt_HashMap.fromArray(~id=module(ExprHash))
        switch treeDto {
            | Error(msg) => {
                setState(st => {
                    st->setResults(
                        ~tree = None, 
                        ~warnings=[],
                        ~results=Some(Error(msg)), 
                        ~getFrmLabelBkgColor
                    )
                })
            }
            | Ok(treeDto) => {
                let results = proofTreeDtoToNewStmtsDto(
                    ~treeDto, 
                    ~rootExprToLabel,
                    ~ctx = wrkCtx,
                    ~typeToPrefix,
                    ~exprToProve=state.exprToProve,
                    ~reservedLabels,
                )
                setState(st => {
                    st->setResults(
                        ~tree = if (st.debugLevel > 0) {Some(treeDto)} else {None}, 
                        ~warnings=findWarnings(treeDto),
                        ~results=Some(Ok(results)), 
                        ~getFrmLabelBkgColor
                    )
                })
            }
        }
    }

    let getEffectiveProverParams = (state:state):bottomUpProverParams => {
        if (isApiCall) {
            state.initialParams
        } else {
            let deriveFromOnLevel0=state.rootStmtsRendered
                    ->Array.filterWithIndex((_,i) => state.args0->Array.getUnsafe(i))
                    ->Array.map(stmt => stmt.expr)
            bottomUpProverParamsMakeDefault(
                ~asrtLabel=?state.label,
                ~maxSearchDepth=state.depth,
                ~lengthRestrict=state.lengthRestrict,
                ~allowNewDisjForExistingVars=state.allowNewDisjForExistingVars,
                ~allowNewStmts=state.allowNewStmts,
                ~allowNewVars=state.allowNewVars,
                ~deriveFromOnLevel0,
                ~deriveFromOnLevel1=
                    if (state.args1EqArgs0) {
                        deriveFromOnLevel0
                    } else {
                        state.rootStmtsRendered
                            ->Array.filterWithIndex((_,i) => state.args1->Array.getUnsafe(i))
                            ->Array.map(stmt => stmt.expr)
                    },
                ~maxNumberOfBranches=
                    ?if (state.debugLevel == 0 || state.maxNumberOfBranchesStr == "") {
                        None
                    } else {
                        state.maxNumberOfBranchesStr->Belt_Int.fromString
                    }
            )
        }
    }

    let exprToLabel = (state:state, expr:expr):string => {
        switch state.rootStmtsRendered->Array.find(stmt => exprEq(expr, stmt.expr)) {
            | None => {
                raise(MmException({
                    msg: "Could not find a label for an expression in MM_cmp_unify_bottom_up.exprToLabel()"
                }))
            }
            | Some(stmt) => stmt.label
        }
    }

    let makeMatchesToShow = (wrkCtx:mmContext, matches:array<applyAsrtResultMatcher>):array<applyAsrtResultMatcherToShow> => {
        matches->Array.map(matcher => {
            {
                res: 
                    if (matcher.matchAsrt) {
                        Some(wrkCtx->frmIntsToStrExn(matcher.frm.frame, matcher.frm.frame.asrt))
                    } else {
                        None
                    },
                hyps: matcher.hypMatchers->Array.mapWithIndex((hypMatcher,i) => {
                    let (label,idx) = switch hypMatcher {
                        | Label(label) => (Some(label), None)
                        | Idx(idx) => (None, Some(idx))
                    }
                    {
                        label,
                        idx,
                        pat: wrkCtx->frmIntsToStrExn(matcher.frm.frame, (matcher.frm.hypsE->Array.getUnsafe(i)).expr),
                    }
                }),
            }
        })
    }

    let getEffectiveProverParamsToShow = (state:state, params:bottomUpProverParams):proverParamsToShow => {
        {
            stepToProve: (state.rootStmts->Array.getUnsafe(state.rootStmts->Array.length-1)).label,
            debugLevel: state.debugLevel,
            customParams:params.customParams,
            maxSearchDepth: params.maxSearchDepth,
            frameParams: params.frameParams->Array.map(p => {
                minDist: p.minDist,
                maxDist: p.maxDist,
                matches: p.matches->Belt.Option.map(makeMatchesToShow(wrkCtx, _)),
                frmsToUse: p.frmsToUse,
                deriveFrom: p.deriveFrom->Array.map(exprToLabel(state, _)),
                allowNewDisjForExistingVars: p.allowNewDisjForExistingVars,
                allowNewStmts: p.allowNewStmts,
                allowNewVars: p.allowNewVars,
                lengthRestrict: p.lengthRestrict->lengthRestrictToStr,
                maxNumberOfBranches: p.maxNumberOfBranches,
            }),
            updateParams: params.updateParamsStr,
        }
    }

    let actProve = () => {
        setState(st => {
            let depthStr = st.depthStr->String.trim
            let st = if (depthStr->String.length == 0) {
                {...st, depth:1, depthStr:"1"}
            } else {
                let depth = Math.Int.max(depthStr->Belt_Int.fromString->Belt_Option.getWithDefault(1), 1)
                let depthStr = depth->Belt_Int.toString
                {...st, depth, depthStr}
            }

            let effectiveProverParams = getEffectiveProverParams(st)
            let paramsToShow = getEffectiveProverParamsToShow(st, effectiveProverParams)

            let debugLevel = st.debugLevel
            let st = {
                ...st,
                proverParamsToShow: Some(paramsToShow),
            }

            openModal(modalRef, () => rndProgress(~text="Proving bottom-up", ~pct=0.))->promiseMap(modalId => {
                updateModal( 
                    modalRef, modalId, () => rndProgress(
                        ~text="Proving bottom-up", ~pct=0., ~onTerminate=makeActTerminate(modalId)
                    )
                )
                unify(
                    ~settingsVer, ~settings, ~preCtxVer, ~preCtx, ~varsText, ~disjText, ~rootStmts=state.rootStmts,
                    ~bottomUpProverParams=Some(effectiveProverParams),
                    ~allowedFrms={
                        inSyntax: settings.allowedFrms.inSyntax,
                        inEssen: {
                            useDisc:state.useDisc,
                            useDepr:state.useDepr,
                            useTranDepr:state.useTranDepr,
                        }
                    },
                    ~syntaxTypes=None,
                    ~exprsToSyntaxCheck=None,
                    ~debugLevel,
                    ~onProgress = msg => updateModal( 
                        modalRef, modalId, () => rndProgress(
                            ~text=msg, ~onTerminate=makeActTerminate(modalId)
                        )
                    )
                )->promiseMap(proofTreeDto => {
                    closeModal(modalRef, modalId)
                    actOnResultsReady(proofTreeDto)
                })
            })->ignore

            st
        })
    }

    React.useEffect0(() => {
        switch apiCallStartTime {
            | None => ()
            | Some(apiCallStartTime) => {
                let apiCallStartTimeStr = apiCallStartTime->Date.toISOString
                if (!(startedForApiCalls->Array.includes(apiCallStartTimeStr))) {
                    startedForApiCalls->Array.push(apiCallStartTimeStr)
                    startedForApiCalls->Js_array2.removeCountInPlace(
                        ~pos=0, 
                        ~count=startedForApiCalls->Array.length - 5
                    )->ignore
                    setTimeout(() => actProve(), delayBeforeStartMs)->ignore
                }
            }
        }
        None
    })

    let actSortByChange = sortBy => {
        setState(setSortBy(_, sortBy))
    }

    let actPageChange = page => {
        setState(setPage(_, page))
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = (idxToSelect:option<int>) => {
        /*  
            Ignoring state.results == Some(Error(_)) because actChooseSelected should be called 
            only when there are no errors. 
        */
        switch state.results {
            | None | Some(Error(_)) => ()
            | Some(Ok(results)) => {
                if (onlyOneResultIsAvailable) {
                    setState(setResultForApiCallHasBeenSelected)
                    onResultSelected(Some(Ok(results->Array.getUnsafe(0))))
                } else {
                    switch idxToSelect {
                        | Some(checkedResultIdx) => {
                            setState(setResultForApiCallHasBeenSelected)
                            onResultSelected(Some(Ok(results->Array.getUnsafe(checkedResultIdx))))
                        }
                        | None => {
                            switch state.checkedResultIdx {
                                | None => ()
                                | Some(checkedResultIdx) => {
                                    setState(setResultForApiCallHasBeenSelected)
                                    onResultSelected(Some(Ok(results->Array.getUnsafe(checkedResultIdx))))
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    React.useEffect2(() => {
        if (isApiCall && !state.resultForApiCallHasBeenSelected) {
            switch state.resultsSorted {
                | None => {
                    switch state.results {
                        | None => ()
                        | Some(Ok(_)) => {
                            Exn.raiseError(
                                "Internal error: state.resultsSorted is None, but state.results is Some(Ok(_))"
                            )
                        }
                        | Some(Error(msg)) => {
                            setState(setResultForApiCallHasBeenSelected)
                            onResultSelected(Some(Error(msg)))
                        }
                    }
                }
                | Some(resultsSorted) => {
                    setState(setResultForApiCallHasBeenSelected)
                    switch selectFirstFoundProof {
                        | Some(selectFirstFoundProof) => {
                            if (selectFirstFoundProof) {
                                if (resultsSorted->Array.length > 0 && (resultsSorted->Array.getUnsafe(0)).isProved) {
                                    actChooseSelected(Some((resultsSorted->Array.getUnsafe(0)).idx))
                                } else {
                                    onResultSelected(None)
                                }
                            } else {
                                onResultSelected(None)
                            }
                        }
                        | None => {
                            if (resultsSorted->Array.length > 0 && (resultsSorted->Array.getUnsafe(0)).isProved) {
                                actChooseSelected(Some((resultsSorted->Array.getUnsafe(0)).idx))
                            } else {
                                onResultSelected(None)
                            }
                        }
                    }
                }
            }
        }
        None
    }, (state.results, state.resultsSorted))

    let actShowProofTree = () => {
        switch state.tree {
            | None => ()
            | Some(tree) => {
                openModalFullScreen(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        let closeBtn =
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                                {React.string("Close")}
                            </Button>
                        <Col spacing=1. style=ReactDOM.Style.make(~margin="5px", ())>
                            {closeBtn}
                            <MM_cmp_proof_tree
                                tree
                                rootExpr=state.exprToProve
                                settings
                                wrkCtx
                                rootStmts=state.rootStmts
                            />
                            {closeBtn}
                        </Col>
                    })
                })->ignore
            }
        }
    }

    let actChangeDebugLevel = debugLevel => {
        setState(setDebugLevel(_,debugLevel))
    }

    let actMaxNumberOfBranchesStrUpdated = maxNumberOfBranchesStr => {
        setState(setMaxNumberOfBranchesStr(_, maxNumberOfBranchesStr))
    }

    let rndLengthRestrictSelector = (value:lengthRestrict) => {
        <FormControl size=#small>
            <InputLabel id="length-restrict-select-label">"Statement length restriction"</InputLabel>
            <Select
                sx={"width": 190}
                labelId="length-restrict-select-label"
                value={lengthRestrictToStr(value)}
                label="Statement length restriction"
                onChange=evt2str(str => actLengthRestrictUpdated(lengthRestrictFromStrExn(str)))
            >
                <MenuItem value="No">{React.string("Unrestricted")}</MenuItem>
                <MenuItem value="LessEq">{React.string("LessEq")}</MenuItem>
                <MenuItem value="Less">{React.string("Less")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndSortBySelector = () => {
        switch state.results {
            | None | Some(Error(_)) => React.null
            | Some(Ok(results)) => {
                if (results->Array.length < 2) {
                    React.null
                } else {
                    <FormControl size=#small>
                        <InputLabel id="sortBy-select-label">"Sort results by"</InputLabel>
                        <Select 
                            labelId="sortBy-select-label"
                            value=sortByToStr(state.sortBy)
                            label="Sort results by"
                            onChange=evt2str(str => actSortByChange(sortByFromStr(str)))
                        >
                            <MenuItem value="NewStmtsNum">{React.string("Number of new steps")}</MenuItem>
                            <MenuItem value="UnprovedStmtsNum">{React.string("Number of unproved steps")}</MenuItem>
                            <MenuItem value="NumOfNewVars">{React.string("Number of new variables")}</MenuItem>
                            <MenuItem value="AsrtLabel">{React.string("Assertion label")}</MenuItem>
                        </Select>
                    </FormControl>
                }
            }
        }
    }

    let rndDebugParam = () => {
        <Row alignItems=#center>
            {React.string("Logging level")}
            <RadioGroup
                row=true
                value={state.debugLevel->Belt_Int.toString}
                onChange=evt2str(str => actChangeDebugLevel(str->Belt_Int.fromString->Belt_Option.getExn))
            >
                <FormControlLabel value="0" control={ <Radio/> } label="0" />
                <FormControlLabel value="1" control={ <Radio/> } label="1" />
                <FormControlLabel value="2" control={ <Radio/> } label="2" />
            </RadioGroup>
        </Row>
    }

    let rndParamsForUsualCall = () => {
        if (state.availableLabels->Array.length == 0) {
            <Col>
                {
                    React.string("The statement to prove doesn't match any existing assertion. " 
                        ++ "Bottom-up proving is not available for such statements.")
                }
                <Button onClick={_=>onCancel()}> {React.string("Ok")} </Button>
            </Col>
        } else {
            <Col>
                <Row>
                    <AutocompleteVirtualized value=state.label options=state.availableLabels size=#small width=200
                        onChange=actLabelUpdated
                        label="Label of root justification"
                    />
                    <TextField 
                        label="Search depth"
                        size=#small
                        style=ReactDOM.Style.make(~width="100px", ())
                        autoFocus=true
                        value=state.depthStr
                        onChange=evt2str(actDepthUpdated)
                        onKeyDown=kbrdHnd2(
                            kbrdClbkMake(~key=keyEnter, ~act=actProve),
                            kbrdClbkMake(~key=keyEsc, ~act=onCancel),
                        )
                    />
                    {rndLengthRestrictSelector(state.lengthRestrict)}
                </Row>
                <Row>
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewDisjForExistingVars
                                onChange={_ => actToggleAllowNewDisjForExistingVars()}
                            />
                        }
                        label="Allow new disjoints"
                        style=ReactDOM.Style.make(
                            ~border="solid 1px lightgrey", 
                            ~borderRadius="7px", 
                            ~paddingRight="10px",
                            ~marginTop="-2px",
                            ~marginLeft="2px",
                            ()
                        )
                    />
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewStmts
                                onChange={_ => actToggleAllowNewStmts()}
                            />
                        }
                        label="Allow new steps"
                        style=ReactDOM.Style.make(
                            ~border="solid 1px lightgrey", 
                            ~borderRadius="7px", 
                            ~paddingRight="10px",
                            ~marginTop="-2px",
                            ~marginLeft="2px",
                            ()
                        )
                    />
                    <FormControlLabel
                        control={
                            <Checkbox
                                checked=state.allowNewVars
                                onChange={_ => actToggleAllowNewVars()}
                            />
                        }
                        label="Allow new variables"
                        disabled={!state.allowNewStmts}
                        style=ReactDOM.Style.make(
                            ~border="solid 1px lightgrey", 
                            ~borderRadius="7px", 
                            ~paddingRight="10px",
                            ~marginTop="-2px",
                            ()
                        )
                    />
                </Row>
                <table>
                    <tbody>
                        <tr>
                            <td>
                                {React.string("Allow usage of assertions:")}
                            </td>
                            <td style=ReactDOM.Style.make(~paddingLeft="10px", ())>
                                {
                                    rndCheckboxWithLabelAndBorder(
                                        ~checked=state.useDisc,
                                        ~onChange=_=>actToggleUseDisc(),
                                        ~label="discouraged",
                                    )
                                }
                            </td>
                            <td>
                                {
                                    rndCheckboxWithLabelAndBorder(
                                        ~checked=state.useDepr,
                                        ~onChange=_=>actToggleUseDepr(),
                                        ~label="deprecated",
                                    )
                                }
                            </td>
                            <td>
                                {
                                    rndCheckboxWithLabelAndBorder(
                                        ~checked=state.useTranDepr,
                                        ~onChange=_=>actToggleUseTranDepr(),
                                        ~label="transitively deprecated",
                                    )
                                }
                            </td>
                        </tr>
                    </tbody>
                </table>
                <Row>
                    {rndDebugParam()}
                    <TextField 
                        label="Max num of branches"
                        size=#small
                        disabled={state.debugLevel == 0}
                        style=ReactDOM.Style.make(~width="200px", ())
                        value=state.maxNumberOfBranchesStr
                        onChange=evt2str(actMaxNumberOfBranchesStrUpdated)
                        onKeyDown=kbrdHnd2(
                            kbrdClbkMake(~key=keyEnter, ~act=actProve),
                            kbrdClbkMake(~key=keyEsc, ~act=onCancel),
                        )
                    />
                </Row>
                <Row>
                    <Button onClick={_=>actProve()} variant=#contained color="grey" >
                        {React.string("Prove")}
                    </Button>
                    <Button onClick={_=>onCancel()}> {React.string("Cancel")} </Button>
                </Row>
            </Col>
        }
    }

    let actToggleShowApiParams = () => {
        setState(toggleShowApiParams)
    }

    let rndActualProverParams = () => {
        switch state.proverParamsToShow {
            | None => React.null
            | Some(proverParamsToShow) => {
                <TextField
                    label="Actual parameters of the bottom-up prover"
                    size=#small
                    style=ReactDOM.Style.make(~width="800px", ())
                    autoFocus=false
                    multiline=true
                    rows=10
                    value=Expln_utils_common.stringify(proverParamsToShow)
                    disabled=true
                />
            }
        }
    }

    let rndParamsForApiCall = () => {
        <Col>
            {
                switch state.proverParamsToShow {
                    | None => "Starting..."->React.string
                    | Some(_) => {
                        if (state.showApiParams) {
                            rndActualProverParams()
                        } else {
                            React.null
                        }
                    }
                }
            }
            <Row>
                <Button onClick={_=>actToggleShowApiParams()}> 
                    { React.string(if (state.showApiParams) { "Hide parameters" } else { "Show parameters" }) } 
                </Button>
                <Button onClick={_=>onCancel()}> {React.string("Close")} </Button>
            </Row>
        </Col>
    }

    let rndParams = () => {
        if (isApiCall) {
            rndParamsForApiCall()
        } else {
            rndParamsForUsualCall()
        }
    }

    let rndPagination = totalNumOfResults => {
        if (state.resultsPerPage < totalNumOfResults) {
            <Pagination count=state.resultsMaxPage page=state.resultsPage onChange={(_,newPage) => actPageChange(newPage)} />
        } else {
            React.null
        }
    }

    let rndApplyButton = () => {
        <Button onClick={_=>actChooseSelected(None)} variant=#contained
                disabled={!onlyOneResultIsAvailable && state.checkedResultIdx->Belt.Option.isNone}>
            {React.string(if onlyOneResultIsAvailable {"Apply"} else {"Apply selected"})}
        </Button>
    }

    let rndShowProofTreeBtn = () => {
        switch state.tree {
            | None => React.null
            | Some(_) => {
                <Button onClick={_=>actShowProofTree()} variant=#outlined>
                    {React.string("Show proof tree")}
                </Button>
            }
        }
    }

    let actOpenWarningsDialog = ( warnings:array<string>, ) => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col>
                        <ol>
                            {
                                warnings->Array.mapWithIndex((msg,i) => {
                                    <li key={i->Belt_Int.toString}>{React.string(msg)}</li>
                                })->React.array
                            }
                        </ol>
                        <Button onClick={_=>modalRef->closeModal(modalId)} variant=#contained >
                            {React.string("Close")}
                        </Button>
                    </Col>
                </Paper>
            })
        })->ignore
    }

    let rndWarningsBtn = (warnings:array<string>) => {
        if (warnings->Array.length == 0) {
            React.null
        } else {
            <IconButton 
                title="There are warnings, click to see details" 
                onClick={_=>actOpenWarningsDialog(warnings)} 
                color="yellow" 
            >
                <MM_Icons.Warning/>
            </IconButton>
        }
    }

    let actShowActualParams = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col>
                        {rndActualProverParams()}
                        <Button onClick={_=>closeModal(modalRef, modalId)} variant=#outlined>
                            {React.string("Close")}
                        </Button>
                    </Col>
                </Paper>
            })
        })->ignore
    }

    let rndShowActualParamsBtn = () => {
        <IconButton 
            title="Show actual parameters of the bottom-up prover" 
            onClick={_=>actShowActualParams()}
        >
            <MM_Icons.DisplaySettings/>
        </IconButton>
    }

    let rndResults = () => {
        switch state.resultsSorted {
            | None => {
                switch state.results {
                    | None => React.null
                    | Some(Ok(_)) => {
                        Exn.raiseError(
                            "Internal error: state.resultsSorted is None, but state.results is Some(Ok(_))"
                        )
                    }
                    | Some(Error(msg)) => {
                        <Col>
                            <pre style=ReactDOM.Style.make(~color="red", ())> {React.string("Error:")} </pre>
                            <pre> {React.string(msg)} </pre>
                        </Col>
                    }
                }
            }
            | Some(resultsSorted) => {
                let totalNumOfResults = resultsSorted->Array.length
                if (totalNumOfResults == 0) {
                    <Col>
                        <Divider/>
                        <Row>
                            {React.string("Nothing found.")}
                            {rndShowProofTreeBtn()}
                            {rndWarningsBtn(state.warnings)}
                            {rndShowActualParamsBtn()}
                        </Row>
                    </Col>
                } else {
                    let start = (state.resultsPage - 1) * state.resultsPerPage
                    let items = resultsSorted->Array.slice( ~start, ~end = start + state.resultsPerPage )
                    <Col>
                        <Divider/>
                        <Row alignItems=#center>
                            {rndApplyButton()}
                            {rndSortBySelector()}
                            {rndPagination(totalNumOfResults)}
                            {rndShowProofTreeBtn()}
                            {rndWarningsBtn(state.warnings)}
                            {rndShowActualParamsBtn()}
                        </Row>
                        {
                            items->Array.map(item => {
                                <table key={item.idx->Belt_Int.toString}>
                                    <tbody>
                                        <tr>
                                            {
                                                if (onlyOneResultIsAvailable) {
                                                    React.null
                                                } else {
                                                    <td>
                                                        <Checkbox
                                                            checked={
                                                                state.checkedResultIdx
                                                                    ->Belt_Option.map(idx => idx == item.idx)
                                                                    ->Belt.Option.getWithDefault(false)
                                                            }
                                                            onChange={_ => actToggleResultChecked(item.idx)}
                                                        />
                                                    </td>
                                                }
                                            }
                                            <td>
                                                <Paper>
                                                    item.elem
                                                </Paper>
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            })->React.array
                        }
                        {rndPagination(totalNumOfResults)}
                        {rndApplyButton()}
                    </Col>
                }
            }
        }
    }

    let rndTitle = () => {
        state.title
    }

    let actOpenRootStmtsDialog = (
        ~title:string,
        ~stmtToProve:reElem,
        ~getFlags: state => array<bool>,
        ~setFlags: array<bool> => unit,
    ) => {

        let proofStatusesAreAvailable = state.rootStmtsRendered->Array.every(stmt => 
            getProofStatus(stmt)->Belt_Option.isSome
        )

        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_root_stmts 
                    title
                    stmtToProve
                    rootStmtsRendered=state.rootStmtsRendered
                    proofStatusesAreAvailable
                    flags=getFlags(state)
                    onClose={newFlags => {
                        setFlags(newFlags)
                        closeModal(modalRef, modalId)
                    }}
                />
            })
        })->ignore
    }

    let rndRootStmtsForLevelShort = (
        ~title: string, 
        ~titleOnly:bool,
        ~dialogTitle: string,
        ~stmtToProve:reElem,
        ~getFlags: state => array<bool>,
        ~setFlags: array<bool> => unit,
    ) => {
        if (titleOnly) {
            <Row style=ReactDOM.Style.make(~border="solid lightgrey 1px", ~borderRadius="6px", ~margin="2px", ())>
                {React.string(title)}
            </Row>
        } else {
            let flags = state->getFlags
            let allSelected = flags->Array.every(b => b)
            let noneSelected = flags->Array.every(b => !b)
            let numberOfSelected = if (allSelected) {
                "All"
            } else if (noneSelected) {
                "None"
            } else {
                let numSelected = flags->Array.reduce(0, (cnt,b) => if (b) {cnt+1} else {cnt})
                let numAll = flags->Array.length
                numSelected->Belt_Int.toString ++ "/" ++ numAll->Belt_Int.toString
            }
            let (selectUnselectText, selectUnselectAct) = if (noneSelected) {
                ("select all", () => setFlags(flags->selectAllArgs))
            } else {
                ("select none", () => setFlags(flags->unselectAllArgs))
            }
            <Row style=ReactDOM.Style.make(~border="solid lightgrey 1px", ~borderRadius="6px", ~margin="2px", ())>
                {React.string(title)}
                <span
                    onClick={_=> { 
                        actOpenRootStmtsDialog( ~title = dialogTitle, ~stmtToProve, ~getFlags, ~setFlags, ) 
                    }}
                    style=ReactDOM.Style.make(~cursor="pointer", ~color="blue", ())
                >
                    {React.string(numberOfSelected)}
                </span>
                <span
                    onClick={_=> selectUnselectAct() }
                    style=ReactDOM.Style.make(
                        ~cursor="pointer", 
                        ~border="solid lightgrey 0px", 
                        ~borderRadius="10px", ~backgroundColor="rgb(240, 240, 240)", 
                        ~paddingLeft="5px", ~paddingRight="5px", 
                        ()
                    )
                >
                    {React.string(selectUnselectText)}
                </span>
            </Row>
        }
    }

    let actToggleArgs1EqArgs0 = () => {
        setState(toggleArgs1EqArgs0)
    }

    let rndRootStmts = () => {
        if (state.rootStmtsRendered->Array.length == 0) {
            React.null
        } else {
            <Row alignItems=#center spacing=0.2>
                {React.string("Allowed statements: ")}
                {
                    rndRootStmtsForLevelShort(
                        ~title = "first level", 
                        ~titleOnly=false,
                        ~dialogTitle = "Select steps to derive from on level 0", 
                        ~stmtToProve = state.title,
                        ~getFlags = state => state.args0,
                        ~setFlags = newFlags => setState(updateArgs0(_, newFlags)),
                    )
                }
                <IconButton 
                    title="Set allowed statements for other levels same as for the first level" 
                    onClick={_=>actToggleArgs1EqArgs0()}
                    color = ?(if (state.args1EqArgs0) {Some("primary")} else {Some("lightgrey")})
                >
                    <MM_Icons.Pause style=ReactDOM.Style.make(~transform="rotate(-90deg)", ())/>
                </IconButton>
                {
                    rndRootStmtsForLevelShort(
                        ~title = if (state.args1EqArgs0) {"other levels: same as first level"} else {"other levels"}, 
                        ~titleOnly=state.args1EqArgs0,
                        ~dialogTitle = "Select steps to derive from on other levels", 
                        ~stmtToProve = state.title,
                        ~getFlags = state => state.args1,
                        ~setFlags = newFlags => setState(updateArgs1(_, newFlags)),
                    )
                }
            </Row>
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.5>
            {rndTitle()}
            {if (!isApiCall) {rndRootStmts()} else {React.null}}
            {rndParams()}
            {rndResults()}
        </Col>
    </Paper>
}
