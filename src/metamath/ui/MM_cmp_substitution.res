open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_wrk_editor_substitution
open MM_context
open Expln_utils_promise
open Expln_React_Modal
open Common
open MM_react_common

type state = {
    expr1Str: string,
    expr1Err: option<string>,
    expr2Str: string,
    expr2Err: option<string>,
    results: option<result<array<wrkSubs>,string>>,
    checkedResultIdx: option<int>,
    invalidResults: option<array<wrkSubs>>,
}

let makeInitialState = (
    ~expr1Init:option<string>,
    ~expr2Init:option<string>,
) => {
    {
        expr1Str: expr1Init->Belt.Option.getWithDefault(""),
        expr1Err: None,
        expr2Str: expr2Init->Belt.Option.getWithDefault(""),
        expr2Err: None,
        results: None,
        checkedResultIdx: None,
        invalidResults: None,
    }
}

let setResults = (st:state,results:result<array<wrkSubs>,string>):state => {
    let validResults = switch results {
        | Error(msg) => Error(msg)
        | Ok(results) => Ok(results->Array.filter(res => res.err->Belt_Option.isNone))
    }
    let invalidResults = switch results {
        | Error(_) => []
        | Ok(results) => results->Array.filter(res => res.err->Belt_Option.isSome)
    }
    let checkedResultIdx = switch validResults {
        | Error(_) => None
        | Ok(validResults) => if (validResults->Array.length == 1) {Some(0)} else {None}
    }
    {
        ...st,
        results:Some(validResults),
        checkedResultIdx,
        invalidResults:Some(invalidResults),
    }
}

let clearResults = (st):state => {
    {
        ...st,
        results: None,
        checkedResultIdx: None,
        invalidResults: None,
    }
}

let setExpr1Str = (st,str):state => {
    {
        ...st,
        expr1Str:str
    }
}

let setExpr2Str = (st,str):state => {
    {
        ...st,
        expr2Str:str
    }
}

let swapExprs = (st):state => {
    {
        ...st,
        expr1Err:None,
        expr2Err:None,
        expr1Str: st.expr2Str,
        expr2Str: st.expr1Str,
    }
}

let copyExpr1ToExpr2 = (st):state => {
    {
        ...st,
        expr1Err:None,
        expr2Err:None,
        expr2Str: st.expr1Str,
    }
}

let copyExpr2ToExpr1 = (st):state => {
    {
        ...st,
        expr1Err:None,
        expr2Err:None,
        expr1Str: st.expr2Str,
    }
}

let toggleResultChecked = (st,idx) => {
    if (st.checkedResultIdx == Some(idx)) {
        {
            ...st,
            checkedResultIdx: None
        }
    } else {
        {
            ...st,
            checkedResultIdx: Some(idx)
        }
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool, ~title:option<string>=?, ()) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
    </span>
}

let findSubsByMatch = "match"
let findSubsByUnif = "unif"

@react.component
let make = (
    ~modalRef:modalRef,
    ~editorState: editorState,
    ~expr1Init:option<string>,
    ~expr2Init:option<string>,
    ~wrkCtx:mmContext,
    ~onCanceled:unit=>unit,
    ~onSubstitutionSelected:wrkSubs=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState(~expr1Init, ~expr2Init))
    let (findSubsBy, setFindSubsBy) = Local_storage_utils.useStateFromLocalStorageStr(
        ~key="find-substitution-by", ~default=findSubsByMatch
    )
    let expr1TextFieldRef = React.useRef(Nullable.null)
    let expr2TextFieldRef = React.useRef(Nullable.null)

    let methodName = if (findSubsBy == findSubsByMatch) {"Match"} else {"Unify"}

    let actSaveResults = (results:result<array<wrkSubs>,string>) => {
        setState(setResults(_, results))
    }

    let actClearResults = () => {
        setState(clearResults)
    }

    let actDetermineSubs = () => {
        let findIncorrectSymbol = syms => syms->Array.find(sym => !(wrkCtx->isConst(sym) || wrkCtx->isVar(sym)))
        let syms1 = state.expr1Str->getSpaceSeparatedValuesAsArray
        let incorrectSymbol1 = findIncorrectSymbol(syms1)
        let syms2 = state.expr2Str->getSpaceSeparatedValuesAsArray
        let incorrectSymbol2 = findIncorrectSymbol(syms2)
        setState(st => {
            let st = switch incorrectSymbol1 {
                | Some(sym) => {...st, expr1Err:Some(`Unknown symbol - '${sym}'`)}
                | None => {...st, expr1Err:None}
            }
            let st = switch incorrectSymbol2 {
                | Some(sym) => {...st, expr2Err:Some(`Unknown symbol - '${sym}'`)}
                | None => {...st, expr2Err:None}
            }
            st
        })
        if (
            incorrectSymbol1->Belt.Option.isNone && incorrectSymbol2->Belt.Option.isNone
            && syms1->Array.length > 0 && syms2->Array.length > 0
        ) {
            actSaveResults(
                findPossibleSubs(
                    editorState, 
                    wrkCtx->ctxSymsToIntsExn(syms1),
                    wrkCtx->ctxSymsToIntsExn(syms2),
                    findSubsBy == findSubsByMatch,
                )
            )
        } else {
            actClearResults()
        }
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = () => {
        switch state.results {
            | None => ()
            | Some(results) => {
                switch results {
                    | Error(_) => ()
                    | Ok(results) => {
                        switch state.checkedResultIdx {
                            | Some(idx) => {
                                if (0 <= idx && idx < results->Array.length) {
                                    onSubstitutionSelected(results->Array.getUnsafe(idx))
                                }
                            }
                            | None => ()
                        } 
                    }
                }
            }
        }
    }

    let actFocus = (ref:React.ref<Nullable.t<Dom.element>>) => {
        switch ref.current->Nullable.toOption {
            | None => ()
            | Some(domElem) => {
                let input = ReactDOM.domElementToObj(domElem)
                switch input["focus"] {
                    | None => ()
                    | Some(_) => input["focus"](.)
                }
            }
        }
    }

    let actExpr1OnEnter = () => {
        if (state.expr2Str->String.trim == "") {
            actFocus(expr2TextFieldRef)
        } else {
            actDetermineSubs()
        }
    }

    let actExpr2OnEnter = () => {
        if (state.expr1Str->String.trim == "") {
            actFocus(expr1TextFieldRef)
        } else {
            actDetermineSubs()
        }
    }

    let actExpr1Change = str => {
        actClearResults()
        setState(setExpr1Str(_,str))
    }

    let actExpr2Change = str => {
        actClearResults()
        setState(setExpr2Str(_,str))
    }

    let actSwapExprs = () => {
        actClearResults()
        setState(swapExprs)
    }

    let actCopyExpr1ToExpr2 = () => {
        actClearResults()
        setState(copyExpr1ToExpr2)
    }

    let actCopyExpr2ToExpr1 = () => {
        actClearResults()
        setState(copyExpr2ToExpr1)
    }

    let actFindSubsByChange = newValue => {
        actClearResults()
        setFindSubsBy(_ => if (newValue == findSubsByMatch) {findSubsByMatch} else {findSubsByUnif})
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndExpr = (~label, ~value, ~autoFocus, ~onChange, ~tabIndex:int, 
        ~onEnter:unit=>unit, ~ref:React.ref<Nullable.t<Dom.element>>) => {
        <TextField 
            inputRef=ReactDOM.Ref.domRef(ref)
            label
            size=#small
            style=ReactDOM.Style.make(~width="700px", ())
            autoFocus
            value
            onChange=evt2str(onChange)
            inputProps={"tabIndex":tabIndex}
            onKeyDown=kbrdHnd2(
                kbrdClbkMake(~key=keyEnter, ~act=onEnter, ()),
                kbrdClbkMake(~key=keyEsc, ~act=onCanceled, ()),
            )
        />
    }

    let expr1Label = `${methodName} what`
    let expr2Label = `${methodName} with`
    
    let rndInput = () => {
        <Col>
            <Row alignItems=#center>
                {React.string("Find substitution by:")}
                <RadioGroup
                    row=true
                    value=findSubsBy
                    onChange=evt2str(actFindSubsByChange)
                >
                    <FormControlLabel value=findSubsByMatch control={ <Radio/> } label="Matching" />
                    <FormControlLabel value=findSubsByUnif control={ <Radio/> } label="Unification" />
                </RadioGroup>
            </Row>
            <table>
                <tbody>
                    <tr>
                        <td>
                            {rndExpr(~label=expr1Label, ~value=state.expr1Str, ~autoFocus=true, 
                                ~onChange=actExpr1Change, ~tabIndex=1, ~onEnter=actExpr1OnEnter, 
                                ~ref=expr1TextFieldRef)}
                        </td>
                        <td>
                            {rndIconButton(
                                ~icon=<MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />, 
                                ~onClick={_=>actCopyExpr1ToExpr2()}, ~active=true, 
                                ~title=`Copy this statement to the below text field`, ())}
                        </td>
                        <td>
                            {rndIconButton(~icon=<MM_Icons.SwapVert />, ~onClick={_=>actSwapExprs()}, ~active=true, 
                                ~title=`Swap "${expr1Label}" and "${expr2Label}"`, ())}
                        </td>
                    </tr>
                </tbody>
            </table>
            {rndError(state.expr1Err)}
            <table>
                <tbody>
                    <tr>
                        <td>
                            {rndExpr(~label=expr2Label, ~value=state.expr2Str, ~autoFocus=false,
                                    ~onChange=actExpr2Change, ~tabIndex=2, ~onEnter=actExpr2OnEnter,
                                    ~ref=expr2TextFieldRef )}
                        </td>
                        <td>
                            {rndIconButton(
                                ~icon=<MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />, 
                                ~onClick={_=>actCopyExpr2ToExpr1()}, ~active=true, 
                                ~title=`Copy this statement to the above text field`, ())}
                        </td>
                    </tr>
                </tbody>
            </table>
            {rndError(state.expr2Err)}
            <Row>
                <Button onClick={_=>actDetermineSubs()} variant=#contained color="grey" >
                    {React.string("Find substitution")}
                </Button>
                <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
            </Row>
        </Col>
    }

    let rndResultButtons = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                switch results {
                    | Error(_) => React.null
                    | Ok(results) => {
                        let numOfResults = results->Array.length
                        if (numOfResults > 0) {
                            <Row>
                                <Button onClick={_=>actChooseSelected()} variant=#contained
                                        disabled={state.checkedResultIdx->Belt.Option.isNone}>
                                    {React.string(if numOfResults == 1 {"Apply"} else {"Apply selected"})}
                                </Button>
                                <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
                            </Row>
                        } else {
                            React.null
                        }
                    }
                }
            }
        }
    }

    let rndNewDisj = (wrkSubs:wrkSubs):React.element => {
        if (wrkSubs.newDisj->disjIsEmpty) {
            React.null
        } else {
            let tableRows = []
            wrkSubs.newDisj->disjForEachArr(disjGrp => {
                let disjGrpStr = "$d " ++ wrkCtx->ctxIntsToStrExn(disjGrp) ++ " $."
                tableRows->Array.push(
                    <tr key=disjGrpStr>
                        <td>
                            {React.string(disjGrpStr)}
                        </td>
                    </tr>
                )
            })
            <table>
                <tbody>
                    {tableRows->React.array}
                </tbody>
            </table>
        }
    }

    let rndSubs = (wrkSubs:wrkSubs):React.element => {
        <table>
            <tbody>
            {React.array(
                wrkSubs.subs->Belt_MapInt.toArray->Array.map(((v,expr)) => {
                    if (expr->Array.length == 1 && v == expr->Array.getUnsafe(0)) {
                        React.null
                    } else {
                        <tr key={v->Belt_Int.toString}>
                            <td>
                                {React.string(wrkCtx->ctxIntToSymExn(v))}
                            </td>
                            <td>
                                {React.string("\u2192")}
                            </td>
                            <td>
                                {React.string(wrkCtx->ctxIntsToStrExn(expr))}
                            </td>
                        </tr>
                    }
                })
            )}
            </tbody>
        </table>
    }

    let rndSubsErr = (err:option<wrkSubsErr>):React.element => {
        switch err {
            | None => React.null
            | Some(err) => {
                let errDescr = switch err {
                    | CommonVar({var1, var2, commonVar}) => {
                        `substitutions for disjoint variables ${wrkCtx->ctxIntToSymExn(var1)} and ${wrkCtx->ctxIntToSymExn(var2)} `
                            ++ `have a common variable ${wrkCtx->ctxIntToSymExn(commonVar)}`
                    }
                    | TypeMismatch({var, subsExpr, typeExpr}) => {
                        `could not prove [${wrkCtx->ctxIntsToStrExn(typeExpr)}] for ${wrkCtx->ctxIntToSymExn(var)} `
                            ++ String.fromCharCode(8594) ++ ` [${wrkCtx->ctxIntsToStrExn(subsExpr)}]`
                    }
                }
                <div>
                    <span style=ReactDOM.Style.make(~color="red", ())>
                        {React.string("Error: ")}
                    </span>
                    {React.string(errDescr)}
                </div>
            }
        }

    }

    let rndWrkSubs = (wrkSubs:wrkSubs):React.element => {
        <>
            {rndNewDisj(wrkSubs)}
            {rndSubs(wrkSubs)}
            {rndSubsErr(wrkSubs.err)}
        </>
    }

    let actShowInvalidSubs = ():unit => {
        switch state.invalidResults {
            | None => ()
            | Some(invalidResults) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <Col spacing=1. style=ReactDOM.Style.make(~margin="10px", ())>
                            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                                {React.string("Invalid substitutions:")}
                            </span>
                            {
                                invalidResults->Array.mapWithIndex((res,i) => {
                                    <Paper key={i->Belt_Int.toString}>
                                        <table>
                                            <tbody>
                                                <tr>
                                                    <td>
                                                        {rndWrkSubs(res)}
                                                    </td>
                                                </tr>
                                            </tbody>
                                        </table>
                                    </Paper>
                                })->React.array
                            }
                            <Button onClick={_=>closeModal(modalRef, modalId)} variant=#contained>
                                {React.string("Close")}
                            </Button>
                        </Col>
                    })
                })->ignore
            }
        }
    }

    let getNumberOfResults = (arrOpt:option<array<'a>>):int => {
        arrOpt->Belt_Option.map(arr => arr->Array.length)->Belt.Option.getWithDefault(0)
    }

    let rndResults = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                switch results {
                    | Error(msg) => React.string(`Error: ${msg}`)
                    | Ok(results) => {
                        let numOfInvalidResults = getNumberOfResults(state.invalidResults)
                        let summary = 
                            <>
                                {
                                    React.string(
                                        `Found substitutions: `
                                            ++ `${results->Array.length->Belt_Int.toString} valid, `
                                    )
                                }
                                {
                                    if (numOfInvalidResults == 0) {
                                        React.string( `0 invalid.` )
                                    } else {
                                        <span
                                            onClick={_=> actShowInvalidSubs() }
                                            style=ReactDOM.Style.make(~cursor="pointer", ~color="blue", ())
                                        >
                                            {React.string( numOfInvalidResults->Belt_Int.toString ++ ` invalid.` )}
                                        </span>
                                    }
                                }
                            </>
                        let numOfResults = results->Array.length
                        <Col>
                            summary
                            {
                                results->Array.mapWithIndex((res,i) => {
                                    <Paper key={i->Belt_Int.toString}>
                                        <table>
                                            <tbody>
                                                <tr>
                                                    {
                                                        if (numOfResults > 1) {
                                                            <td>
                                                                <Checkbox
                                                                    checked={state.checkedResultIdx == Some(i)}
                                                                    onChange={_ => actToggleResultChecked(i)}
                                                                />
                                                            </td>
                                                        } else {
                                                            React.null
                                                        }
                                                    }
                                                    <td>
                                                        {rndWrkSubs(res)}
                                                    </td>
                                                </tr>
                                            </tbody>
                                        </table>
                                    </Paper>
                                })->React.array
                            }
                            {rndResultButtons()}
                        </Col>
                    }
                }
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndInput()}
            {rndResults()}
        </Col>
    </Paper>
}