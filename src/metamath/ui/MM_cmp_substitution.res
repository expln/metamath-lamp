open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_context
open Expln_utils_promise
open Expln_React_Modal
open Common

type state = {
    expr1Str: string,
    expr1Err: option<string>,
    expr2Str: string,
    expr2Err: option<string>,
    results: option<array<wrkSubs>>,
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

let setResults = (st,results):state => {
    {
        ...st,
        results:Some(results->Js_array2.filter(res => res.err->Belt_Option.isNone)),
        checkedResultIdx: if (results->Js_array2.length == 1) {Some(0)} else {None},
        invalidResults:Some(results->Js_array2.filter(res => res.err->Belt_Option.isSome)),
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

    let actSaveResults = results => {
        setState(setResults(_, results))
    }

    let actDetermineSubs = () => {
        let findIncorrectSymbol = syms => syms->Js_array2.find(sym => !(wrkCtx->isConst(sym) || wrkCtx->isVar(sym)))
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
        if (incorrectSymbol1->Belt.Option.isNone && incorrectSymbol2->Belt.Option.isNone) {
            actSaveResults(
                findPossibleSubs(
                    editorState, 
                    wrkCtx->ctxSymsToIntsExn(syms1),
                    wrkCtx->ctxSymsToIntsExn(syms2),
                )
            )
        }
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = () => {
        switch state.results {
            | None => ()
            | Some(results) => {
                switch state.checkedResultIdx {
                    | Some(idx) => {
                        if (0 <= idx && idx < results->Js.Array2.length) {
                            onSubstitutionSelected(results[idx])
                        }
                    }
                    | None => ()
                } 
            }
        }
    }

    let actExpr1Change = str => {
        setState(setExpr1Str(_,str))
    }

    let actExpr2Change = str => {
        setState(setExpr2Str(_,str))
    }

    let actSwapExprs = () => {
        setState(swapExprs)
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndExpr = (~label, ~value, ~autoFocus, ~onChange, ~tabIndex:int) => {
        <TextField 
            label
            size=#small
            style=ReactDOM.Style.make(~width="700px", ())
            autoFocus
            value
            onChange=evt2str(onChange)
            inputProps={"tabIndex":tabIndex}
        />
    }
    
    let rndInput = () => {
        <Col>
            <table>
                <tbody>
                    <tr>
                        <td>
                            {rndExpr(~label="Replace what", ~value=state.expr1Str, ~autoFocus=true, 
                                ~onChange=actExpr1Change, ~tabIndex=1)}
                        </td>
                        <td>
                            {rndIconButton(~icon=<MM_Icons.SwapVert />, ~onClick={_=>actSwapExprs()}, ~active=true, 
                                ~title="Swap \"Replace what\" and \"Replace with\"", ())}
                        </td>
                    </tr>
                </tbody>
            </table>
            {rndError(state.expr1Err)}
            {rndExpr(~label="Replace with", ~value=state.expr2Str, ~autoFocus=false,
                ~onChange=actExpr2Change, ~tabIndex=2)}
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
                let numOfResults = results->Js_array2.length
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

    let rndNewDisj = (wrkSubs:wrkSubs):React.element => {
        if (wrkSubs.newDisj->disjIsEmpty) {
            React.null
        } else {
            let tableRows = []
            wrkSubs.newDisj->disjForEachArr(disjGrp => {
                let disjGrpStr = "$d " ++ wrkCtx->ctxIntsToStrExn(disjGrp) ++ " $."
                tableRows->Js.Array2.push(
                    <tr key=disjGrpStr>
                        <td>
                            {React.string(disjGrpStr)}
                        </td>
                    </tr>
                )->ignore
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
                wrkSubs.subs->Belt_MapInt.toArray->Js_array2.map(((v,expr)) => {
                    if (expr->Js_array2.length == 1 && v == expr[0]) {
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
                            ++ Js_string2.fromCharCode(8594) ++ ` [${wrkCtx->ctxIntsToStrExn(subsExpr)}]`
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
                                invalidResults->Js_array2.mapi((res,i) => {
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
        arrOpt->Belt_Option.map(arr => arr->Js.Array2.length)->Belt.Option.getWithDefault(0)
    }

    let rndResults = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                let numOfInvalidResults = getNumberOfResults(state.invalidResults)
                let summary = 
                    <>
                        {
                            React.string(
                                `Found substitutions: `
                                    ++ `${getNumberOfResults(state.results)->Belt_Int.toString} valid, `
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
                let numOfResults = results->Js.Array2.length
                <Col>
                    summary
                    {
                        results->Js_array2.mapi((res,i) => {
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

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndInput()}
            {rndResults()}
        </Col>
    </Paper>
}