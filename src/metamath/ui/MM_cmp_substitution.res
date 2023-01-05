open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_context
open MM_parser

type state = {
    expr1Str: string,
    expr1Err: option<string>,
    expr2Str: string,
    expr2Err: option<string>,
    results: option<array<wrkSubs>>,
    checkedResultIdx: int
}

let makeInitialState = () => {
    {
        expr1Str: "",
        expr1Err: None,
        expr2Str: "",
        expr2Err: None,
        results: None,
        checkedResultIdx: -1
    }
}

let setResults = (st,results):state => {
    {
        ...st,
        results:Some(results),
        checkedResultIdx: if (results->Js_array2.length == 1) {0} else {-1},
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

let toggleResultChecked = (st,idx) => {
    if (st.checkedResultIdx ==idx) {
        {
            ...st,
            checkedResultIdx: -1
        }
    } else {
        {
            ...st,
            checkedResultIdx: idx
        }
    }
}

@react.component
let make = (
    ~editorState: editorState,
    ~wrkCtx:mmContext,
    ~onCanceled:unit=>unit,
    ~onSubstitutionSelected:wrkSubs=>unit
) => {
    let (state, setState) = React.useState(makeInitialState)

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
                if (0 <= state.checkedResultIdx && state.checkedResultIdx < results->Js.Array2.length) {
                    onSubstitutionSelected(results[state.checkedResultIdx])
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

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndExpr = (~label, ~value, ~autoFocus, ~onChange) => {
        <TextField 
            label
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            autoFocus
            value
            onChange=evt2str(onChange)
        />
    }
    
    let rndInput = () => {
        <Col>
            {rndExpr(~label="Replace what", ~value=state.expr1Str, ~autoFocus=true, ~onChange=actExpr1Change)}
            {rndError(state.expr1Err)}
            {rndExpr(~label="Replace with", ~value=state.expr2Str, ~autoFocus=false, ~onChange=actExpr2Change)}
            {rndError(state.expr2Err)}
            <Row>
                <Button onClick={_=>actDetermineSubs()} variant=#contained>
                    {React.string("Extract substitution")}
                </Button>
                <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
            </Row>
        </Col>
    }

    let rndResultButtons = () => {
        let onlyOneSubstitutionIsAvailable = switch state.results {
            | None => false
            | Some(results) => results->Js_array2.length == 1
        }
        <Row>
            <Button onClick={_=>actChooseSelected()} variant=#contained disabled={state.checkedResultIdx < 0}>
                {React.string(if onlyOneSubstitutionIsAvailable {"Apply substitution"} else {"Apply selected"})}
            </Button>
            <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
        </Row>
    }

    let rndWrkSubs = (subs:wrkSubs):React.element => {
        <table>
            <tbody>
            {React.array(
                subs->Belt_MapInt.toArray->Js_array2.map(((v,expr)) => {
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

    let rndResults = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                let numOfResults = results->Js.Array2.length
                if (numOfResults == 0) {
                    <span>
                    {React.string("No substitution can be extracted from the provided expressions.")}
                    </span>
                } else {
                    <Col>
                        {
                            results->Js_array2.mapi((res,i) => {
                                <table key={i->Belt_Int.toString}>
                                    <tbody>
                                        <tr>
                                            {
                                                if (numOfResults > 1) {
                                                    <td>
                                                        <Checkbox
                                                            checked={state.checkedResultIdx == i}
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
                            })->React.array
                        }
                        {rndResultButtons()}
                    </Col>
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