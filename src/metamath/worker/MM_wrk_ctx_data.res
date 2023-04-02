open MM_context
open MM_parser
open Common

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
}

let makeEmptyWrkCtxErr = () => {
    varsErr: None,
    disjErr: None,
}

let prepareParenInts = (wrkCtx, parenStr) => {
    let parenSyms = parenStr->getSpaceSeparatedValuesAsArray
    let parenInts = []
    let maxI = parenSyms->Js_array2.length / 2 - 1
    for i in 0 to maxI {
        let leftParen = parenSyms[i*2]
        let rightParen = parenSyms[i*2+1]
        switch wrkCtx->ctxSymToInt(leftParen) {
            | Some(leftParenInt) if wrkCtx->isConst(leftParen) => {
                switch wrkCtx->ctxSymToInt(rightParen) {
                    | Some(rightParenInt) if wrkCtx->isConst(rightParen) => {
                        parenInts->Js.Array2.push(leftParenInt)->ignore
                        parenInts->Js.Array2.push(rightParenInt)->ignore
                    }
                    | _ => ()
                }
            }
            | _ => ()
        }
    }
    parenInts
}

let lineToVarDef = (line:string):result<array<string>,string> => {
    let arr = getSpaceSeparatedValuesAsArray(line)
    if (arr->Js_array2.length != 3) {
        Error(`A line representing a variable definition should consist of exactly three parts` 
                        ++ ` separated with a whitespace.`)
    } else {
        Ok(arr)
    }
}

let newLineRegex = %re("/[\n\r]/")
let textToVarDefs = (text:string):result<array<array<string>>,string> => {
    let varLines = text
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(strOpt => strOpt->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(str => str->Js_string2.length > 0)
    if (varLines->Js.Array2.length == 0) {
        Ok([])
    } else {
        varLines->Js_array2.reduce(
            (res,line) => {
                switch res {
                    | Error(_) => res
                    | Ok(varDefs) => {
                        switch lineToVarDef(line) {
                            | Error(msg) => Error(msg)
                            | Ok(varDef) => {
                                varDefs->Js_array2.push(varDef)->ignore
                                Ok(varDefs)
                            }
                        }
                    }
                }
            },
            Ok([])
        )
    }
}

let parseVariables = (wrkCtx, varsText):option<wrkCtxErr> => {
    switch textToVarDefs(varsText) {
        | Error(msg) => Some({...makeEmptyWrkCtxErr(), varsErr:Some(msg)})
        | Ok(varDefs) => {
            try {
                varDefs->Js_array2.forEach(varDef => {
                    wrkCtx->applySingleStmt(Var({symbols:[varDef[2]]}))
                    wrkCtx->applySingleStmt(Floating({label:varDef[0], expr:[varDef[1], varDef[2]]}))
                })
                None
            } catch {
                | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), varsErr:Some(msg)})
            }
        }
    }
}

let addDisjFromString = (wrkCtx, disjStr) => {
    wrkCtx->applySingleStmt(Disj({vars:disjStr->Js.String2.split(",")->Js_array2.map(Js_string2.trim)}))
}

let parseDisjoints = (wrkCtx, disjText):option<wrkCtxErr> => {
    try {
        disjText->multilineTextToNonEmptyLines->Js_array2.forEach(addDisjFromString(wrkCtx))
        None
    } catch {
        | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), disjErr:Some(msg)})
    }
}

let createWrkCtx = (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
): result<mmContext,wrkCtxErr> => {
    let wrkCtx = createContext(~parent=preCtx, ())
    let err:option<wrkCtxErr> = [
        () => parseVariables(wrkCtx, varsText),
        () => parseDisjoints(wrkCtx, disjText),
    ]->Js.Array2.reduce(
        (err,nextStep) => {
            switch err {
                | Some(_) => err
                | None => nextStep()
            }
        },
        None
    )
    switch err {
        | Some(err) => Error(err)
        | None => Ok(wrkCtx)
    }
}
