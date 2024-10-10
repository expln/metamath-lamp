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
    let maxI = parenSyms->Array.length / 2 - 1
    for i in 0 to maxI {
        let leftParen = parenSyms->Array.getUnsafe(i*2)
        let rightParen = parenSyms->Array.getUnsafe(i*2+1)
        switch wrkCtx->ctxSymToInt(leftParen) {
            | Some(leftParenInt) if wrkCtx->isConst(leftParen) => {
                switch wrkCtx->ctxSymToInt(rightParen) {
                    | Some(rightParenInt) if wrkCtx->isConst(rightParen) => {
                        parenInts->Array.push(leftParenInt)
                        parenInts->Array.push(rightParenInt)
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
    if (arr->Array.length != 3) {
        Error(`A line representing a variable definition should consist of exactly three parts` 
                        ++ ` separated with a whitespace.`)
    } else {
        Ok(arr)
    }
}

let textToVarDefs = (text:string):result<array<array<string>>,string> => {
    let varLines = text->multilineTextToNonEmptyLines
    if (varLines->Array.length == 0) {
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
                                varDefs->Array.push(varDef)
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
                varDefs->Array.forEach(varDef => {
                    wrkCtx->applySingleStmt(Var({symbols:[varDef->Array.getUnsafe(2)]}), ())
                    wrkCtx->applySingleStmt(Floating({label:varDef->Array.getUnsafe(0), expr:[varDef->Array.getUnsafe(1), varDef->Array.getUnsafe(2)]}), ())
                })
                None
            } catch {
                | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), varsErr:Some(msg)})
            }
        }
    }
}

let addDisjFromString = (wrkCtx, disjStr) => {
    wrkCtx->applySingleStmt(Disj({
        vars:disjStr
            ->String.split(" ")
            ->Array.map(String.trim(_))
            ->Array.filter(str => str != "")
    }), ())
}

let parseDisjoints = (wrkCtx, disjText):option<wrkCtxErr> => {
    try {
        disjText->multilineTextToNonEmptyLines->Array.forEach(addDisjFromString(wrkCtx, _))
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
