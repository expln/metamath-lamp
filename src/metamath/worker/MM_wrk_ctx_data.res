open MM_context
open MM_parser

type wrkCtxHyp = {
    id: string,
    label: string,
    text: string,
}

type wrkCtxErr = {
    varsErr: option<string>,
    disjErr: option<string>,
    hypErr: option<(string,string)>,
}

let makeEmptyWrkCtxErr = () => {
    varsErr: None,
    disjErr: None,
    hypErr: None,
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

let addVarFromString = (wrkCtx, str) => {
    let arr = getSpaceSeparatedValuesAsArray(str)
    if (arr->Js_array2.length != 3) {
        raise(MmException({msg:`Cannot convert '${str}' to Var statement.`}))
    } else {
        wrkCtx->applySingleStmt(Var({symbols:[arr[2]]}))
        wrkCtx->applySingleStmt(Floating({label:arr[0], expr:[arr[1], arr[2]]}))
    }
}

let newLineRegex = %re("/[\n\r]/")
let parseVariables = (wrkCtx, varsText):option<wrkCtxErr> => {
    let varLines = varsText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    if (varLines->Js.Array2.length == 0) {
        None
    } else {
        try {
            varLines->Js_array2.forEach(addVarFromString(wrkCtx))
            None
        } catch {
            | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), varsErr:Some(msg)})
        }
    }
}

let addDisjFromString = (wrkCtx, disjStr) => {
    wrkCtx->applySingleStmt(Disj({vars:disjStr->Js.String2.split(",")->Js_array2.map(Js_string2.trim)}))
}

let parseDisjoints = (wrkCtx, disjText):option<wrkCtxErr> => {
    let disjLines = disjText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    try {
        disjLines->Js_array2.forEach(addDisjFromString(wrkCtx))
        None
    } catch {
        | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), disjErr:Some(msg)})
    }
}

let addStmtToCtx = (wrkCtx:mmContext, stmt:wrkCtxHyp):option<wrkCtxErr> => {
    try {
        wrkCtx->applySingleStmt(Essential({label:stmt.label, expr:getSpaceSeparatedValuesAsArray(stmt.text)}))
        None
    } catch {
        | MmException({msg}) => Some({...makeEmptyWrkCtxErr(), hypErr:Some((stmt.id,msg))})
    }
}

let createWrkCtx = (
    ~preCtx: mmContext,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
): result<mmContext,wrkCtxErr> => {
    let wrkCtx = createContext(~parent=preCtx, ())
    let err:option<wrkCtxErr> = [
        () => parseVariables(wrkCtx, varsText),
        () => parseDisjoints(wrkCtx, disjText),
        () => hyps->Js.Array2.reduce(
            (err,stmt) => {
                switch err {
                    | Some(_) => err
                    | None => addStmtToCtx(wrkCtx, stmt)
                }
            },
            None
        )
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
