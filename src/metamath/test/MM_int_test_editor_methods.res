open MM_parser
open MM_context
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_search_asrt
open MM_wrk_unify
open MM_substitution
open MM_parenCounter
open MM_statements_dto

let createEditorState = (
    ~mmFilePath:string, 
    ~stopBefore:option<string>=?, 
    ~stopAfter:option<string>=?, 
    ~debug:option<bool>=?, 
    ()
) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore?, ~stopAfter?, ~debug?, ())
    while (ctx->getNestingLevel != 0) {
        ctx->closeChildContext
    }
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let frms = prepareFrmSubsData(ctx)
    let st = {
        settingsV: 1,
        settings: {
            parens,
            typeSettings: [ ],
        },
        typeColors: Belt_HashMapString.make(~hintSize=0),

        preCtxV: 1,
        preCtx: ctx,
        parenCnt: parenCntMake(MM_wrk_ctx.prepareParenInts(ctx, parens), ()),
        frms,
        preCtxColors: Belt_HashMapString.make(~hintSize=0),

        varsText: "",
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: "",
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],

        unifyAllIsRequiredCnt: 0
    }
    recalcAllColors(st)
}

let addStmt = (
    st:editorState, 
    ~before:option<stmtId>=?,
    ~typ:option<userStmtType>=?, 
    ~label:option<string>=?, 
    ~stmt:string, 
    ()
):(editorState,stmtId) => {
    let st = switch before {
        | None => st
        | Some(beforeStmtId) => {
            let st = st->uncheckAllStmts
            st->toggleStmtChecked(beforeStmtId)
        }
    }
    let (st,stmtId) = st->addNewStmt
    let st = st->completeContEditMode(stmtId, stmt)
    let st = switch label {
        | Some(label) => st->completeLabelEditMode(stmtId, label)
        | None => st
    }
    let st = switch typ {
        | Some(typ) => st->completeTypEditMode(stmtId, typ)
        | None => st
    }
    let st = st->uncheckAllStmts
    (st->updateEditorStateWithPostupdateActions(st => st), stmtId)
}

let duplicateStmt = (st, stmtId):(editorState,stmtId) => {
    let st = st->uncheckAllStmts
    let st = st->toggleStmtChecked(stmtId)
    let st = st->duplicateCheckedStmt
    if (st.checkedStmtIds->Js.Array2.length != 1) {
        raise(MmException({msg:`duplicateStmt: st.checkedStmtIds->Js.Array2.length != 1`}))
    } else {
        let newStmtId = st.checkedStmtIds[0]
        let st = st->uncheckAllStmts
        (st->updateEditorStateWithPostupdateActions(st => st), newStmtId)
    }
}

let updateStmt = (
    st, 
    stmtId,
    ~label:option<string>=?,
    ~typ:option<userStmtType>=?,
    ~content:option<string>=?,
    ~contReplaceWhat:option<string>=?,
    ~contReplaceWith:option<string>=?,
    ()
):editorState => {
    let st = switch label {
        | None => st
        | Some(label) => {
            switch st->renameStmt(stmtId, label) {
                | Error(msg) => raise(MmException({msg:msg}))
                | Ok(st) => st
            }
        }
    }
    let st = st->updateStmt(stmtId, stmt => {
        let stmt = switch typ {
            | None => stmt
            | Some(typ) => {...stmt, typ}
        }
        let stmt = switch content {
            | Some(content) => {...stmt, cont:strToCont(content, ())}
            | None => {
                switch (contReplaceWhat, contReplaceWith) {
                    | (Some(contReplaceWhat), Some(contReplaceWith)) => {
                        {
                            ...stmt, 
                            cont: stmt.cont
                                    ->contToStr
                                    ->Js.String2.replace(contReplaceWhat, contReplaceWith)
                                    ->strToCont(_, ())
                        }
                    }
                    | _ => stmt
                }
            }
        }
        stmt
    })
    st->updateEditorStateWithPostupdateActions(st => st)
}

let addStmtsBySearch = (
    st,
    ~addBefore:option<stmtId>=?,
    ~filterLabel:option<string>=?, 
    ~filterTyp:option<string>=?, 
    ~filterPattern:option<string>=?, 
    ~chooseLabel:string,
    ()
):editorState => {
    let st = switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot addStmtsBySearch when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let st = st->uncheckAllStmts
            let st = switch addBefore {
                | None => st
                | Some(stmtId) => st->toggleStmtChecked(stmtId)
            }
            let searchResults = doSearchAssertions(
                ~wrkCtx,
                ~frms=st.frms,
                ~label=filterLabel->Belt_Option.getWithDefault(""),
                ~typ=st.preCtx->ctxSymToIntExn(filterTyp->Belt_Option.getWithDefault("|-")),
                ~pattern=st.preCtx->ctxStrToIntsExn(filterPattern->Belt_Option.getWithDefault("")),
                ()
            )
            let st = switch searchResults->Js_array2.find(res => res.stmts[res.stmts->Js_array2.length-1].label == chooseLabel) {
                | None => 
                    raise(MmException({
                        msg:`addStmtsBySearch: could not find ${chooseLabel}. ` 
                            ++ `Available: ${searchResults->Js_array2.map(res => res.stmts[res.stmts->Js_array2.length-1].label)->Js_array2.joinWith(", ")} `
                    }))
                | Some(searchResult) => st->addNewStatements(searchResult)
            }
            st->uncheckAllStmts
        }
    }
    st->updateEditorStateWithPostupdateActions(st => st)
}

let addNewStmts = (st:editorState, newStmts:stmtsDto, ~before:option<stmtId>=?, ()):editorState => {
    let st = switch before {
        | None => st
        | Some(beforeStmtId) => {
            let st = st->uncheckAllStmts
            st->toggleStmtChecked(beforeStmtId)
        }
    }
    let st = st->addNewStatements(newStmts)
    let st = st->uncheckAllStmts
    st->updateEditorStateWithPostupdateActions(st => st)
}

let getStmtId = (
    st:editorState, 
    ~predicate:option<userStmt=>bool>=?,
    ~contains:option<string>=?, 
    ~label:option<string>=?, 
    ()
) => {
    let predicate = switch predicate {
        | None => _ => true
        | Some(predicate) => predicate
    }
    let predicate = switch contains {
        | None => predicate
        | Some(contains) => stmt => predicate(stmt) && stmt.cont->contToStr->Js.String2.includes(contains)
    }
    let predicate = switch label {
        | None => predicate
        | Some(label) => stmt => predicate(stmt) && stmt.label == label
    }

    let found = st.stmts->Js_array2.filter(predicate)
    if (found->Js_array2.length != 1) {
        raise(MmException({msg:`getStmtId:  found.length = ${found->Js_array2.length->Belt_Int.toString}`}))
    } else {
        found[0].id
    }
}

let applySubstitution = (st, ~replaceWhat:string, ~replaceWith:string):editorState => {
    let st = switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot applySubstitution when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let wrkSubs = findPossibleSubs(
                st, 
                wrkCtx->ctxStrToIntsExn(replaceWhat),
                wrkCtx->ctxStrToIntsExn(replaceWith)
            )->Js.Array2.filter(subs => subs.err->Belt_Option.isNone)
            if (wrkSubs->Js.Array2.length != 1) {
                raise(MmException({msg:`Unique substitution was expected in applySubstitution.`}))
            } else {
                st->applySubstitutionForEditor(wrkSubs[0])
            }
        }
    }
    st->updateEditorStateWithPostupdateActions(st => st)
}

let unifyAll = (st):editorState => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot unifyAll when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let stmts = st.stmts
                ->Js_array2.filter(stmt => stmt.typ == P)
                ->Js_array2.map(stmt => {
                    {
                        label:stmt.label,
                        expr:
                            switch stmt.expr {
                                | None => raise(MmException({msg:`Expr must be set for all statements before unification.`}))
                                | Some(expr) => expr
                            },
                        justification: stmt.jstf,
                    }
                })
            let proofTree = unifyAll(
                ~parenCnt = st.parenCnt,
                ~frms = st.frms,
                ~ctx = wrkCtx,
                ~stmts,
                ()
            )
            let proofTreeDto = proofTree->proofTreeToDto(stmts->Js_array2.map(stmt=>stmt.expr))
            applyUnifyAllResults(st, proofTreeDto)
        }
    }
}

let unifyBottomUp = (
    st,
    ~stmtId:stmtId,
    ~asrtLabel:option<string>=?,
    ~maxSearchDepth:int=4, 
    ~lengthRestriction:lengthRestrict=Less,
    ~allowNewVars:bool=true,
    ~useRootStmtsAsArgs:bool=false,
    ~chooseLabel:string,
    ()
):(editorState, stmtsDto) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot unifyBottomUp when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let st = st->uncheckAllStmts
            let st = st->toggleStmtChecked(stmtId)
            let rootStmts = st->getStmtsForUnification
            let proofTree = MM_provers.unifyAll(
                ~parenCnt = st.parenCnt,
                ~frms = st.frms,
                ~ctx = wrkCtx,
                ~stmts = rootStmts,
                ~bottomUpProverParams = {
                    asrtLabel,
                    maxSearchDepth,
                    lengthRestriction,
                    allowNewVars,
                    useRootStmtsAsArgs
                },
                //~onProgress = msg => Js.Console.log(msg),
                ()
            )
            let proofTreeDto = proofTree->proofTreeToDto(rootStmts->Js_array2.map(stmt=>stmt.expr))
            let newStmts = proofTreeDtoToNewStmtsDto(
                ~treeDto = proofTreeDto, 
                ~rootStmts,
                ~ctx = wrkCtx,
                ~typeToPrefix = 
                    Belt_MapString.fromArray(
                        st.settings.typeSettings->Js_array2.map(ts => (ts.typ, ts.prefix))
                    )
            )
            let resultOpt = newStmts->Js_array2.find(newStmtsDto => {
                let lastStmt = newStmtsDto.stmts[newStmtsDto.stmts->Js_array2.length - 1]
                switch lastStmt.jstf {
                    | Some({label}) => label == chooseLabel
                    | _ => raise(MmException({msg:`Cannot get asrt label from newStmtsDto.`}))
                }
            })
            switch resultOpt {
                | None => raise(MmException({msg:`Cannot find bottom-up result by label.`}))
                | Some(result) => (st, result)
            }
        }
    }
}

let removeAllJstf = (st:editorState):editorState => {
    let st = {...st, stmts: st.stmts->Js.Array2.map(stmt => {...stmt, jstfText:""})}
    st->updateEditorStateWithPostupdateActions(st => st)
}

let newLineRegex = %re("/[\n\r]/")
let addDisj = (st:editorState, disj:string):editorState => {
    let disjLines = st.disjText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    disjLines->Js_array2.push(disj)->ignore
    let st = st->completeDisjEditMode( disjLines->Js.Array2.joinWith("\n") )
    st->updateEditorStateWithPostupdateActions(st => st)
}

let removeDisj = (st:editorState, disj:string):editorState => {
    let disjLines = st.disjText
        ->Js_string2.splitByRe(newLineRegex)
        ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(s => s->Js_string2.length > 0)
    let st = st->completeDisjEditMode(
        disjLines->Js_array2.filter(line => line != disj)->Js.Array2.joinWith("\n")
    )
    st->updateEditorStateWithPostupdateActions(st => st)
}

let mergeStmt = (st:editorState, stmtId):editorState => {
    let st = st->uncheckAllStmts
    let st = st->toggleStmtChecked(stmtId)
    switch st->findStmtsToMerge {
        | Error(msg) => raise(MmException({msg:msg}))
        | Ok((stmt1,stmt2)) => {
            let stmtIdToUse = if (stmt1.id == stmtId) {stmt1.id} else {stmt2.id}
            let stmtIdToRemove = if (stmt1.id == stmtId) {stmt2.id} else {stmt1.id}
            let st = switch st->mergeStmts(stmtIdToUse, stmtIdToRemove) {
                | Ok(st) => st->uncheckAllStmts
                | Error(msg) => raise(MmException({msg:msg}))
            }
            let st = st->uncheckAllStmts
            st->updateEditorStateWithPostupdateActions(st => st)
        }
    }
}