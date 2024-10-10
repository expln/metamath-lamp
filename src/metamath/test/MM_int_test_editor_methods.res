open MM_parser
open MM_context
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers
open MM_wrk_editor
open MM_wrk_editor_substitution
open MM_wrk_settings
open MM_wrk_search_asrt
open MM_wrk_unify
open MM_statements_dto
open MM_wrk_editor_json
open MM_wrk_pre_ctx_data
open MM_int_test_utils
open Common

type rootStmtsToUse =
    | AllStmts
    | NoneStmts
    | SomeStmts(array<stmtId>)

let createEditorState = (
    ~mmFilePath:string, 
    ~stopBefore:option<string>=?, 
    ~stopAfter:option<string>=?, 
    ~editorState:option<string>=?,
    ~debug:option<bool>=?, 
    ()
) => {
    let parens = "( ) { } [ ]"
    let settings = {
        parens,
        asrtsToSkip: [],
        descrRegexToDisc: "",
        labelRegexToDisc: "^(ax-frege54c)|(.+OLD)|(.+ALT)$",
        descrRegexToDepr: "",
        labelRegexToDepr: "",
        discColor:"",
        deprColor:"",
        tranDeprColor:"",
        editStmtsByLeftClick:true,
        initStmtIsGoal: false,
        defaultStmtLabel: "qed",
        defaultStmtType: "",
        unifMetavarPrefix: "&",
        sortDisjByType:"class wff",
        checkSyntax: true,
        stickGoalToBottom: true,
        autoMergeStmts: false,
        autoUnifyAll: false,
        typeSettings: [ ],
        webSrcSettings: [ ],
        longClickEnabled: true,
        longClickDelayMs: 500,
        hideContextSelector: false,
        showVisByDefault:false,
        editorHistMaxLength:0,
        allowedFrms: {
            inSyntax: {
                useDisc:false,
                useDepr:true,
                useTranDepr:true,
            },
            inEssen: {
                useDisc:false,
                useDepr:true,
                useTranDepr:true,
            },
        },
        useDefaultTransforms:false,
        useCustomTransforms:false,
        customTransforms:"",
        combCntMax:10000,
    }

    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ~skipComments=true, ~skipProofs=true, ())
    let ctx = loadContext(
        ast, 
        ~stopBefore?, 
        ~stopAfter?, 
        ~descrRegexToDisc=settings.descrRegexToDisc->strToRegex->Belt_Result.getExn,
        ~labelRegexToDisc=settings.labelRegexToDisc->strToRegex->Belt_Result.getExn,
        ~descrRegexToDepr=settings.descrRegexToDepr->strToRegex->Belt_Result.getExn,
        ~labelRegexToDepr=settings.labelRegexToDepr->strToRegex->Belt_Result.getExn,
        ~debug?, 
        ()
    )
    while (ctx->getNestingLevel != 0) {
        ctx->closeChildContext
    }
    
    let st = createInitialEditorState(
        ~preCtxData=preCtxDataMake(~settings)->preCtxDataUpdate(~ctx=([],ctx), ()),
        ~stateLocStor=
            switch editorState {
                | None => None
                | Some(fileName) => {
                    readEditorStateFromJsonStr(
                        Expln_utils_files.readStringFromFile(
                            getTestDataDir() ++ "/" ++ fileName ++ ".json"
                        )
                    )->Belt.Result.mapWithDefault(None, state => Some(state))
                }
            }
    )
    st->verifyEditorState
}

let addStmt = (
    st:editorState, 
    ~before:option<stmtId>=?,
    ~typ:option<userStmtType>=?, 
    ~isGoal:bool=false,
    ~label:option<string>=?, 
    ~jstf:option<string>=?, 
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
    let (st,stmtId) = st->addNewStmt(())
    let st = st->completeContEditMode(stmtId, stmt)
    let st = switch label {
        | Some(label) => st->completeLabelEditMode(stmtId, label)
        | None => st
    }
    let st = switch typ {
        | Some(typ) => st->completeTypEditMode(stmtId, typ, isGoal)
        | None => st
    }
    let st = switch jstf {
        | Some(jstf) => st->completeJstfEditMode(stmtId, jstf)
        | None => st
    }
    let st = st->uncheckAllStmts
    (st->verifyEditorState, stmtId)
}

let duplicateStmt = (st, stmtId):(editorState,stmtId) => {
    let st = st->uncheckAllStmts
    let st = st->toggleStmtChecked(stmtId)
    let st = st->duplicateCheckedStmt(false)
    if (st.checkedStmtIds->Js.Array2.length != 1) {
        raise(MmException({msg:`duplicateStmt: st.checkedStmtIds->Js.Array2.length != 1`}))
    } else {
        let (newStmtId,_) = st.checkedStmtIds->Array.getUnsafe(0)
        let st = st->uncheckAllStmts
        (st->verifyEditorState, newStmtId)
    }
}

let updateStmt = (
    st, 
    stmtId,
    ~label:option<string=>string>=?,
    ~typ:option<userStmtType>=?,
    ~content:option<string>=?,
    ~jstf:option<string>=?,
    ~contReplaceWhat:option<string>=?,
    ~contReplaceWith:option<string>=?,
    ()
):editorState => {
    let st = switch label {
        | None => st
        | Some(label) => {
            let oldLabel = (st->editorGetStmtById(stmtId)->Belt_Option.getExn).label
            switch st->renameStmt(stmtId, label(oldLabel)) {
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
        let stmt = switch jstf {
            | None => stmt
            | Some(jstf) => {...stmt, jstfText:jstf}
        }
        let stmt = switch content {
            | Some(_) => stmt
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
    let st = switch content {
        | Some(content) => st->completeContEditMode(stmtId, content)
        | None => st
    }
    st->verifyEditorState
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
            let st = switch searchResults->Array.find(res => (res.stmts->Array.getUnsafe(res.stmts->Js_array2.length-1)).label == chooseLabel) {
                | None => 
                    raise(MmException({
                        msg:`addStmtsBySearch: could not find ${chooseLabel}. ` 
                            ++ `Available: ${searchResults->Array.map(res => (res.stmts->Array.getUnsafe(res.stmts->Js_array2.length-1)).label)->Array.joinUnsafe(", ")} `
                    }))
                | Some(searchResult) => st->addNewStatements(searchResult, ())
            }
            st->uncheckAllStmts
        }
    }
    st->verifyEditorState
}

let addNewStmts = (st:editorState, newStmts:stmtsDto, ~before:option<stmtId>=?, ()):editorState => {
    assertNoErrors(st)
    let st = switch before {
        | None => st
        | Some(beforeStmtId) => {
            let st = st->uncheckAllStmts
            st->toggleStmtChecked(beforeStmtId)
        }
    }
    let st = st->addNewStatements(newStmts, ())
    let st = st->uncheckAllStmts
    st->verifyEditorState
}

let getStmt = (
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

    let found = st.stmts->Array.filter(predicate)
    if (found->Js_array2.length != 1) {
        raise(MmException({msg:`getStmt:  found.length = ${found->Js_array2.length->Belt_Int.toString}`}))
    } else {
        found->Array.getUnsafe(0)
    }
}

let getStmtId = (
    st:editorState, 
    ~predicate:option<userStmt=>bool>=?,
    ~contains:option<string>=?, 
    ~label:option<string>=?, 
    ()
) => {
    getStmt( st, ~predicate?, ~contains?, ~label?, () ).id
}

let deleteStmts = (st:editorState, ids:array<stmtId> ) => {
    let st = st->uncheckAllStmts
    let st = ids->Js_array2.reduce(
        (st, id) => st->toggleStmtChecked(id),
        st
    )
    let st = st->deleteCheckedStmts
    st->verifyEditorState
}

let applySubstitution = (st, ~replaceWhat:string, ~replaceWith:string, ~useMatching:bool):editorState => {
    assertNoErrors(st)
    let st = switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot applySubstitution when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let wrkSubs = findPossibleSubs(
                st, 
                wrkCtx->ctxStrToIntsExn(replaceWhat),
                wrkCtx->ctxStrToIntsExn(replaceWith),
                useMatching
            )->Belt.Result.getExn->Array.filter(subs => subs.err->Belt_Option.isNone)
            if (wrkSubs->Js.Array2.length != 1) {
                raise(MmException({msg:`Unique substitution was expected in applySubstitution.`}))
            } else {
                st->applySubstitutionForEditor(wrkSubs->Array.getUnsafe(0))
            }
        }
    }
    st->verifyEditorState
}

let unifyAll = (st):editorState => {
    assertNoErrors(st)
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot unifyAll when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let rootStmts = st->getRootStmtsForUnification->Array.map(userStmtToRootStmt)
            let proofTree = unifyAll(
                ~parenCnt = st.parenCnt,
                ~frms = st.frms,
                ~allowedFrms = st.settings.allowedFrms,
                ~combCntMax = st.settings.combCntMax,
                ~wrkCtx,
                ~rootStmts,
                ~syntaxTypes=st.syntaxTypes,
                ~exprsToSyntaxCheck=st->getAllExprsToSyntaxCheck(rootStmts),
                ()
            )
            let proofTreeDto = proofTree->proofTreeToDto(rootStmts->Array.map(stmt=>stmt.expr))
            applyUnifyAllResults(st, proofTreeDto)
        }
    }
}

let filterRootStmts = (stmts:array<userStmt>, rootStmtsToUse:rootStmtsToUse):array<expr> => {
    let stmtsFiltered = switch rootStmtsToUse {
        | AllStmts => stmts
        | NoneStmts => []
        | SomeStmts(ids) => stmts->Array.filter(stmt => ids->Array.includes(stmt.id))
    }
    stmtsFiltered->Array.map(stmt => userStmtToRootStmt(stmt).expr)
}

let unifyBottomUp = (
    st,
    ~stmtId:stmtId,
    ~bottomUpProverParams:option<bottomUpProverParams>=?,
    ~args0:rootStmtsToUse=AllStmts,
    ~args1:rootStmtsToUse=NoneStmts,
    ~asrtLabel:option<string>=?,
    ~maxSearchDepth:int=4,
    ~lengthRestrict:lengthRestrict=Less,
    ~allowNewDisjForExistingVars:bool=true,
    ~allowNewStmts:bool=true,
    ~allowNewVars:bool=true,
    ~useDisc: option<bool>=?,
    ~useDepr: option<bool>=?,
    ~useTranDepr: option<bool>=?,
    ~combCntMax:int=10000,
    ~chooseLabel:option<string>=?,
    ~chooseResult:option<stmtsDto => bool>=?,
    ()
):(editorState, array<stmtsDto>) => {
    assertNoErrors(st)
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot unifyBottomUp when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let st = st->uncheckAllStmts
            let st = st->toggleStmtChecked(stmtId)
            let rootUserStmts = st->getRootStmtsForUnification
            let rootStmts = rootUserStmts->Array.map(userStmtToRootStmt)
            let proofTree = MM_provers.unifyAll(
                ~parenCnt = st.parenCnt,
                ~frms = st.frms,
                ~wrkCtx,
                ~rootStmts,
                ~bottomUpProverParams = 
                    switch bottomUpProverParams {
                        | Some(params) => params
                        | None => {
                            bottomUpProverParamsMakeDefault(
                                ~asrtLabel?,
                                ~maxSearchDepth,
                                ~lengthRestrict,
                                ~allowNewDisjForExistingVars,
                                ~allowNewStmts,
                                ~allowNewVars,
                                ~args0 = filterRootStmts(rootUserStmts, args0),
                                ~args1 = filterRootStmts(rootUserStmts, args1),
                                ()
                            )
                        }
                    },
                ~allowedFrms={
                    inSyntax: st.settings.allowedFrms.inSyntax,
                    inEssen: {
                        useDisc: useDisc->Belt_Option.getWithDefault(st.settings.allowedFrms.inEssen.useDisc),
                        useDepr: useDepr->Belt_Option.getWithDefault(st.settings.allowedFrms.inEssen.useDepr),
                        useTranDepr: useTranDepr->Belt_Option.getWithDefault(st.settings.allowedFrms.inEssen.useTranDepr),
                    }
                },
                ~combCntMax,
                //~onProgress = msg => Js.Console.log(msg),
                ()
            )
            let proofTreeDto = proofTree->proofTreeToDto(rootStmts->Array.map(stmt=>stmt.expr))
            let rootExprToLabel = st.stmts->Array.map(userStmtToRootStmt)
                ->Array.map(stmt => (stmt.expr,stmt.label))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let result = proofTreeDtoToNewStmtsDto(
                ~treeDto = proofTreeDto, 
                ~exprToProve=(rootStmts->Array.getUnsafe(rootStmts->Js_array2.length-1)).expr,
                ~ctx = wrkCtx,
                ~typeToPrefix = 
                    Belt_MapString.fromArray(
                        st.settings.typeSettings->Array.map(ts => (ts.typ, ts.prefix))
                    ),
                ~rootExprToLabel,
                ~reservedLabels=st.stmts->Array.map(stmt => stmt.label)
            )
            let result = switch chooseLabel {
                | None => result
                | Some(chooseLabel) => {
                    result->Array.filter(newStmtsDto => {
                        let lastStmt = newStmtsDto.stmts->Array.getUnsafe(newStmtsDto.stmts->Js_array2.length - 1)
                        switch lastStmt.jstf {
                            | Some({label}) => label == chooseLabel
                            | _ => raise(MmException({msg:`Cannot get asrt label from newStmtsDto.`}))
                        }
                    })
                }
            }
            let result = switch chooseResult {
                | None => result
                | Some(chooseResult) => result->Array.filter(chooseResult)
            }
            (st, result)
        }
    }
}

let getSingleStmtsDto = (stmtsDtoArr:array<stmtsDto>):stmtsDto => {
    if (stmtsDtoArr->Js_array2.length > 1) {
        raise(MmException({msg:`There are more than 1 element in stmtsDtoArr.`}))
    } else if (stmtsDtoArr->Js_array2.length == 0) {
        raise(MmException({msg:`stmtsDtoArr is empty.`}))
    } else {
        stmtsDtoArr->Array.getUnsafe(0)
    }
}

let removeAllJstf = (st:editorState):editorState => {
    let st = {...st, stmts: st.stmts->Array.map(stmt => {...stmt, jstfText:""})}
    st->verifyEditorState
}

let addDisj = (st:editorState, disj:string):editorState => {
    let disjLines = st.disjText->multilineTextToNonEmptyLines
    disjLines->Array.push(disj)
    let st = st->completeDisjEditMode( disjLines->Array.joinUnsafe("\n") )
    st->verifyEditorState
}

let removeDisj = (st:editorState, disj:string):editorState => {
    let disjLines = st.disjText->multilineTextToNonEmptyLines
    let st = st->completeDisjEditMode(
        disjLines->Array.filter(line => line != disj)->Array.joinUnsafe("\n")
    )
    st->verifyEditorState
}

let setDisj = (st:editorState, disj:string):editorState => {
    let st = st->completeDisjEditMode( disj )
    st->verifyEditorState
}

let setVars = (st:editorState, vars:string):editorState => {
    let st = st->completeVarsEditMode( vars )
    st->verifyEditorState
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
            st->verifyEditorState
        }
    }
}