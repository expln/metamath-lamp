open Expln_test
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

let setMmPath = "/books/metamath/set.mm"
let failOnMismatch = true
let debug = false

let parenCnt = ref(parenCntMake([], ()))

let createEditorState = (~mmFilePath:string, ~stopBefore:option<string>=?, ~stopAfter:option<string>=?, ()) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore?, ~stopAfter?, ~debug, ())
    while (ctx->getNestingLevel != 0) {
        ctx->closeChildContext
    }
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let frms = prepareFrmSubsData(ctx)
    parenCnt.contents = parenCntMake(MM_wrk_ctx.prepareParenInts(ctx, parens), ())
    let st = {
        settingsV: 1,
        settings: {
            parens,
            typeSettings: [ ],
        },
        typeColors: Belt_HashMapString.make(~hintSize=0),

        preCtxV: 1,
        preCtx: ctx,
        parenCnt: parenCnt.contents,
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

let addStmt = (st, ~typ:option<userStmtType>=?, ~label:option<string>=?, ~stmt:string, ()):(editorState,string) => {
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
    (st->updateEditorStateWithPostupdateActions(st => st), stmtId)
}

let duplicateStmt = (st, stmtId):(editorState,string) => {
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
    let st = st->updateStmt(stmtId, stmt => {
        let stmt = switch label {
            | None => stmt
            | Some(label) => {...stmt, label}
        }
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
    ~addBefore:option<string>=?,
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
            switch searchResults->Js_array2.find(res => res.stmts[res.stmts->Js_array2.length-1].label == chooseLabel) {
                | None => 
                    raise(MmException({
                        msg:`addStmtsBySearch: could not find ${chooseLabel}. ` 
                            ++ `Available: ${searchResults->Js_array2.map(res => res.stmts[res.stmts->Js_array2.length-1].label)->Js_array2.joinWith(", ")} `
                    }))
                | Some(searchResult) => st->addNewStatements(searchResult)
            }
        }
    }
    st->updateEditorStateWithPostupdateActions(st => st)
}

let applySubstitution = (st, ~replaceWhat:string, ~replaceWith:string):editorState => {
    let st = switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot applySubstitution when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let wrkSubs = findPossibleSubs(
                st, 
                wrkCtx->ctxStrToIntsExn(replaceWhat),
                wrkCtx->ctxStrToIntsExn(replaceWith)
            )
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
                ->Js_array2.filter(stmt => stmt.typ == #p)
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
                ~parenCnt = parenCnt.contents,
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

let unifyBottomUp = (st,stmtId, 
    ~asrtLabel:option<string>=?,
    ~maxSearchDepth:int=4, 
    ~lengthRestriction:lengthRestrict=Less,
    ~allowNewVars:bool=true,
    ~useRootStmtsAsArgs:bool=true,
    ~chooseLabel:string,
    ()
):(editorState, array<newStmtsDto>) => {
    switch st.wrkCtx {
        | None => raise(MmException({msg:`Cannot unifyBottomUp when wrkCtx is None.`}))
        | Some(wrkCtx) => {
            let st = st->uncheckAllStmts
            let st = st->toggleStmtChecked(stmtId)
            let stmts = st->getStmtsForUnification
            let proofTree = MM_provers.unifyAll(
                ~parenCnt = parenCnt.contents,
                ~frms = st.frms,
                ~ctx = wrkCtx,
                ~stmts,
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
            let proofTreeDto = proofTree->proofTreeToDto(stmts->Js_array2.map(stmt=>stmt.expr))
            let newStmts = proofTreeDtoToNewStmtsDto(
                ~treeDto = proofTreeDto, 
                ~rootStmts = stmts,
                ~ctx = wrkCtx,
                ~typeToPrefix = 
                    Belt_MapString.fromArray(
                        st.settings.typeSettings->Js_array2.map(ts => (ts.typ, ts.prefix))
                    )
            )
            let result = newStmts
                ->Js_array2.map(newStmtsDto => {
                    let lastStmt = newStmtsDto.stmts[newStmtsDto.stmts->Js_array2.length - 1]
                    switch lastStmt.jstf {
                        | Some({label}) => (label, newStmtsDto)
                        | _ => raise(MmException({msg:`Cannot get asrt label from newStmtsDto.`}))
                    }
                })
                ->Js_array2.filter(((label, _)) => label == chooseLabel)
                ->Js.Array2.sortInPlaceWith(((la, _),(lb, _)) => la->Js_string2.localeCompare(lb)->Belt.Float.toInt)
                ->Js_array2.map(((_, newStmtsDto)) => newStmtsDto)
            (st, result)
        }
    }
}

let newStmtsDtoToStr = (newStmtsDto:newStmtsDto):string => {
    let disjStr = newStmtsDto.newDisjStr->Js.Array2.sortInPlace->Js.Array2.joinWith("\n")
    let stmtsStr = newStmtsDto.stmts
        ->Js.Array2.map(stmt => {
            [
                stmt.label,
                switch stmt.jstf {
                    | None => ""
                    | Some({args, label}) => "[" ++ args->Js_array2.joinWith(" ") ++ " : " ++ label ++ " ]"
                },
                if (stmt.isProved) {"\u2713"} else {" "},
                stmt.exprStr
            ]->Js.Array2.joinWith(" ")
        })->Js.Array2.joinWith("\n")
    disjStr ++ "\n" ++ stmtsStr
}

let arrNewStmtsDtoToStr = (arr:array<newStmtsDto>):string => {
    arr
        ->Js_array2.map(newStmtsDtoToStr)
        ->Js_array2.joinWith("\n------------------------------------------------------------------------------------\n")
}

let editorStateToStr = st => {
    let lines = []
    lines->Js_array2.push("Variables:")->ignore
    lines->Js_array2.push(st.varsText)->ignore
    lines->Js_array2.push("")->ignore
    lines->Js_array2.push("Disjoints:")->ignore
    lines->Js_array2.push(st.disjText)->ignore
    lines->Js_array2.push("")->ignore
    st.stmts->Js.Array2.forEach(stmt => {
        lines->Js_array2.push("")->ignore
        lines->Js_array2.push(
            "--- "
            ++ (stmt.typ :> string) 
            ++ " -------------------------------------------------------------------------------"
        )->ignore
        lines->Js_array2.push(stmt.label)->ignore
        lines->Js_array2.push(stmt.jstfText)->ignore
        lines->Js_array2.push(contToStr(stmt.cont))->ignore
        lines->Js_array2.push(
            stmt.proofStatus
                ->Belt_Option.map(status => (status :> string))
                ->Belt_Option.getWithDefault("None")
        )->ignore
    })
    lines->Js.Array2.joinWith("\n")
}

let curTestDataDir = ref("")

let setTestDataDir = dirName => {
    curTestDataDir.contents = "./src/metamath/test/resources/int-test-data/" ++ dirName
}

let assertStrEqFile = (actualStr:string, expectedStrFileName:string) => {
    let fileWithExpectedResult = curTestDataDir.contents ++ "/" ++ expectedStrFileName ++ ".txt"
    let expectedResultStr = try {
        Expln_utils_files.readStringFromFile(fileWithExpectedResult)->Js.String2.replaceByRe(%re("/\r/g"), "")
    } catch {
        | Js.Exn.Error(exn) => {
            if (
                exn->Js.Exn.message
                    ->Belt_Option.getWithDefault("")
                    ->Js_string2.includes("no such file or directory")
            ) {
                ""
            } else {
                raise(MmException({msg:`Could not read from ${fileWithExpectedResult}`}))
            }
        }
    }
    if (actualStr != expectedResultStr) {
        let fileWithActualResult = fileWithExpectedResult ++ ".actual"
        Expln_utils_files.writeStringToFile(fileWithActualResult, actualStr)
        if (failOnMismatch) {
            assertEq( fileWithActualResult, fileWithExpectedResult )
        }
    }
}

let assertEditorState = (st, expectedStrFileName:string) => {
    let actualStr = st->editorStateToStr
    assertStrEqFile(actualStr, expectedStrFileName)
}

let assertProof = (st, stmtId:string, expectedStrFileName:string) => {
    let actualStr = st->generateCompressedProof(stmtId)
        ->Belt.Option.getWithDefault("no proof generated")
        ->Js.String2.replaceByRe(%re("/\r/g"), "")
    assertStrEqFile(actualStr, expectedStrFileName)
}

let assertTextsEq = (text1:string, fileName1:string, text2:string, fileName2:string):unit => {
    if (text1 != text2) {
        Expln_utils_files.writeStringToFile(
            curTestDataDir.contents ++ "/" ++ fileName1 ++ ".txt", 
            text1
        )
        Expln_utils_files.writeStringToFile(
            curTestDataDir.contents ++ "/" ++ fileName2 ++ ".txt", 
            text2
        )
        assertEq( text1, text2 )
    }
}

let assertTextEqFile = (actualStr:string, expectedStrFileName:string):unit => {
    assertStrEqFile(actualStr, expectedStrFileName)
}

let getStmtId = (st:editorState, ~contains:string) => {
    let found = st.stmts->Js_array2.filter(stmt => stmt.cont->contToStr->Js.String2.includes(contains))
    if (found->Js_array2.length != 1) {
        raise(MmException({msg:`getStmtId:  found.length = ${found->Js_array2.length->Belt_Int.toString}`}))
    } else {
        found[0].id
    }
}
