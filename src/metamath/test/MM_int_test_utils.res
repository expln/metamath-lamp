open Expln_test
open MM_parser
open MM_context
open MM_proof_tree
open MM_provers
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_search_asrt
open MM_substitution
open MM_parenCounter

let parenCnt = ref(parenCntMake([]))

let createEditorState = (~mmFilePath:string, ~stopBefore:option<string>=?, ~stopAfter:option<string>=?, ()) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore?, ~stopAfter?, ())
    let frms = prepareFrmSubsData(ctx)
    parenCnt.contents = parenCntMake(ctx->ctxStrToIntsExn("( ) { } [ ]"))
    {
        settingsV: 1,
        settings: {
            parens: "( ) [ ] { }",
            parensIsValid: true,
            types: [],
            colors: [],
        },

        preCtxV: 1,
        preCtx: ctx,
        frms,


        varsText: "",
        varsEditMode: false,
        varsErr: None,

        disjText: "",
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],
    }
}

let addStmt = (st, ~typ:option<userStmtType>=?, ~label:option<string>=?, ~stmt:string, ()):(editorState,string) => {
    let (st,stmtId) = st->addNewStmt
    let st = st->completeContEditMode(stmtId, strToCont(stmt))
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
                ~parenCnt=parenCnt.contents,
                ~label=filterLabel->Belt_Option.getWithDefault(""),
                ~typ=st.preCtx->ctxSymToIntExn(filterTyp->Belt_Option.getWithDefault("|-")),
                ~pattern=st.preCtx->ctxStrToIntsExn(filterPattern->Belt_Option.getWithDefault("")),
                ()
            )
            switch searchResults->Js_array2.find(res => res.asrtLabel == chooseLabel) {
                | None => raise(MmException({msg:`Could not find ${chooseLabel}`}))
                | Some(searchResult) => st->addAsrtSearchResult(searchResult)
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

let unifyAll = st => {
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
                ~debug=true,
                ()
            )
            let proofTreeDto = proofTree->ptToDto(stmts->Js_array2.map(stmt=>stmt.expr))
            applyUnifyAllResults(st, proofTreeDto)
        }
    }
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
    curTestDataDir.contents = "./src/metamath/test/resources/int-tests/" ++ dirName
}

let assertEditorState = (st, expectedStateFileName:string) => {
    let actualResultStr = st->editorStateToStr
    let fileWithExpectedResult = curTestDataDir.contents ++ "/" ++ expectedStateFileName ++ ".txt"
    let expectedResultStr = try {
        Expln_utils_files.readStringFromFile(fileWithExpectedResult)
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
    if (actualResultStr != expectedResultStr) {
        let fileWithActualResult = fileWithExpectedResult ++ ".actual"
        Expln_utils_files.writeStringToFile(fileWithActualResult, actualResultStr)
        assertEq( fileWithActualResult, fileWithExpectedResult )
    }
}
