open Expln_test
open MM_parser
open MM_context
open MM_proof_tree
open MM_provers
open MM_wrk_editor
open MM_wrk_settings
open MM_substitution

let createEditorState = (~mmFilePath:string, ~stopBefore:option<string>=?, ~stopAfter:option<string>=?, ()) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore?, ~stopAfter?, ())
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
        frms: prepareFrmSubsData(ctx),


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
    (st, stmtId)
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

let setMmPath = "C:/Users/Igor/igye/books/metamath/set.mm"

describe("MM_wrk_editor integration tests", _ => {
    it("proving reccot", _ => {
        setTestDataDir("prove-reccot")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="reccsc", ())

        let (st, trgtStmt) = st->addStmt(
            ~label="reccot", 
            ~stmt="|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( 1 / ( cot ` A ) ) )",
            ()
        )
        assertEditorState(st, "step1")
    })
    
})
