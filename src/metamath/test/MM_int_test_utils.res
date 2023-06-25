open Expln_test
open MM_parser
open MM_wrk_editor
open MM_statements_dto
open MM_progress_tracker

let setMmPath = "./src/metamath/test/resources/set-no-proofs._mm"
let asrtsToSkipFilePath = "./src/metamath/test/resources/set-no-proofs-asrts-to-skip.txt"
let failOnMismatch = true
let debug = false

let curTestDataDir = ref("")

let setTestDataDir = dirName => {
    curTestDataDir.contents = "./src/metamath/test/resources/int-test-data/" ++ dirName
}

let getTestDataDir = () => curTestDataDir.contents

let proofStatusToStr = status => {
    switch status {
        | Ready => "ready"
        | Waiting => "waiting"
        | NoJstf => "noJstf"
        | JstfIsIncorrect => "jstfIsIncorrect"
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
        lines->Js_array2.push(
            "--- "
            ++ (stmt.typ->userStmtTypeToStr)
            ++ " -------------------------------------------------------------------------------"
        )->ignore
        lines->Js_array2.push(stmt.label)->ignore
        lines->Js_array2.push(stmt.jstfText)->ignore
        lines->Js_array2.push(contToStr(stmt.cont))->ignore
        lines->Js_array2.push(
            stmt.proofStatus
                ->Belt_Option.map(status => (status->proofStatusToStr))
                ->Belt_Option.getWithDefault("None")
        )->ignore
        switch stmt.stmtErr {
            | None => ()
            | Some(msg) => lines->Js_array2.push("Stmt Error: " ++ msg)->ignore
        }
        switch stmt.syntaxErr {
            | None => ()
            | Some(msg) => {
                lines->Js_array2.push(
                    if (msg == "") {
                        "Syntax error."
                    } else {
                        `Syntax error - ${msg}.`
                    }
                )->ignore
            }
        }
        switch stmt.unifErr {
            | None => ()
            | Some(msg) => lines->Js_array2.push("Unif Error: " ++ msg)->ignore
        }
    })
    lines->Js.Array2.joinWith("\n")
}

let newStmtsDtoToStr = (newStmtsDto:stmtsDto):string => {
    let disjStr = newStmtsDto.newDisjStr->Js.Array2.joinWith("\n")
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

let readEditorStateToString = (fileName:string):string => {
    Expln_utils_files.readStringFromFile(curTestDataDir.contents ++ "/" ++ fileName ++ ".txt")
        ->Js.String2.replaceByRe(%re("/\r/g"), "")
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
        Expln_utils_files.writeStringToFile(actualStr, fileWithActualResult)
        if (failOnMismatch) {
            assertEq( fileWithActualResult, fileWithExpectedResult )
        }
    }
}

let assertNoErrors = (st) => {
    if (st->editorStateHasErrors) {
        let filePath = curTestDataDir.contents ++ "/" ++ "editor-state-error.txt"
        Expln_utils_files.writeStringToFile( st->editorStateToStr, filePath )
        raise(MmException({msg:`Editor state has errors: ${filePath}`}))
    }
}

let assertEditorState = (st, expectedStrFileName:string) => {
    let actualStr = st->editorStateToStr
    assertStrEqFile(actualStr, expectedStrFileName)
}

let assertStmtsDto = (stmtsDto, expectedStrFileName:string) => {
    let actualStr = stmtsDto->newStmtsDtoToStr
    assertStrEqFile(actualStr, expectedStrFileName)
}

let assertProof = (st, stmtId:string, expectedStrFileName:string) => {
    let actualStr = switch st->generateCompressedProof(stmtId) {
        | None => "no proof generated"
        | Some((actualStr, _, _)) => actualStr->Js.String2.replaceByRe(%re("/\r/g"), "")
    }
    assertStrEqFile(actualStr, expectedStrFileName)
}

let assertTextsEq = (text1:string, fileName1:string, text2:string, fileName2:string):unit => {
    if (text1 != text2) {
        Expln_utils_files.writeStringToFile(
            text1,
            curTestDataDir.contents ++ "/" ++ fileName1 ++ ".txt"
        )
        Expln_utils_files.writeStringToFile(
            text2,
            curTestDataDir.contents ++ "/" ++ fileName2 ++ ".txt"
        )
        assertEq( text1, text2 )
    }
}

let assertTextEqFile = (actualStr:string, expectedStrFileName:string):unit => {
    assertStrEqFile(actualStr, expectedStrFileName)
}

let generateReducedMmFile = (
    ~pathToFullMmFile:string,
    ~pathToSaveTo:string,
    ~skipComments:bool=false,
    ~skipProofs:bool=false,
    ()
) => {
    let fullMmFileText = Expln_utils_files.readStringFromFile(pathToFullMmFile)
    let (ast, _) = parseMmFile(~mmFileContent=fullMmFileText, ~skipComments, ~skipProofs, ())
    let reducedContent = astToStr(ast)
    Expln_utils_files.writeStringToFile( reducedContent, pathToSaveTo )
}

let countFrames = (
    ast, 
    ~stopBefore="",
    ~stopAfter="",
    ()
) => {

    let (cnt, _) = traverseAst(
        ref(0),
        ast,
        ~process = (cnt,node) => {
            switch node {
                | {stmt: Axiom({label}) | Provable({label})} => {
                    if (stopBefore == label) {
                        Some(())
                    } else {
                        cnt.contents = cnt.contents + 1
                        if (stopAfter == label) {
                            Some(())
                        } else {
                            None
                        }
                    }
                }
                | _ => None
            }
        },
        ()
    )
    cnt.contents
}

let testProgressTrackerMake = (
    ~step:float, 
    ~maxCnt:int,
):progressStateInt => {
    progressTrackerIntMake(
        ~step, 
        ~maxCnt,
        ~onProgress = pct => {
            Js.Console.log2(Js.Date.make()->Js.Date.toISOString, (pct *. 100.)->Js_math.round->Belt_Float.toString ++ "%")
        }, 
        ()
    )
}

let testProgressTrackerIncCnt = (state:progressStateInt):unit => {
    state->progressTrackerIntIncCnt
}