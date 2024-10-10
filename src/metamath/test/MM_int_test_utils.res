open Expln_test
open MM_parser
open MM_wrk_editor
open MM_statements_dto
open MM_progress_tracker
open MM_editor_history
open Common

let setMmPath = "./src/metamath/test/resources/set-no-proofs._mm"
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
    let userStmtTypeToStr = (stmt:userStmt) => {
        switch stmt.typ {
            | E => "e"
            | P => if (stmt.isGoal) {"g"} else {"p"}
        }
    }

    let lines = []
    lines->Array.push("Variables:")
    lines->Array.push(st.varsText)
    lines->Array.push("")
    lines->Array.push("Disjoints:")
    lines->Array.push(st.disjText)
    lines->Array.push("")
    st.stmts->Array.forEach(stmt => {
        lines->Array.push("")
        lines->Array.push(
            "--- "
            ++ (stmt->userStmtTypeToStr)
            ++ " -------------------------------------------------------------------------------"
        )
        lines->Array.push(stmt.label)
        lines->Array.push(stmt.jstfText)
        lines->Array.push(contToStr(stmt.cont))
        lines->Array.push(
            stmt.proofStatus
                ->Belt_Option.map(status => (status->proofStatusToStr))
                ->Belt_Option.getWithDefault("None")
        )
        switch stmt.stmtErr {
            | None => ()
            | Some({msg}) => lines->Array.push("Stmt Error: " ++ msg)
        }
        switch stmt.syntaxErr {
            | None => ()
            | Some(msg) => {
                lines->Array.push(
                    if (msg == "") {
                        "Syntax error."
                    } else {
                        `Syntax error - ${msg}.`
                    }
                )
            }
        }
        switch stmt.unifErr {
            | None => ()
            | Some(msg) => lines->Array.push("Unif Error: " ++ msg)
        }
    })
    lines->Array.joinUnsafe("\n")
}

let newStmtsDtoToStr = (newStmtsDto:stmtsDto):string => {
    let disjStr = newStmtsDto.newDisjStr->Array.joinUnsafe("\n")
    let stmtsStr = newStmtsDto.stmts
        ->Array.map(stmt => {
            [
                stmt.label,
                switch stmt.jstf {
                    | None => ""
                    | Some({args, label}) => "[" ++ args->Array.joinUnsafe(" ") ++ " : " ++ label ++ " ]"
                },
                if (stmt.isProved) {"\u2713"} else {" "},
                stmt.exprStr
            ]->Array.joinUnsafe(" ")
        })->Array.joinUnsafe("\n")
    disjStr ++ "\n" ++ stmtsStr
}

let readTestFileToString = (fileName:string):string => {
    Expln_utils_files.readStringFromFile(curTestDataDir.contents ++ "/" ++ fileName ++ ".txt")
        ->Js.String2.replaceByRe(%re("/\r/g"), "")
}

let assertStrEqFile = (actualStr:string, expectedStrFileName:string) => {
    let fileWithExpectedResult = curTestDataDir.contents ++ "/" ++ expectedStrFileName ++ ".txt"
    let expectedResultStr = try {
        Expln_utils_files.readStringFromFile(fileWithExpectedResult)->Js.String2.replaceByRe(%re("/\r/g"), "")
    } catch {
        | Exn.Error(exn) => {
            if (
                exn->Exn.message
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
    if (st->editorStateHasCriticalErrors) {
        let filePath = curTestDataDir.contents ++ "/" ++ "editor-state-error.txt"
        Expln_utils_files.writeStringToFile( st->editorStateToStr, filePath )
        raise(MmException({msg:`Editor state has errors: ${filePath}`}))
    }
}

let assertEditorHistory = (ht:editorHistory, expectedStrFileName:string) => {
    let actualStr = ht->editorHistToStringExtended
    assertStrEqFile(actualStr, expectedStrFileName)
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

let assertFileContentsEq = (fileName1:string, fileName2:string):unit => {
    let text1 = readTestFileToString(fileName1)
    let text2 = readTestFileToString(fileName2)
    if (text1 != text2) {
        failMsg(`${fileName1} != ${fileName2}`)
    }
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
            Console.log2(Date.make()->Date.toISOString, pct->floatToPctStr)
        }, 
        ()
    )
}

let testProgressTrackerIncCnt = (state:progressStateInt):unit => {
    state->progressTrackerIntIncCnt
}