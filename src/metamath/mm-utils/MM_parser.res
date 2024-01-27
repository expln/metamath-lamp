open MM_progress_tracker

type parserProofTableRec = {
    id:string,
    args:array<string>,
    label:string
}

type proof =
    | Uncompressed({labels:array<string>})
    | Compressed({labels:array<string>, compressedProofBlock:string})

type rec mmAstNode = {
    begin: int,
    end: int,
    stmt: stmt
}
and stmt =
    | Comment({text:string})
    | Const({symbols:array<string>})
    | Block({level:int, statements:array<mmAstNode>})
    | Var({symbols:array<string>})
    | Disj({vars:array<string>})
    | Floating({label:string, expr:array<string>})
    | Essential({label:string, expr:array<string>})
    | Axiom({label:string, expr:array<string>})
    | Provable({label:string, expr:array<string>, proof:option<proof>})

type mmException = {
    msg:string,
    begin?:int,
}
exception MmException(mmException)

let isWhitespace = str => str == " " || str == "\t" || str == "\n" || str == "\r"

let textAt = (text,i) => {
    let textLength = text->Js_string2.length
    let lengthToShow = 20
    let ellipsis = if (i+lengthToShow < textLength) {"..."} else {""}
    "'" ++ text->Js.String2.substrAtMost(~from=i, ~length=lengthToShow) ++ ellipsis ++ "'"
}

let parseMmFile = (
    ~mmFileContent as text:string, 
    ~skipComments:bool=false,
    ~skipProofs:bool=false,
    ~onProgress: float=>unit = _ => (), 
    ()
): (mmAstNode,array<string>) => {
    let textLength = text->Js_string2.length
    let textLengthFlt = textLength->Belt_Int.toFloat
    let idx = ref(0) // index of the next char to read.
    let endOfFile = ref(false) // if idx is outside of text then endOfFile is true.
    let ch = ref("") // the char idx is pointing to. If endOfFile then ch == "".
    let progressTracker = progressTrackerMake(~step=0.1, ~dontDecrease=true, ~onProgress, ())
    let allLabels = []

    let setIdx = i => {
        idx.contents = i
        if (idx.contents >= textLength) {
            endOfFile.contents = true
            ch.contents = ""
        } else {
            endOfFile.contents = false
            ch.contents = text->Js_string2.charAt(idx.contents)
        }
    }
    setIdx(0)

    let readNextChar = ():unit => {
        if (!endOfFile.contents) {
            idx.contents = idx.contents+1
            if (idx.contents >= textLength) {
                endOfFile.contents = true
                ch.contents = ""
            } else {
                ch.contents = text->Js_string2.charAt(idx.contents)
            }
        }
    }

    let skipWhitespaces = ():unit => {// makes sure the next char to process is not a whitespace or the text is read to the end.
        while (!endOfFile.contents && ch.contents->isWhitespace) {
            readNextChar()
        }
    }

    let readAllTextTill = (tillToken:string):option<string> => {
        let result = ref(None)
        let beginIdx = idx.contents
        while (result.contents->Belt_Option.isNone) {
            let foundIdx = text->Js_string2.indexOfFrom(tillToken, idx.contents)
            if (foundIdx < 0) {
                result.contents = Some(None)
            } else {
                let nextIdx = foundIdx + tillToken->Js_string2.length
                setIdx(nextIdx)
                if (endOfFile.contents || ch.contents->isWhitespace) {
                    result.contents = Some(Some(text->Js_string2.substring(~from=beginIdx, ~to_=foundIdx)))
                } else {
                    setIdx(foundIdx+1)
                }
            }
        }
        result.contents->Belt_Option.getExn
    }

    let textAt = textAt(text, _)

    let parseComment = (~beginIdx:int):mmAstNode => {
        switch readAllTextTill("$)") {
            | None => raise(MmException({msg:`A comment is not closed at ${textAt(beginIdx)}`}))
            | Some(commentText) => {begin:beginIdx, end:idx.contents-1, stmt:Comment({text:commentText})}
        }
    }

    let rec readNextToken = (~skipComments=true, ()):string => {
        if (!skipComments) {
            skipWhitespaces()
            let beginIdx = idx.contents
            while (!endOfFile.contents && !(ch.contents->isWhitespace)) {
                readNextChar()
            }
            text->Js_string2.substring(~from=beginIdx, ~to_=idx.contents)
        } else {
            let nextToken = ref(readNextToken(~skipComments=false, ()))
            while (nextToken.contents == "$(") {
                parseComment(~beginIdx=idx.contents)->ignore
                nextToken.contents = readNextToken(~skipComments=false, ())
            }
            nextToken.contents
        }
    }

    let readAllTokensTill = (tillToken:string):option<array<string>> => {
        let result = ref(None)
        let tokens = []
        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken(())
            if (token == "") {
                result.contents = Some(None)
            } else if (token == "$(") {
                //skipping comments inside of statements
                parseComment(~beginIdx=idx.contents)->ignore
            } else if (token == tillToken) {
                result.contents = Some(Some(tokens))
            } else {
                tokens->Js_array2.push(token)->ignore
            }
        }
        result.contents->Belt_Option.getExn
    }

    let parseConst = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A constant statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Const({symbols:tokens})}
        }
    }

    let parseVar = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A variable statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Var({symbols:tokens})}
        }
    }

    let parseDisj = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A disjoint statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Disj({vars:tokens})}
        }
    }

    let parseFloating = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A floating statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Floating({label, expr:tokens})}
        }
    }

    let parseEssential = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`An essential statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Essential({label, expr:tokens})}
        }
    }

    let parseAxiom = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`An axiom statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Axiom({label, expr:tokens})}
        }
    }

    let parseProvable = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$=") {
            | None => raise(MmException({msg:`A provable statement is not closed[1] at ${textAt(beginIdx)}`}))
            | Some(expression) => {
                let firstProofToken = readNextToken(())
                if (firstProofToken == "(") {
                    switch readAllTokensTill(")") {
                        | None => raise(MmException({msg:`A provable statement is not closed[2] at ${textAt(beginIdx)}`}))
                        | Some(proofLabels) => {
                            switch readAllTokensTill("$.") {
                                | None => raise(MmException({msg:`A probale statement is not closed[3] at ${textAt(beginIdx)}`}))
                                | Some(compressedProofBlocks) => {
                                    {
                                        begin:beginIdx, 
                                        end:idx.contents-1, 
                                        stmt:Provable({
                                            label, 
                                            expr:expression,
                                            proof:
                                                if (skipProofs) {
                                                    None
                                                } else {
                                                    Some(Compressed({
                                                        labels:proofLabels, 
                                                        compressedProofBlock:
                                                            ""->Js_string2.concatMany(compressedProofBlocks)
                                                    }))
                                                }
                                        })
                                    }
                                }
                            }
                        }
                    }
                } else {
                    switch readAllTokensTill("$.") {
                        | None => raise(MmException({msg:`A provable statement is not closed[4] at ${textAt(beginIdx)}`}))
                        | Some(proofLabels) => {
                            {
                                begin:beginIdx, 
                                end:idx.contents-1, 
                                stmt:Provable({
                                    label, 
                                    expr:expression, 
                                    proof:
                                        if (skipProofs) {
                                            None
                                        } else {
                                            Some(Uncompressed({
                                                labels:[firstProofToken]->Js.Array2.concat(proofLabels)
                                            }))
                                        },
                                })
                            }
                        }
                    }
                }
            }
        }
    }

    let rec parseBlock = (~beginIdx:int, ~level:int):mmAstNode => {// parses text until $} token or until the end of text
        let result = ref(None)
        let statements = []

        let pushStmt = stmt => {
            statements->Js_array2.push(stmt)->ignore
        }

        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken(~skipComments=false, ())
            let tokenIdx = idx.contents - token->Js_string2.length
            if (token == "") {
                if (level == 0) {
                    result.contents = Some({begin:beginIdx, end:idx.contents-1, stmt:Block({level, statements:statements})})
                } else {
                    raise(MmException({msg:`Unexpected end of a block. The block begins at ${textAt(beginIdx)} and is not closed.`}))
                }
            } else if (token == "$}") {
                result.contents = Some({begin:beginIdx, end:idx.contents-1, stmt:Block({level, statements:statements})})
            } else if (token == "${") {
                pushStmt(parseBlock(~beginIdx=tokenIdx, ~level=level+1))
            } else if (token == "$(") {
                let comment = parseComment(~beginIdx=tokenIdx)
                if (!skipComments) {
                    pushStmt(comment)
                }
            } else if (token == "$[") {
                //skipping include statements
                readAllTextTill("$]")->ignore
            } else if (token == "$c") {
                pushStmt(parseConst(~beginIdx=tokenIdx))
            } else if (token == "$v") {
                pushStmt(parseVar(~beginIdx=tokenIdx))
            } else if (token == "$d") {
                pushStmt(parseDisj(~beginIdx=tokenIdx))
            } else {
                let label = token
                let token2 = readNextToken(())
                let token2Idx = idx.contents - token2->Js_string2.length
                if (token2 == "") {
                    raise(MmException({msg:`Unexpected end of file at ${textAt(tokenIdx)}`}))
                } else if (token2 == "$f") {
                    pushStmt(parseFloating(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$e") {
                    pushStmt(parseEssential(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$a") {
                    allLabels->Js_array2.push(label)->ignore
                    pushStmt(parseAxiom(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$p") {
                    allLabels->Js_array2.push(label)->ignore
                    pushStmt(parseProvable(~beginIdx=tokenIdx, ~label))
                } else {
                    raise(MmException({msg:`Unexpected token '${token2}' at ${textAt(token2Idx)}`}))
                }
            }

            progressTracker->progressTrackerSetCurrPct(tokenIdx->Belt_Int.toFloat /. textLengthFlt)
        }
        result.contents->Belt_Option.getExn
    }

    let rootAst = parseBlock(~beginIdx=idx.contents, ~level=0)
    (rootAst, allLabels)
}

let traverseAst: (
    'c,
    mmAstNode,
    ~preProcess:('c, mmAstNode)=>option<'res>=?,
    ~process:('c, mmAstNode)=>option<'res>=?,
    ~postProcess:('c, mmAstNode)=>option<'res>=?,
    ()
) => ('c, option<'res>) =
    (context, root, ~preProcess=?, ~process=?, ~postProcess=?, ()) => Expln_utils_data.traverseTree(
        context, 
        root, 
        (_, node) => {
            switch node {
                | {stmt:Block({statements})} => Some(statements)
                | _ => None
            }
        },
        ~preProcess=?preProcess,
        ~process=?process,
        ~postProcess=?postProcess,
        ()
    )

let proofToStr = proof => {
    switch proof {
        | Some(Uncompressed({labels})) => labels->Js_array2.joinWith(" ")
        | Some(Compressed({labels, compressedProofBlock})) =>
            "( " ++ labels->Js_array2.joinWith(" ") ++ " ) " ++ compressedProofBlock
        | None => "?"
    }
}

let stmtToStr: mmAstNode => string = node => {
    switch node {
        | {stmt:Block({level})} => `block(level=${level->Belt_Int.toString})`
        | {stmt:Comment({text})} => "$( " ++ text ++ " $)"
        | {stmt:Const({symbols})} =>  "$c " ++ symbols->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Var({symbols})} =>  "$v " ++ symbols->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Disj({vars})} =>  "$d " ++ vars->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Floating({label, expr})} =>  label ++ " $f " ++ expr->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Essential({label, expr})} =>  label ++ " $e " ++ expr->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Axiom({label, expr})} =>  label ++ " $a " ++ expr->Js_array2.joinWith(" ") ++ " $."
        | {stmt:Provable({label, expr, proof})} =>  label ++ " $p " ++ expr->Js_array2.joinWith(" ")
            ++ " $= " ++ proofToStr(proof) ++ " $."
    }
}

let stmtToStrRec: mmAstNode => array<string> = stmt => {
    let makePrefix = level => "    "->Js.String2.repeat(level)
    let ((_,result),_) = traverseAst(
        (ref(0),[]),
        stmt,
        ~preProcess=((level,arr),node)=>{
            switch node {
                | {stmt:Block({level: newLevel})} => {
                    if (newLevel != 0) {
                        arr->Js_array2.push(makePrefix(level.contents) ++ "${")->ignore
                    }
                    level.contents = newLevel
                }
                | _ => ()
            }
            None
        },
        ~process=((level,arr),node)=>{
            let str = switch node {
                | {stmt:Block(_)} => ""
                | _ =>  stmtToStr(node)
            }
            if (str != "") {
                arr->Js_array2.push(makePrefix(level.contents) ++ str)->ignore
            }
            None
        },
        ~postProcess=((level,arr),node)=>{
            switch node {
                | {stmt:Block({level: newLevel})} => {
                    level.contents = newLevel-1
                    if (newLevel != 0) {
                        arr->Js_array2.push(makePrefix(level.contents) ++ "$}")->ignore
                    }
                }
                | _ => ()
            }
            None
        },
        ()
    )
    result
}

let astToStr = ( ast:mmAstNode ):string => {
    let res = []
    let save = str => res->Js_array2.push(str)->ignore
    traverseAst(
        (),
        ast,
        ~preProcess = (_,node) => {
            switch node {
                | {stmt:Block({level})} if level > 0 => save("${")
                | _ => ()
            }
            None
        },
        ~process = (_,node) => {
            switch node {
                | {stmt:Comment({text})} => save("$( " ++ text ++ " $)")
                | {stmt:Const({symbols})} =>  save( "$c " ++ symbols->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Var({symbols})} =>  save( "$v " ++ symbols->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Disj({vars})} =>  save( "$d " ++ vars->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Floating({label, expr})} =>  save( label ++ " $f " ++ expr->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Essential({label, expr})} =>  save( label ++ " $e " ++ expr->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Axiom({label, expr})} =>  save( label ++ " $a " ++ expr->Js_array2.joinWith(" ") ++ " $." )
                | {stmt:Provable({label, expr, proof})} => save(
                    label ++ " $p " ++ expr->Js_array2.joinWith(" ") ++ " $= " ++ proofToStr(proof) ++ " $."
                )
                | _ => ()
            }
            None
        },
        ~postProcess = (_,node) => {
            switch node {
                | {stmt:Block({level})} if level > 0 => save("$}")
                | _ => ()
            }
            None
        },
        ()
    )->ignore
    res->Js_array2.joinWith("\n")
}