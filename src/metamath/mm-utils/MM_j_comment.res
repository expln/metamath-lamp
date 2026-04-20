type commandArg =
    | Unquoted(string)
    | Quoted(string)

type command = {
    command:string,
    args:array<commandArg>
}

module CommandParser = {
    open Parser
    type parser<'d> = parser<string,'d>
    let enableLog = false
    let log = (parser,name) => if enableLog {Parser.log(parser,name,~tokenSep="")} else {parser}

    let whitespaceChar:parser<string> = oneOf([" ", "\t", "\r", "\n", "\f"])
        ->log("whitespaceChar")

    let endOfCommand:parser<string> = val(";")
        ->log("endOfCommand")

    let notSpaceNotEndChar:parser<string> = not_(any([whitespaceChar, endOfCommand]))
        ->log("notSpaceNotEnd")

    let commandName:parser<string> = rep(notSpaceNotEndChar, ~minCnt=1)->map(Array.join(_, ""))
        ->log("commandName")

    let unquotedArg:parser<commandArg> = 
        rep(notSpaceNotEndChar, ~minCnt=1)->map(arg => Unquoted(Array.join(arg, "")))
        ->log("unquotedArg")

    let singleQuote:parser<string> = val("'")
        ->log("singleQuote")

    let doubleQuote:parser<string> = val(`"`)
        ->log("doubleQuote")

    let quotedArg:parser<commandArg> = 
        any([
            seq3(singleQuote,rep(not_(singleQuote)),singleQuote), 
            seq3(doubleQuote,rep(not_(doubleQuote)),doubleQuote)
        ])
        ->map(((_,argValue,_)) => Quoted(argValue->Array.join("")))
        ->log("quotedArg")

    let argument:parser<commandArg> = any([quotedArg, unquotedArg])
        ->log("argument")

    let whitespace:parser<string> = rep(whitespaceChar, ~minCnt=1)->map(Array.join(_, ""))
        ->log("whitespace")

    let argList:parser<array<commandArg>> = seq2(argument, rep(seq2(whitespace, argument)))->map(((head,tail)) => {
        Array.concat([head], tail->Array.map(((_,arg)) => arg))
    })->log("argList")

    let command:parser<command> = seq4(commandName, opt(seq2(whitespace, argList)), opt(whitespace), endOfCommand)
        ->map(((cmdName, optArgs, _, _)) => {
            {command:cmdName, args:optArgs->Option.map(((_,args))=>args)->Option.getOr([])}
        })
        ->log("command")

    let commands:parser<array<command>> = seq2(command,rep(seq2(opt(whitespace),command)))->map(((head,tail)) => {
        Array.concat([head], tail->Array.map(((_,cmd)) => cmd))
    })->log("commands")
}

let parseJComment = (text:string):array<command> => {
    switch CommandParser.commands(Parser.makeParserInput(text->String.trim->String.split(""))) {
        | None => []
        | Some({data}) => data
    }
}