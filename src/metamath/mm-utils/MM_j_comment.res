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

    let nonWhitespaceChar:parser<string> = not_(whitespaceChar)
        ->log("nonWhitespaceChar")

    let endOfCommand:parser<string> = val(";")
        ->log("endOfCommand")

    let notSpaceNotEnd:parser<string> = not_(any([whitespaceChar, endOfCommand]))
        ->log("notSpaceNotEnd")

    let commandName:parser<string> = any([rep(nonWhitespaceChar, ~minCnt=2)->map(Array.join(_, "")), notSpaceNotEnd])
        ->log("commandName")

    let unquotedArg:parser<commandArg> = 
        any([rep(nonWhitespaceChar, ~minCnt=2)->map(Array.join(_, "")), notSpaceNotEnd])
        ->map(arg => Unquoted(arg))
        ->log("unquotedArg")

    let quote:parser<string> = val("'")
        ->log("quote")

    let quotedArg:parser<commandArg> = 
        seq3(quote,rep(not_(quote)),quote)->map(((_,argValue,_)) => Quoted(argValue->Array.join("")))
        ->log("quotedArg")

    let argument:parser<commandArg> = any([quotedArg, unquotedArg])
        ->log("argument")

    let whitespace:parser<string> = rep(whitespaceChar, ~minCnt=1)->map(Array.join(_, ""))
        ->log("whitespace")

    let argList:parser<array<commandArg>> = seq2(argument, rep(seq2(whitespace, argument)))->map(((head,tail)) => {
        Array.concat([head], tail->Array.map(((_,arg)) => arg))
    })->log("argList")

    let command:parser<command> = seq4(commandName, whitespace, opt(seq2(argList, whitespace)), endOfCommand)
        ->map(((cmdName, _, optArgs, _)) => {
            {command:cmdName, args:optArgs->Option.map(((args,_))=>args)->Option.getOr([])}
        })
        ->log("command")

    let commands:parser<array<command>> = seq2(command,rep(seq2(whitespace,command)))->map(((head,tail)) => {
        Array.concat([head], tail->Array.map(((_,cmd)) => cmd))
    })->log("commands")
}

let parseJComment = (text:string):array<command> => {
    switch CommandParser.commands(Parser.makeParserInput(text->String.trim->String.split(""))) {
        | None => []
        | Some({data}) => data
    }
}