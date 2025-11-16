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
    let logParsers = true
    let log = (parser,name) => if logParsers {Parser.log(parser,name,~tokenSep="")} else {parser}

    let whitespaceChar:parser<string> = oneOf([" ", "\t", "\r", "\n", "\f"])
        ->log("whitespaceChar")

    let nonWhitespaceChar:parser<string> = not_(whitespaceChar)
        ->log("nonWhitespaceChar")

    let commandName:parser<string> = rep(nonWhitespaceChar, ~minCnt=1)->map(Array.join(_, ""))
        ->log("commandName")

    let unquotedArg:parser<commandArg> = rep(nonWhitespaceChar, ~minCnt=1)->map(arg => Unquoted(arg->Array.join("")))
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

    let endOfCommand:parser<string> = val(";")
        ->log("endOfCommand")

    let command:parser<command> = seq5(commandName, whitespace, opt(argList), opt(whitespace), endOfCommand)
        ->map(((cmdName, _, optArgs, _, _)) => {command:cmdName, args:optArgs->Option.getOr([])})
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