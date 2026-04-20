type commandArg =
    | Unquoted(string)
    | Quoted(string)

type command = {
    command:string,
    args:array<commandArg>
}

let parseJComment: string=>array<command>