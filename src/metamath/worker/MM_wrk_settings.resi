type typeSettings = {
    typ: string,
    color: string,
    prefix: string,
}

type webSrcSettings = {
    alias: string,
    url: string,
    trusted: bool,
}

type settings = {
    parens: string,
    asrtsToSkip: array<string>,
    asrtsToSkipRegex: string,
    editStmtsByLeftClick:bool,
    defaultStmtType:string,
    typeSettings: array<typeSettings>,
    webSrcSettings: array<webSrcSettings>,
}

let markUrlAsTrusted: (settings, string) => settings