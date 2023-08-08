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

    descrRegexToDisc: string,
    labelRegexToDisc: string,
    descrRegexToDepr: string,
    labelRegexToDepr: string,
    discColor:option<string>,
    deprColor:option<string>,
    tranDeprColor:option<string>,
    
    editStmtsByLeftClick:bool,
    defaultStmtType:string,
    defaultStmtLabel:string,
    initStmtIsGoal:bool,
    checkSyntax:bool,
    stickGoalToBottom:bool,
    autoMergeStmts:bool,
    typeSettings: array<typeSettings>,
    unifMetavarPrefix:string,
    webSrcSettings: array<webSrcSettings>,
    longClickEnabled:bool,
    longClickDelayMs:int,
    hideContextSelector:bool,
    showVisByDefault:bool,
    editorHistMaxLength:int,
}

let markUrlAsTrusted: (settings, string) => settings

let settingsGetTypeColors: settings => Belt_HashMapString.t<string>