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

type frameRestrict = {
    useDisc:bool,
    useDepr:bool,
    useTranDepr:bool,
}

type allowedFrms = {
    inSyntax: frameRestrict,
    inEssen: frameRestrict,
}

type bottomUpProverDefaults = {
    searchDepth: int,
    lengthRestrict: string,
    allowNewDisjForExistingVars: bool,
    allowNewStmts: bool,
    allowNewVars: bool,
    debugLevel:int,
}

type settings = {
    parens: string,
    asrtsToSkip: array<string>, //deprecated

    descrRegexToDisc: string,
    labelRegexToDisc: string,
    descrRegexToDepr: string,
    labelRegexToDepr: string,
    discColor:string,
    deprColor:string,
    tranDeprColor:string,
    allowedFrms:allowedFrms,

    editStmtsByLeftClick:bool,
    defaultStmtType:string,
    defaultStmtLabel:string,
    initStmtIsGoal:bool,
    checkSyntax:bool,
    stickGoalToBottom:bool,
    autoMergeStmts:bool,
    autoUnifyAll:bool,
    typeSettings: array<typeSettings>,
    unifMetavarPrefix:string,
    sortDisjByType:string,
    webSrcSettings: array<webSrcSettings>,
    longClickEnabled:bool,
    longClickDelayMs:int,
    hideContextSelector:bool,
    showVisByDefault:bool,
    editorHistMaxLength:int,
    combCntMax:int,

    useDefaultTransforms:bool,
    useCustomTransforms:bool,
    customTransforms:string,

    bottomUpProverDefaults: bottomUpProverDefaults,
}

let markUrlAsTrusted: (settings, string) => settings

let settingsGetTypeColors: settings => Belt_HashMapString.t<string>