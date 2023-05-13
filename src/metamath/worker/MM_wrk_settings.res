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
    checkSyntax:bool,
    typeSettings: array<typeSettings>,
    webSrcSettings: array<webSrcSettings>,
}

let markUrlAsTrusted = (settings:settings, url:string):settings => {
    if (settings.webSrcSettings->Js.Array2.find(ws => ws.url == url)->Belt.Option.isSome) {
        {
            ...settings,
            webSrcSettings: settings.webSrcSettings->Js.Array2.map(ws => {
                if (ws.url == url) {
                    {
                        ...ws,
                        trusted:true
                    }
                } else {
                    ws
                }
            })
        }
    } else {
        {
            ...settings,
            webSrcSettings: settings.webSrcSettings->Js.Array2.concat([
                {
                    alias:"",
                    url,
                    trusted:true,
                }
            ])
        }
    }
}

let settingsGetTypeColors = (settings:settings):Belt_HashMapString.t<string> => {
    settings.typeSettings
        ->Js_array2.map(ts => (ts.typ, ts.color))
        ->Belt_HashMapString.fromArray
}