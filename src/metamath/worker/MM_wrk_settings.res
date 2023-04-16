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
    typeSettings: array<typeSettings>,
    webSrcSettings: array<webSrcSettings>,
}

let markUrlAsTrusted = (settings:settings, url:string):settings => {
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
}