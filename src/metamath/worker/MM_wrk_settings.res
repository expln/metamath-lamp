type typeSettings = {
    typ: string,
    color: string,
    prefix: string,
}

type settings = {
    parens: string,
    typeSettings: array<typeSettings>,
    asrtsToSkip: array<string>,
    asrtsToSkipRegex: string,
}
