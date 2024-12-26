function code(code) {
    return String.fromCharCode(code)
}

function exn(msg) {
    throw new Error(msg)
}

function getResponse(apiResponse) {
    if (apiResponse.isOk) {
        return apiResponse.res
    } else {
        exn(apiResponse.err)
    }
}

await api.setLogApiCallsToConsole(true)

async function showInfoMsg(msg) {
    getResponse(await api.showInfoMsg({msg: String(msg)}))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg({msg: String(msg)}))
}

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

async function unifyAll() {
    getResponse(await api.editor().unifyAll())
    return await getEditorState()
}

function undefToNull(value) {
    return value === undefined ? null : value
}

async function substitute({what, with_}) {
    return getResponse(await api.editor().substitute({what, with_}))
}

async function mergeDuplicatedSteps() {
    return getResponse(await api.editor().mergeDuplicatedSteps())
}

function isObj(x) {
    return x !== undefined && x !== null && typeof x === 'object' && !Array.isArray(x)
}

function isArr(x) {
    return x !== undefined && x !== null && Array.isArray(x)
}

function isStr(x) {
    return x !== undefined && x !== null && typeof x === 'string'
}

function arDiff(a, ...bs) {
    for (const b of bs) {
        const bSet = new Set(b)
        a = a.filter(e => !bSet.has(e))
    }
    return a
}

function arIntersect(a, ...bs) {
    for (const b of bs) {
        const bSet = new Set(b)
        a = a.filter(e => bSet.has(e))
    }
    return a
}

function arUnion(a, ...bs) {
    return [...new Set(a.concat(...bs))]
}

function getStepIdx(editorState, label) {
    const steps = editorState.steps
    for (let i = 0; i < steps.length; i++) {
        if (steps[i].label === label) {
            return i
        }
    }
    exn(`Cannot find the step with the label '${label}'`)
}

function getStepByLabel(editorState, label) {
    return editorState.steps[getStepIdx(editorState, label)]
}

function parseMmp(mmpText) {
    const lines = mmpText.split('\n')
    let firstComment = undefined // option<string>
    const disj = [] // array<array<string>>
    const stmts = [] // array<step>; step = {label:string, type:string(h|p), jstf:string(1 2 3 : asrt), stmt:string}
    let curPart/**/ = undefined
    let partLines = []

    function parseStep(text)/*step | undefined*/ {
        const parts = text.match(/^([^:]+):([^:\s]*):(\S*)(\s.*)?/)
        if (parts == null) {
            return undefined
        } else {
            const label = parts[1]
            const isHyp = label[0] === 'h'
            let jstf = isHyp ?'' : parts[2].split(',').join(' ') + ' : ' + parts[3]
            jstf = jstf.trim()
            jstf = jstf === ':' ? '' : jstf
            stmts.push({
                label,
                type:isHyp?'h':'p',
                jstf,
                stmt: parts.length > 4 ? parts[4].trim() : ''
            })
        }
    }

    function getPartType(line)/*h|c|d|s|f|p|undefined*/ {
        if (line.slice(0,2) === '$(') {
            return 'h'/*header*/
        } else if (line.slice(0,1) === '*') {
            return 'c'/*comment*/
        } else if (line.slice(0,2) === '$d') {
            return 'd'/*disjoints*/
        } else if (parseStep(line) !== undefined) {
            return 's'/*step - hypothesis or derivation*/
        } else if (line.slice(0,2) === '$)') {
            return 'f'/*footer*/
        } else if (line.slice(0,2) === '$=') {
            return 'p'/*generated proof*/
        } else {
            return undefined
        }
    }

    function closePrevPartAndPrepareForNewPart(newPart) {
        //close the previous part
        if (curPart === 'h') {
            //do nothing
        } else if (curPart === 'c') {
            if (firstComment === undefined && disj.length === 0 && stmts.length === 0) {
                firstComment = partLines.join('\n').slice(1) //slice(1) removes the leading *
            }
        } else if (curPart === 'd') {
            //slice(3) removes the leading '$d '
            disj.push(partLines.join(' ').slice(3).split(' ').map(v => v.trim()).filter(v => v !== ''))
        } else if (curPart === 's') {
            stmts.push(parseStep(partLines.join(' ')))
        } else if (curPart === 'f') {
            //do nothing
        } else if (curPart === 'p') {
            //do nothing
        }
        //prepare for the new part
        partLines = []
        curPart = newPart
    }

    for (let i = 0; i < lines.length; i++) {
        let line = lines[i]
        let newPart = getPartType(line)
        if (newPart !== undefined && curPart !== newPart) {
            closePrevPartAndPrepareForNewPart(newPart)
        }
        partLines.push(line)
    }
    return {descr:firstComment, disj, stmts,}
}

async function importFromMmp() {
    const mmpStrResp = getResponse(await api.multilineTextInput({prompt:'MMP file content:'}))
    if (!mmpStrResp.okClicked) {
        return
    }
    const parsed = parseMmp(mmpStrResp.text)
    console.log('parsed', parsed)
}

function makeMacro(name, func) {
    return {
        name,
        run: async () => {
            try {
                await func()
            } catch (ex) {
                await showErrMsg(`${ex.message}${code(10)}${ex.stack}`)
                throw ex
            }
        }
    }
}

await api.macro.registerMacroModule({
    moduleName: 'MMJ2',
    macros: [
        makeMacro('Import from MMP file', importFromMmp)
    ]
})

await api.macro.runMacro({moduleName:'MMJ2', macroName:'Import from MMP file'})

// const parsed = parseMmp("$( <MM> <PROOF_ASST> THEOREM=syllogism LOC_AFTER=\n" +
//     "\n" +
//     "hd1::syllogism.1 |- ( ph -> ps ) \n" +
//     "hd2::syllogism.2 |- ( ps -> ch ) \n" +
//     "\n" +
//     "* !              |- ( ph -> ( ps -> ch ) ) \n" +
//     "* !              |- ( ( ph -> ps ) -> ( ph -> ch ) ) \n" +
//     "!d3::              |- &W1\n" +
//     "!d5::              |- &W2\n" +
//     "!d6::ax-2              |- ( &W2 -> ( &W1 -> ( ph -> ch ) ) )\n" +
//     "d4:d5,d6:ax-mp          |- ( &W1 -> ( ph -> ch ) )\n" +
//     "qed:d3,d4:ax-mp     |- ( ph -> ch ) \n" +
//     "\n" +
//     "$)")
// console.log("parsed", parsed)