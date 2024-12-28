await api.setLogApiCallsToConsole(true)

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

async function showInfoMsg(msg) {
    getResponse(await api.showInfoMsg({msg: String(msg)}))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg({msg: String(msg)}))
}

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

async function resetEditorContent() {
    getResponse(await api.editor().resetEditorContent())
}

async function setDescriptionInEditor(descr) {
    getResponse(await api.editor().setDescription({descr}))
}

async function addStepsToEditor({steps,vars}) {
    getResponse(await api.editor().addSteps({steps,vars}))
}

async function setDisjointsInEditor(disj) {
    getResponse(await api.editor().setDisjoints({disj}))
}

async function unifyAll() {
    getResponse(await api.editor().unifyAll())
    return await getEditorState()
}

async function substitute({what, with_}) {
    return getResponse(await api.editor().substitute({what, with_}))
}

async function mergeDuplicatedSteps() {
    return getResponse(await api.editor().mergeDuplicatedSteps())
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

const varTypes = new Map([['W','wff'],['S','setvar'],['C','class']])
const workVarRegex = /&[WSC]\d+/g
function extractWorkVars(steps)/*array<(string,string)>*/ {
    const workVars = []
    for (const step of steps) {
        for (const match of step.stmt.matchAll(workVarRegex)) {
            const workVar = match[0]
            workVars.push([varTypes.get(workVar[1]), workVar])
        }
    }
    return [...new Set(workVars.map(v => v.join(' ')))].map(s => s.split(' '))
}

function renameHyps(steps) {
    const renaming = new Map()/*oldLabel -> newLabel*/
    for (const step of steps) {
        if (step.type === 'h' && step.jstf.includes(':')) {
            renaming.set(step.label, step.jstf.split(':')[1])
        }
    }
    function rename(oldLabel) {
        return renaming.get(oldLabel)??oldLabel
    }
    for (const step of steps) {
        step.label = renaming.get(step.label)??step.label
        if (step.jstf.includes(':')) {
            const [args, asrt] = step.jstf.split(':')
            step.jstf = args.split(' ').map(rename).join(' ') + ':' + asrt
        }
    }
    return steps
}

const stepRegExp = /^([^:]+):([^:\s]*):(\S*)(\s.*)?/;
function parseMmp(mmpText) {
    const lines = mmpText.split('\n')
    let firstComment = undefined // option<string>
    const disj = [] // array<array<string>>
    const steps = [] // array<step>; step = {label:string, typ:string(h|p|g), jstf:string(1 2 3 : asrt), stmt:string}
    let curPart/**/ = undefined
    let partLines = []

    function parseStep(text)/*step | undefined*/ {
        const parts = text.match(stepRegExp)
        if (parts == null) {
            return undefined
        } else {
            let label = parts[1]
            const isHyp = label[0] === 'h'
            label = isHyp ? label.slice(1) : label
            label = label.slice(0,1) === '!' ? label.slice(1) : label
            let jstf = parts[2].split(',').join(' ') + ':' + parts[3]
            jstf = jstf === ':' ? '' : jstf
            return {
                label,
                type:isHyp?'h':(label==='qed'?'g':'p'),
                jstf,
                stmt: parts.length > 4 ? parts[4].trim() : ''
            }
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
        if (curPart === 'c') {
            if (firstComment === undefined && disj.length === 0 && steps.length === 0) {
                firstComment = partLines.join('\n').slice(1) //slice(1) removes the leading *
            }
        } else if (curPart === 'd') {
            //slice(3) removes the leading '$d '
            disj.push(partLines.join(' ').slice(3).split(' ').map(v => v.trim()).filter(v => v !== ''))
        } else if (curPart === 's') {
            steps.push(parseStep(partLines.join(' ')))
        }
        //prepare for the new part
        partLines = []
        curPart = newPart
    }

    for (let i = 0; i < lines.length; i++) {
        let line = lines[i]
        let newPart = getPartType(line)
        if (newPart !== undefined) {
            closePrevPartAndPrepareForNewPart(newPart)
        }
        partLines.push(line)
    }
    closePrevPartAndPrepareForNewPart(undefined)
    return {descr:firstComment??'', disj, vars:extractWorkVars(steps), steps:renameHyps(steps)}
}

async function importFromMmp() {
    const {okClicked, text:mmpText} = getResponse(await api.multilineTextInput({prompt:'MMP file content:'}))
    if (!okClicked) {
        return
    }
    const {descr, vars, disj, steps} = parseMmp(mmpText)
    await resetEditorContent()
    await setDescriptionInEditor(descr)
    await addStepsToEditor({steps,vars})
    await setDisjointsInEditor(disj)
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
//     "d4:d5,d2:ax-mp          |- ( &W1 -> ( ph -> ch ) )\n" +
//     "qed:d3,d4:ax-mp     |- ( ph -> ch ) \n" +
//     "\n" +
//     "$)")
// console.log("parsed", parsed)