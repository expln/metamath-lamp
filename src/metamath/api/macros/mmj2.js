await api.setLogApiCallsToConsole(true)

const NEW_LINE = String.fromCharCode(10)

function hasNoValue(x) {
    return x === undefined || x === null
}

function hasValue(x) {
    return !hasNoValue(x)
}

function exn(msg) {
    throw new Error(msg)
}

async function sleep(millis) {
    return new Promise((resolve,reject) => {
        setTimeout(() => resolve(), millis)
    })
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

async function updateStepsInEditor(steps) {
    getResponse(await api.editor().updateSteps({steps}))
}

async function markStepsCheckedInEditor(labels) {
    getResponse(await api.editor().markStepsChecked({labels}))
}

async function addAsrtByLabel(label) {
    getResponse(await api.editor().addAsrtByLabel({asrtLabel:label}))
}

async function substitute({what, with_}) {
    const resp = await api.editor().substitute({what, with_, method:'u'})
    if (resp.isOk) {
        return true
    } else if (resp.err === 'No substitutions found.') {
        return false
    } else {
        exn(resp.err)
    }
}

async function setDisjointsInEditor(disj) {
    getResponse(await api.editor().setDisjoints({disj}))
}

async function unifyAll() {
    getResponse(await api.editor().unifyAll())
    return await getEditorState()
}

async function mergeDuplicatedSteps() {
    getResponse(await api.editor().unifyAll())
    return getResponse(await api.editor().mergeDuplicatedSteps())
}

async function renameStepsInEditor(renaming) {
    getResponse(await api.editor().renameSteps({renaming}))
}

async function setMarkFirstProvableStepAsGoal(bool) {
    getResponse(await api.settings.setMarkFirstProvableStepAsGoal(bool))
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
    const lines = mmpText.split(NEW_LINE)
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
                firstComment = partLines.join(NEW_LINE).slice(1) //slice(1) removes the leading *
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

async function loadMmpTextToEditor(mmpText) {
    await setMarkFirstProvableStepAsGoal(false)
    await sleep(100) // the sleep() is needed for the setting changes to propagate to all the React components
    const {descr, vars, disj, steps} = parseMmp(mmpText)
    await resetEditorContent()
    await setDescriptionInEditor(descr)
    await addStepsToEditor({steps,vars})
    await setDisjointsInEditor(disj)
    await unifyAll()
}

async function importFromMmp() {
    const {okClicked, text:mmpText} = getResponse(await api.multilineTextInput({prompt:'MMP file content:'}))
    if (okClicked) {
        await loadMmpTextToEditor(mmpText)
    }
}

function getFirstStepWithError(editorState) {
    return editorState.steps.find(step =>
        step.status === 'x' || hasValue(step.stmtErr) || hasValue(step.syntaxErr) || hasValue(step.unifErr)
    )
}

async function editorHasErrors() {
    return getFirstStepWithError(await unifyAll()) !== undefined
}

function getLabelAfterRenaming(renaming, label) {
    for (const [oldLabel, newLabel] of renaming) {
        if (label === oldLabel) {
            label = newLabel
        }
    }
    return label
}

async function unifyByAddingAsrt({stepLabelToUnify, asrtLabel}) {
    await markStepsCheckedInEditor([stepLabelToUnify])
    await addAsrtByLabel(asrtLabel)
    await markStepsCheckedInEditor([])
    const editorState = await getEditorState()
    const existingStepIdx = getStepIdx(editorState, stepLabelToUnify)
    const newStepIdx = existingStepIdx - 1
    const existingStep = editorState.steps[existingStepIdx];
    const newStep = editorState.steps[newStepIdx];
    const existingStmt = existingStep.stmt
    const newStepStmt = newStep.stmt
    if (!(await substitute({what:newStepStmt, with_:existingStmt}))) {
        if (!(await substitute({what:existingStmt, with_:newStepStmt}))) {
            await showErrMsg(`Cannot unify these two steps: ${newStep.label} and ${existingStep.label}`)
            return
        }
    }
    await getEditorState()
    const renaming = await mergeDuplicatedSteps()
    const oldLabel = existingStep.label
    const newLabel = getLabelAfterRenaming(renaming, oldLabel)
    await renameStepsInEditor([[newLabel,oldLabel]])
    await unifyAll()
}

async function mmj2Unify() {
    //unify all to trigger error check
    const editorState = await unifyAll()
    const stepWithErr = getFirstStepWithError(editorState)
    if (stepWithErr === undefined) {
        await showInfoMsg('Nothing to unify.')
        return
    }
    if (
        (stepWithErr.unifErr??'').startsWith('Could not find a match for assertion')
        && stepWithErr.jstf.args.length === 0
    ) {
        //check if this is the only error
        const origJstf = stepWithErr.jstfText
        await updateStepsInEditor([{label:stepWithErr.label, jstf:''}])
        if (await editorHasErrors()) {
            await updateStepsInEditor({steps:[{label:stepWithErr.label, jstf:origJstf}]})
            await unifyAll()
            await showErrMsg('Cannot determine how to unify because there are more than one error in the editor.')
            return
        }
        await unifyByAddingAsrt({stepLabelToUnify:stepWithErr.label, asrtLabel:stepWithErr.jstf.asrt})
    } else {
        await showInfoMsg('Cannot determine how to unify.')
    }
}

function makeMacro(name, func) {
    return {
        name,
        run: async () => {
            try {
                await func()
            } catch (ex) {
                await showErrMsg(`${ex.message}${NEW_LINE}${ex.stack}`)
                throw ex
            }
        }
    }
}

await api.macro.registerMacroModule({
    moduleName: 'MMJ2',
    macros: [
        makeMacro('Unify', mmj2Unify),
        makeMacro('Import from MMP file', importFromMmp),
    ]
})
