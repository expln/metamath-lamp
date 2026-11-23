// Converts the editor state returned by metamath-lamp's api.editor().getState()
// to compact plain text for an AI agent.
// The format is based on getEditorStateForLlm() from src/metamath/api/macros/llm_api_set_mm.js.

function hasValue(value) {
    return value !== undefined && value !== null
}

function isNonEmptyStr(value) {
    return typeof value === 'string' && value.trim().length > 0
}

function getStepErrors(step) {
    const errors = []
    if (hasValue(step.stmtErr)) errors.push(step.stmtErr.msg)
    if (isNonEmptyStr(step.syntaxErr)) errors.push(step.syntaxErr)
    if (isNonEmptyStr(step.unifErr)) errors.push(step.unifErr)
    return errors.filter(isNonEmptyStr)
}

function stepIsRelevant(step) {
    return step.isBkm
        || step.isHyp
        || step.isGoal
        || step.status === '?' || step.status === '~' || step.status === 'x'
        || getStepErrors(step).length > 0
}

function getStepType({isGoal, isHyp, isBkm}) {
    if (isGoal) return 'G'
    if (isHyp) return 'H'
    return isBkm ? 'P' : 'p'
}

function getStepStatus({isHyp, status}) {
    if (isHyp) return '.'
    return status ?? '.'
}

/*
* Returns "    <prefix>: <text>" where continuation lines of a multiline text are aligned with its first line.
* */
function indentedField(prefix, text) {
    const head = `    ${prefix}: `
    const pad = ' '.repeat(head.length)
    return text.trim().split('\n').map((line, i) => (i === 0 ? head : pad) + line).join('\n')
}

function stepToText({step, maxLabelLength}) {
    const lines = [`${getStepType(step)} ${getStepStatus(step)} ${step.label.padEnd(maxLabelLength, ' ')} ${step.stmt}`]
    if (!step.isHyp && isNonEmptyStr(step.jstfText)) {
        lines.push(indentedField('jstf', step.jstfText))
    }
    for (const err of getStepErrors(step)) {
        lines.push(indentedField('error', err))
    }
    return lines.join('\n')
}

function pushSection(res, title, text, err) {
    if (!isNonEmptyStr(text) && !isNonEmptyStr(err)) {
        return
    }
    res.push(`${title}:`)
    if (isNonEmptyStr(text)) {
        res.push(text.trim())
    }
    if (isNonEmptyStr(err)) {
        res.push(indentedField('error', err))
    }
    res.push('')
}

/*
* state - the result of api.editor().getState()
* onlyRelevantSteps - if true, proved non-bookmarked steps without errors are omitted.
* notes - additional lines to show at the top.
* */
export function editorStateToText({state, onlyRelevantSteps, notes = []}) {
    const allSteps = state.steps
    const steps = onlyRelevantSteps ? allSteps.filter(stepIsRelevant) : allSteps
    const maxLabelLength = Math.max(0, ...steps.map(step => step.label.length))
    const res = ['--- editor state begin ---']
    res.push(`Editor tab id: ${state.editorId}`)
    for (const note of notes) {
        res.push(`Note: ${note}`)
    }
    res.push('')
    pushSection(res, 'Description', state.descr)
    pushSection(res, 'Variables', state.varsText, state.varsErr)
    pushSection(res, 'Disjoints', state.disjText, state.disjErr)
    if (state.selectedSteps.length > 0) {
        res.push(`Selected steps: ${state.selectedSteps.join(' ')}`)
        res.push('')
    }
    if (steps.length < allSteps.length) {
        res.push(`Steps (${steps.length} of ${allSteps.length} shown, proved non-bookmarked steps are hidden):`)
    } else {
        res.push(`Steps (${allSteps.length}):`)
    }
    for (const step of steps) {
        res.push(stepToText({step, maxLabelLength}))
    }
    res.push('--- editor state end ---')
    return res.join('\n')
}
