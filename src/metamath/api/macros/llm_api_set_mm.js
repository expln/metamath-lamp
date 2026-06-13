const moduleName = "LLM driven proofs"

await api.setLogApiCallsToConsole(true)
await api.settings.setMarkFirstProvableStepAsGoal(false)

function hasNoValue(value) {
    return value === undefined || value === null
}

function hasValue(value) {
    return !hasNoValue(value)
}

async function showInfoMsg(msg) {
    getResponse(await api.showInfoMsg({msg:String(msg)}))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg({msg:String(msg)}))
}

function panic(msg) {
    throw new Error(msg)
}

function getResponse(apiResponse) {
    if (apiResponse.isOk) {
        return apiResponse.res
    } else {
        panic(apiResponse.err)
    }
}

async function putTextToClipboard(text) {
    await navigator.clipboard.writeText(text);
}

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

/*
* Invoke "Unify All" and return the editor state after that.
* "Unify All" validates the editor for errors and assigns statuses to steps (proved, unproved, partially proved, etc.).
* */
async function unifyAll() {
    getResponse(await api.editor().unifyAll())
    return await getEditorState()
}

/*
* Unselect all selected steps.
* */
async function unselectAllSteps() {
    getResponse(await api.editor().markStepsChecked({labels:[]}))
}

function stepHasError(step) {
    return hasValue(step.stmtErr) || hasValue(step.syntaxErr) || hasValue(step.unifErr)
}

function editorStateHasError(st) {
    return hasValue(st.varsErr) || hasValue(st.disjErr) || st.steps.some(stepHasError)
}

function getIdxOfLabel(st, label) {
    return st.steps.findIndex(step => step.label === label)
}

/*
* Decide if a step should be shown to an LLM.
* Invoke "Unify All" before using this function, as steps must have the 'status' attribute assigned.
* */
function stepIsVisibleToLlm(step) {
    return step.isBkm /* show all bookmarked steps */
        || step.isHyp /* show all hypothesis steps */
        || step.isGoal /* show all goal steps */
        || (step.status === '?' || step.status === '~' || step.status === 'x') /* show all unproved steps */
        || stepHasError(step) /* show all steps having any error */
}

function getStepTypeForLlm({isGoal, isHyp, isBkm}) {
    if (isGoal) return 'G'
    if (isHyp) return 'H'
    return isBkm ? 'P' : 'p'
}

function getStepStatusForLlm({isHyp, status}) {
    if (isHyp) return '.'
    return status??'.'
}

/*
* Accepts any object (any primitive, array, object, null, undefined)
* and returns an array of all strings this object contains.
* For a string, a one element array containing that string will be returned.
* For null, undefined, and an empty object (i.e. {}) an empty array will be returned.
* For an object {a:1, b:{c:2, d:"AAA"}, e:"BBB"} an array ["AAA", "BBB"] will be returned.
* */
function getAllText(any) {
    if (typeof any === 'string') return [any];
    if (any === null || any === undefined || typeof any !== 'object') return [];
    return Object.values(any).flatMap(getAllText);
}

/*
* Convert a step object returned by unifyAll() to a new step object to be sent to an LLM.
* */
function makeStepForLlm({step, maxLabelLength}) {
    const type = getStepTypeForLlm(step)
    const status = getStepStatusForLlm(step)
    const label = step.label.padEnd(maxLabelLength, ' ');
    const errors = getAllText([step.stmtErr, step.syntaxErr, step.unifErr])
    let res = `${type} ${status} ${label} ${(step.stmt)}`
    if (errors.length) {
        res += '\n' + errors.join('\n')
    }
    return res
}

async function getEditorStateForLlm(){
    //unify all and get the full editor state
    const st = await unifyAll()
    const fullStepsToSendToLlm = st.steps.filter(stepIsVisibleToLlm)
    const maxLabelLength = Math.max(...fullStepsToSendToLlm.map(step=>step.label.length))
    const stepsToSendToLlm = fullStepsToSendToLlm.map(step=>makeStepForLlm({step, maxLabelLength}))
    const res = ['--- editor state begin ---']
    if (st.varsText.length > 0) {
        res.push('Variables:')
        res.push(st.varsText)
        if (st.varsErr.length > 0) {
            res.push('Error in variables:')
            res.push(st.varsErr)
        }
        res.push('')
    }
    if (st.disjText.length > 0) {
        res.push('Disjoints:')
        res.push(st.disjText)
        if (st.disjErr.length > 0) {
            res.push('Error in disjoints:')
            res.push(st.disjErr)
        }
        res.push('')
    }
    if (stepsToSendToLlm.length) {
        res.push('Steps:')
        for (const step of stepsToSendToLlm) {
            let stepHasError = step.includes('\n');
            if (stepHasError) {
                res.push('')
            }
            res.push(step)
            if (stepHasError) {
                res.push('')
            }
        }
    }
    res.push('--- editor state end ---')
    return res.join('\n')
}

async function putEditorStateForLlmToClipboard() {
    await putTextToClipboard(await getEditorStateForLlm());
    console.log('The editor state has been copied to the clipboard.')
}

async function addSteps({beforeLabel, afterLabel, variables, steps}) {
    const label = hasValue(beforeLabel) ? beforeLabel : afterLabel
    const st = await getEditorState()
    const atIdx = hasValue(label) ? getIdxOfLabel(st, label) : null
    if (hasValue(label) && atIdx < 0) {
        panic(`No step with label '${label}' exists.`)
    }
    getResponse(await api.editor().addSteps({
        atIdx,
        vars: variables,
        steps: steps.map(step => ({
            label: step.label,
            type: step.type,
            jstf: step.justification,
            stmt: step.statement,
            isBkm: true,
        })),
    }))
    await putEditorStateForLlmToClipboard()
}

async function updateSteps({steps}) {
    getResponse(await api.editor().updateSteps({
        steps: steps.map(step => ({
            label: step.label,
            typ: step.type,
            stmt: step.statement,
            jstf: step.justification,
            isBkm: step.isBookmarked,
        })),
    }))
    await putEditorStateForLlmToClipboard()
}

async function deleteSteps({labels}) {
    getResponse(await api.editor().deleteSteps({
        labels,
    }))
    await putEditorStateForLlmToClipboard()
}

/*
* This function is passed as an input parameter to the bottom-up prover.
* It dynamically changes the bottom-up prover parameters in some special cases during the proving process.
* */
function updateParams(params, expr, dist, intToSym, symToInt) {
    function relaxLengthRestriction({params, passedToLessEq}) {
        return {
            ...params,
            customParams: {...params.customParams, passedToLessEq},
            assertionParams: params.assertionParams.map(asrtParams => {
                if (asrtParams.minDist === 1) {
                    return {...asrtParams, statementLengthRestriction: passedToLessEq ? 'LessEq' : 'Less'}
                } else {
                    return asrtParams
                }
            })
        }
    }
    if (params.customParams === undefined) {
        params = {
            ...params,
            customParams: {
                symbolCodes: {elemOf:symToInt('e.'), closingParen:symToInt(')'),}
            }
        }
    }
    const provingIsElemOf = expr.length >= 3
        && (
            expr[expr.length-3] === params.customParams.symbolCodes.elemOf
            && expr[expr.length-1] === params.customParams.symbolCodes.closingParen
            || expr[expr.length-2] === params.customParams.symbolCodes.elemOf
        )
    if (provingIsElemOf) {
        if (!params.customParams.passedToLessEq) {
            params = relaxLengthRestriction({params, passedToLessEq:true})
        }
    } else if (params.customParams.passedToLessEq) {
        params = relaxLengthRestriction({params, passedToLessEq:false})
    }
    return params
}

const updateParamsStr = updateParams.toString()

async function provePriv({stepToProve, stepsToDeriveFrom, debugLevel}) {
    return getResponse(await api.editor().proveBottomUp({
        delayBeforeStartMs:200,
        stepToProve,
        debugLevel,
        maxSearchDepth:100,
        assertionParams: [
            {
                maxDist:0,
                stepsToDeriveFrom,
                allowNewDisjointsForExistingVariables:true,
                allowNewStatements: true,
                allowNewVariables: false,
                statementLengthRestriction: 'No',
            },
            {
                minDist:1,
                stepsToDeriveFrom:[],
                allowNewDisjointsForExistingVariables:true,
                allowNewStatements: true,
                allowNewVariables: false,
                statementLengthRestriction: 'Less',
            }
        ],
        updateParams: updateParamsStr
    }))
}

async function prove({stepToProve, stepsToDeriveFrom}) {
    await unselectAllSteps()
    stepsToDeriveFrom = stepsToDeriveFrom??[]
    if (stepsToDeriveFrom.includes(stepToProve)) {
        panic(`Steps to derive from ${stepsToDeriveFrom} must not include the step to prove '${stepToProve}'`)
    }
    const st = await getEditorState()
    const unknownLabels = [stepToProve, ...stepsToDeriveFrom].filter(lbl => getIdxOfLabel(st, lbl) < 0)
    if (unknownLabels.length > 0) {
        panic(`No steps exist for labels: ${unknownLabels}`)
    }
    const stepToProveIdx = getIdxOfLabel(st, stepToProve)
    const misplacedStepsToDeriveFrom = stepsToDeriveFrom.filter(lbl => stepToProveIdx < getIdxOfLabel(st, lbl))
    if (misplacedStepsToDeriveFrom.length > 0) {
        panic(
            `Some steps to derive from ${misplacedStepsToDeriveFrom} ` +
            `are located after the step to prove ${stepToProve}.`
        )
    }
    //run the bottom-up prover for the specified steps
    const proved = await provePriv({stepToProve, stepsToDeriveFrom, debugLevel:1})
    if (proved) {
        await putEditorStateForLlmToClipboard()
    }
}

function makeFrameForLlm({disj, hyps, asrt}) {
    const disjStr = disj.map(disjGrp => disjGrp.join(' ')).join(' $ ')
    const hypsStr = hyps.join('\n')
    let res = ''
    if (disjStr.length > 0) {
        res += `Disj:\n${disjStr}\n`
    }
    if (hypsStr.length > 0) {
        res += `Hyps:\n${hypsStr}\n`
    }
    res += `Asrt:\n${asrt}`
    return res
}

const FIND_ASSERTIONS_PAGE_SIZE = 100
let lastPattern = undefined
let lastFoundAssertions = undefined
async function findAssertions({pattern, pageNum}) {
    if (lastPattern !== pattern) {
        lastFoundAssertions = getResponse(await api.editor().findAssertions({pattern}))
        lastPattern = pattern
    }
    const pageIdx = (pageNum??1) - 1
    const minIdx = pageIdx*FIND_ASSERTIONS_PAGE_SIZE
    const maxIdx = minIdx + FIND_ASSERTIONS_PAGE_SIZE - 1
    const res = []
    let i = minIdx
    while (i < lastFoundAssertions.length && i <= maxIdx) {
        res.push(makeFrameForLlm(lastFoundAssertions[i]))
        i++
    }
    const numOfPages = Math.ceil(lastFoundAssertions.length / FIND_ASSERTIONS_PAGE_SIZE)
    const header = `Results for pattern '${pattern}', page ${pageIdx + 1} of ${numOfPages}`
    const pageContent = res.join('\n\n-----\n')
    await putTextToClipboard(`${header}\n\n${pageContent}`);
    console.log('Found assertions have been copied to the clipboard.')
}

function makeFunctionMap(fns) {
    return Object.fromEntries(
        fns.map(fn => [fn.name, async params => await fn(params)])
    )
}

const AVAILABLE_ACTIONS = {
    getState: async params => await putEditorStateForLlmToClipboard(params),
    ...makeFunctionMap([findAssertions, addSteps, updateSteps, deleteSteps, prove])
}

async function runLlmSuggestedAction() {
    const {okClicked, text:actionText} = getResponse(await api.multilineTextInput({
        prompt:'Enter an LLM suggested action in JSON format:'
    }))
    if (okClicked) {
        const {functionName, parameters} = JSON.parse(actionText)
        if (hasNoValue(functionName)) {
            panic("No function name was specified.")
        }
        const func = AVAILABLE_ACTIONS[functionName]
        if (hasNoValue(func)) {
            panic(`The specified function '${functionName}' is not defined.`)
        }
        await func(parameters)
    }
}

function makeMacro(name, func) {
    return {
        name,
        run: async () => {
            try {
                await func()
            } catch (ex) {
                showErrMsg(`${ex.message}\n\n${ex.stack}`)
                throw ex
            }
        }
    }
}

await api.macro.registerMacroModule({
    moduleName,
    macros: [
        makeMacro('Copy editor state for LLM to clipboard', putEditorStateForLlmToClipboard),
        makeMacro('Run LLM suggested action', runLlmSuggestedAction),
    ]
})