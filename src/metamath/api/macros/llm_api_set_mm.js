const moduleName = 'LLM driven proofs'

await api.setLogApiCallsToConsole(true)

function exn(msg) {
    throw new Error(msg)
}

function hasNoValue(value) {
    return value === null || value === undefined
}

function hasValue(value) {
    return !hasNoValue(value)
}

function getResponse(apiResponse) {
    if (apiResponse.isOk) {
        return apiResponse.res
    } else {
        exn(apiResponse.err)
    }
}

async function showInfoMsg(msg) {
    getResponse(await api.showInfoMsg({msg:String(msg)}))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg({msg:String(msg)}))
}

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

function stepIsVisibleToLlm(step) {
    return step.isBkm /* show all bookmarked steps */
        || step.status !== 'v' /* show all unproved steps */
        || step.stmtErr || step.syntaxErr || step.unifErr /* show all steps having any error */
}

function getStepTypeForLlm({isGoal, isHyp}) {
    if (isGoal) return 'g'
    if (isHyp) return 'h'
    return 'p'
}

function getStepStatusForLlm({isHyp, status}) {
    if (isHyp) return null
    return status
    // if (status === 'v') return 'proved'
    // if (status === '?') return 'unproved'
    // if (status === '~') return 'jstf_is_correct'
    // if (status === 'x') return 'jstf_is_incorrect'
    // return 'undefined'
}

function minimizeStepForLlm(step) {
    return {
        status: getStepStatusForLlm(step),
        label: step.label,
        type: getStepTypeForLlm(step),
        justification: step.jstfText,
        statement: step.stmt,
        isBookmarked: step.isBkm,
    }
}

async function getEditorStateForLlm(){
    await unselectAllSteps()
    //assign statuses for all steps
    getResponse(await api.editor().unifyAll())
    //get full editor state
    const st = await getEditorState()
    //prepare minimized editor state for an LLM
    const stLlm = {
        variables: st.varsText,
        disjoints: st.disjText,
        disjointsError: st.disjErr,
        steps: st.steps.filter(stepIsVisibleToLlm).map(minimizeStepForLlm)
    }
    //return editor state as a pretty printed JSON
    return JSON.stringify(stLlm, null, 4)
}

async function copyEditorStateForLlmToClipboard() {
    await navigator.clipboard.writeText(await getEditorStateForLlm());
    console.log('The editor state has been copied to clipboard.')
}

async function unifyAll() {
    getResponse(await api.editor().unifyAll())
    return await getEditorState()
}

async function updateSteps(steps) {
    return getResponse(await api.editor().updateSteps({steps}))
}

async function deleteSteps(labels) {
    return getResponse(await api.editor().deleteSteps({labels}))
}

function undefToNull(value) {
    return value === undefined ? null : value
}

function getIdxOfLabel(st, label) {
    return st.steps.findIndex(step => step.label === label)
}

async function addSteps({beforeLabel, afterLabel, variables, steps}) {
    const label = hasValue(beforeLabel) ? beforeLabel : afterLabel
    const st = getEditorState()
    const atIdx = hasValue(label) ? getIdxOfLabel(st, label) : null
    if (hasValue(label) && atIdx < 0) {
        showErrMsg(`No step with label '${label}' exists.`)
        return
    }
    getResponse(await api.editor().addSteps({
        atIdx:hasValue(label) ? atIdx : null,
        vars: variables,
        steps: steps.map(step => ({
            label: step.label,
            typ: step.type,
            jstf: step.justification,
            stmt: step.statement,
            isBkm: hasValue(step.isBookmarked) ? step.isBookmarked : true,
        })),
    }))
    await copyEditorStateForLlmToClipboard()
}

async function updateSteps({steps}) {
    getResponse(await api.editor().updateSteps({
        steps: steps.map(step => ({
            label: step.label,
            type: step.typ,
            stmt: step.statement,
            jstf: step.justification,
            isBkm: step.isBookmarked,
        })),
    }))
    await copyEditorStateForLlmToClipboard()
}

async function deleteSteps({labels}) {
    getResponse(await api.editor().deleteSteps({
        labels,
    }))
    await copyEditorStateForLlmToClipboard()
}

async function resetEditorContent() {
    getResponse(await api.editor().resetEditorContent())
}

function updateParams(params, expr, dist, intToSym, symToInt) {
    if (params.customParams === undefined) {
        params = {
            ...params,
            customParams: {
                symbolCodes: {
                    elemOf:symToInt('e.'),
                    closingParen:symToInt(')'),
                }
            }
        }
    }
    if (
        expr.length >= 3
        && (
            expr[expr.length-3] === params.customParams.symbolCodes.elemOf
                && expr[expr.length-1] === params.customParams.symbolCodes.closingParen
            || expr[expr.length-2] === params.customParams.symbolCodes.elemOf
        )
    ) {
        params = {
            ...params,
            customParams: {
                ...params.customParams,
                passedToLessEq:true
            },
            assertionParams: params.assertionParams.map(asrtParams => {
                if (asrtParams.minDist === 1) {
                    return {...asrtParams, statementLengthRestriction: 'LessEq'}
                } else {
                    return asrtParams
                }
            })
        }
    }
    if (
        expr.length >= 3
        && params.customParams.passedToLessEq
        && !(
            expr[expr.length-3] === params.customParams.symbolCodes.elemOf
            && expr[expr.length-1] === params.customParams.symbolCodes.closingParen
            || expr[expr.length-2] === params.customParams.symbolCodes.elemOf
        )
    ) {
        params = undefined
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

async function unselectAllSteps() {
    getResponse(await api.editor().markStepsChecked({labels:[]}))
}

async function prove({stepToProve, stepsToDeriveFrom}) {
    await unselectAllSteps()
    if (hasValue(stepsToDeriveFrom) && stepsToDeriveFrom.includes(stepToProve)) {
        showErrMsg(`Steps to derive from ${stepsToDeriveFrom} must not include the step to prove '${stepToProve}'`)
        return
    }
    const st = getEditorState()
    const unknownLabels = [stepToProve, ...(stepsToDeriveFrom??[])].filter(lbl => getIdxOfLabel(st, lbl) < 0)
    if (unknownLabels.length > 0) {
        showErrMsg(`No steps exist for labels: ${unknownLabels}`)
        return
    }
    //run bottom-up prover for the specified steps
    await provePriv({stepToProve, stepsToDeriveFrom, debugLevel:1})

}

const AVAILABLE_ACTIONS = {
    getState: async params => await copyEditorStateForLlmToClipboard(params),
    addSteps: async params => await addSteps(params),
    updateSteps: async params => await updateSteps(params),
    deleteSteps: async params => await deleteSteps(params),
    prove: async params => await prove(params),
}

async function runLlmSuggestedAction() {
    const {okClicked, text:actionText} = getResponse(await api.multilineTextInput({prompt:'Enter LLM suggested action in JSON format:'}))
    if (okClicked) {
        const {functionName, parameters} = JSON.parse(actionText)
        if (hasNoValue(functionName)) {
            await showErrMsg("No function name was specified.")
            return
        }
        const func = AVAILABLE_ACTIONS[functionName]
        if (hasNoValue(func)) {
            await showErrMsg(`The specified function '${functionName}' is not defined.`)
            return
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
                await showErrMsg(`${ex.message}\n${ex.stack}`)
                throw ex
            }
        }
    }
}

await api.macro.registerMacroModule({
    moduleName,
    macros: [
        makeMacro('Copy editor state for LLM to clipboard', copyEditorStateForLlmToClipboard),
        makeMacro('Run LLM suggested action', runLlmSuggestedAction),
    ]
})