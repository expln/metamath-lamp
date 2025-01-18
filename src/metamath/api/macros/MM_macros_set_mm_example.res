let setMmExampleMacros = `const moduleName = 'set.mm example macros'

await api.setLogApiCallsToConsole(true)

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
    getResponse(await api.showInfoMsg({msg:String(msg)}))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg({msg:String(msg)}))
}

async function getEditorState() {
    return getResponse(await api.editor().getState())
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

async function addSteps({atIdx, steps, vars}) {
    return getResponse(await api.editor().addSteps({atIdx:undefToNull(atIdx), steps, vars:undefToNull(vars)}))
}

async function resetEditorContent() {
    getResponse(await api.editor().resetEditorContent())
}

async function addStepsToEditor({steps,vars}) {
    getResponse(await api.editor().addSteps({steps,vars}))
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
        && expr[expr.length-3] === params.customParams.symbolCodes.elemOf
        && expr[expr.length-1] === params.customParams.symbolCodes.closingParen
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
        && (
            expr[expr.length-3] !== params.customParams.symbolCodes.elemOf
            || expr[expr.length-1] !== params.customParams.symbolCodes.closingParen
        )
    ) {
        params = undefined
    }
    return params
}

const updateParamsStr = updateParams.toString()

async function provePriv({stepToProve, stepsToDeriveFrom, selectFirstFoundProof, debugLevel}) {
    return getResponse(await api.editor().proveBottomUp({
        delayBeforeStartMs:200,
        stepToProve,
        debugLevel,
        maxSearchDepth:100,
        selectFirstFoundProof,
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
    if (!(await provePriv({stepToProve, stepsToDeriveFrom, selectFirstFoundProof:true, debugLevel:0}))) {
        return await provePriv({stepToProve, stepsToDeriveFrom, selectFirstFoundProof:false, debugLevel:1})
    } else {
        return true
    }
}

function getLabelsOfSelectedProvableSteps(editorState) {
    return editorState.steps
        .filter(step => !step.isHyp && editorState.selectedSteps.includes(step.label))
        .map(step => step.label)
}

async function setMmProve() {
    const editorState = await getEditorState()
    const labelsOfSelectedProvableSteps = getLabelsOfSelectedProvableSteps(editorState)
    if (labelsOfSelectedProvableSteps.length === 0) {
        await showErrMsg('Select at least one provable step.')
        return
    }
    // const stepToProve = labelsOfSelectedProvableSteps[labelsOfSelectedProvableSteps.length-1]
    const stepToProve = labelsOfSelectedProvableSteps[0]
    await prove({
        stepToProve,
        stepsToDeriveFrom: editorState.selectedSteps.filter(label => label !== stepToProve)
    })
}

function makeMacro(name, func) {
    return {
        name,
        run: async () => {
            try {
                await func()
            } catch (ex) {
                await showErrMsg([!@#]backtick[!@#][!@#]dollar[!@#]{ex.message}[!@#]backslash[!@#]n[!@#]dollar[!@#]{ex.stack}[!@#]backtick[!@#])
                throw ex
            }
        }
    }
}

await api.macro.registerMacroModule({
    moduleName,
    macros: [
        makeMacro('Prove', setMmProve),
    ]
})


// async function test1() {
//     await resetEditorContent()
//     await addStepsToEditor({
//         steps: [
//             {label:'h1', type:'h', stmt:'|- ( ph -> A e. QQ )'},
//             {label:'h2', type:'h', stmt:'|- ( ph -> B e. QQ )'},
//             {label:'h3', type:'h', stmt:'|- ( ph -> C e. QQ )'},
//             {label:'h4', type:'h', stmt:'|- ( ph -> ( ( sqrt [!@#]backtick[!@#] ( A - ( ( ( B - B ) ^ 2 ) x. C ) ) ) / ( cos [!@#]backtick[!@#] ( A + ( B + C ) ) ) ) = ; 1 0 )'},
//             {label:'1', type:'p', stmt:'|- ( ph -> ( ( sqrt [!@#]backtick[!@#] ( A - ( ( 0 ^ 2 ) x. C ) ) ) / ( cos [!@#]backtick[!@#] ( A + ( B + C ) ) ) ) = ; 1 0 )'},
//         ]
//     })
//     console.log('### proved = ', await prove({stepToProve: '1', stepsToDeriveFrom: ['h4']}))
//     await unifyAll()
// }
//
// await test1()`