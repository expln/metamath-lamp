await api.setLogApiCallsToConsole(true)

const ALL_TESTS = []

async function runAllTests() {
    for (const test of ALL_TESTS) {
        console.log(`Starting test: ${test.name}`)
        await test()
        console.log(`Passed test: ${test.name}`)
    }
    console.log(`All ${ALL_TESTS.length} tests passed.`)
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

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

async function resetEditorContent() {
    getResponse(await api.editor().resetEditorContent())
}

async function addStepsToEditor({steps,vars}) {
    getResponse(await api.editor().addSteps({steps,vars}))
}

async function markStepsCheckedInEditor(labels) {
    getResponse(await api.editor().markStepsChecked({labels}))
}

async function assertStepIsProved(stepLabel) {
    const editorState = await getEditorState()
    if (!editorState.steps.some(step => step.label === stepLabel && step.status === 'v')) {
        exn(`Step is not proved: ${stepLabel}`)
    }
}

async function prove() {
    await api.macro.runMacro({moduleName:'set.mm example macros', macroName:'Prove'})
}

async function test_bottomUpProver_proves_B_is_CC_from_B_is_QQ() {
    await resetEditorContent()
    await addStepsToEditor({
        steps: [
            {label:'h1', type:'h', stmt:'|- ( ph -> A e. QQ )'},
            {label:'h2', type:'h', stmt:'|- ( ph -> B e. QQ )'},
            {label:'h3', type:'h', stmt:'|- ( ph -> C e. QQ )'},
            {label:'h4', type:'h', stmt:'|- ( ph -> ( ( sqrt ` ( A - ( ( ( B - B ) ^ 2 ) x. C ) ) ) / ( cos ` ( A + ( B + C ) ) ) ) = ; 1 0 )'},
            {label:'1', type:'p', stmt:'|- ( ph -> ( ( sqrt ` ( A - ( ( 0 ^ 2 ) x. C ) ) ) / ( cos ` ( A + ( B + C ) ) ) ) = ; 1 0 )'},
        ]
    })
    await markStepsCheckedInEditor(['h4', '1'])
    await prove()
    await assertStepIsProved('1')
}
ALL_TESTS.push(test_bottomUpProver_proves_B_is_CC_from_B_is_QQ)

await runAllTests()