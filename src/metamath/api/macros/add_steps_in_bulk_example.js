const moduleName = 'Add steps in bulk'

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

async function getEditorState() {
    return getResponse(await api.editor().getState())
}

function getIdxOfLabel(st, label) {
    return st.steps.findIndex(step => step.label === label)
}

function parseSteps(str) {
    return str
        .split('\n')
        .map(line => line.trim())
        .filter(line => line.length > 0);
}

async function addMultipleSteps() {
    const st = await getEditorState()
    if (st.selectedSteps.length > 1) {
        panic(`At most one step can be selected, but got ${st.selectedSteps.length}: ${st.selectedSteps}`)
    }
    const atIdx = st.selectedSteps.length === 0 ? null : getIdxOfLabel(st, st.selectedSteps[0])
    const {okClicked, text:stepsText} = getResponse(await api.multilineTextInput({
        prompt:'Enter steps to add (one step per line)'
    }))
    if (okClicked) {
        const stepsToAdd = parseSteps(stepsText)
        if (stepsToAdd.length === 0) {
            panic("No steps were provided.")
        }
        getResponse(await api.editor().addSteps({
            atIdx,
            steps: stepsToAdd.map(step => ({stmt: step})),
        }))
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
        makeMacro('Add steps in bulk', addMultipleSteps),
    ]
})