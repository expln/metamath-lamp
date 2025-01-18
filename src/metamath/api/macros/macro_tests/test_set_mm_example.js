await api.setLogApiCallsToConsole(true)

await api.showInfoMsg({
    msg:
        "Do the following steps:\n" +
        "1. Load set.mm and stop before mathbox.\n" +
        "2. Run the code from set_mm_example.js in the console.\n" +
        "3. Click Ok."
})

async function bottomUpProver_proves_B_is_CC_from_B_is_QQ() {
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
    console.log('### proved = ', await prove({stepToProve: '1', stepsToDeriveFrom: ['h4']}))
    await unifyAll()
}

await bottomUpProver_proves_B_is_CC_from_B_is_QQ()