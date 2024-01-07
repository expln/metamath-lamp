let setMmExampleMacros = `
const frmsToUse = [
    //common
    "_reexpcli","3t2e6","6p1e7","3t3e9","recni","mulneg2i","2p2e4","4p1e5","2t2e4","4p3e7","7p1e8","3p2e5","3p1e4",
    "4p4e8","7p2e9","8p1e9","4p4e8","3p3e6","5p1e6",
    "0nn0","1nn0","2nn0","3nn0","4nn0","5nn0","6nn0","7nn0","8nn0","9nn0","10nn0",
    "recn","subidd","addid2d","peano2re","0re", "subadd23", "resqcld","posdifd","3cn","df-3","expp1",
    "ax-1cn", "sq1","mulexp", "3re", "reexpcl", "neg1cn","n2dvds3","3z","m1expo", "1exp", "1pneg1e0",
    "sqneg","3nn","expm1t","3m1e2","1p2e3","oveq2i","df-7","6cn","6p2e8","neg1sqe1","2cn","mulcomi","eqtr2i",
    "qre","qcn","nnq","nnexpcld","resqcl","peano2rem","nnexpcld","oveq1","eqeq2d","qmulcl","qexpcl","1nn","qsubcl",
    "sylancl",
    //inference
    "pm3.2i","3pm3.2i",
    // "_anpnaneq0_i",
    // "resqcli","resubcli","ltadd1i","recni","addid1i", "subidi", "oveq2i","eqcomi","eqtr4i","breqtri","breqtrri",
    // "eqbrtri","posdifi", "sqge0i","ltletri","oveq1i","sqvali","adddiri","sqcli","mulassi","mulcomi","mulid2i","mulcli",
    // "addcli", "addsubassi","binom2i","eqeltri","addassi","addcomi","2timesi", "mulid1i","sqmuli","adddii", "nn0expcli",
    // "nn0rei","remulcli", "readdcli","negsubi", "renegcli", "mulm1i","eqtri","eqtr3i",
    //deduction
    "_sqnegd","_a23eqa32","_a3eqaaa","_reexpcld","_anpnaneq0_d",
    "a1i","3jca","negidd","mulneg1d","mulneg2d","times2d",
    "resqcld","resubcld","ltadd1d","recnd","addid1d","oveq2d","eqcomd","eqtr4d","breqtrd","breqtrrd","eqbrtrd","posdifd",
    "sqge0d","ltletrd","oveq1d","sqvald","adddird","sqcld","mulassd","mulcomd","mulid2d","mulcld","addcld","subidd",
    "addsubassd","binom2d","eqeltrd","addassd","addcomd","2timesd","mulid1d","sqmuld","adddid",
    "nn0expcld","nn0red","remulcld","readdcld","negsubd",
    "renegcld","mulm1d","eqtrd","eqtr3d",
    "reexpcld","1cnd","negcld","jca","0red","0re","1red","2re","3re","4re","5re","6re","7re","8re","9re",
    "mulexpd","negeqd","sqvald","expmuld","expp1d","nn0mulcli","expaddd","nn0addcli","nn0addcld","comraddd","mulcomd",
    "eqtr2d","eqnetrd","neqned","expdivd","expcld","expne0d","divdird",

    "_3cubeslem3_pow1",
    "_3cubeslem3_pow2",
    "_3cubeslem3_pow3",
    "_3cubeslem3_pow4",
    "_3cubeslem3_pow5",
    "_3cubeslem3_pow6",
    "_3cubeslem3_pow7",
    "_3cubeslem3_left_part_d",
    "_3cubeslem3_right_part_d",
    "3cubeslem2",
]
const frmsToExclude = [
    "mpbir","id","eqidd",
    "eqid",
    //slow
    "ax-mp","mpbi","mp2an",
    //not for now
    "cu3addi", "cu3addd", "binom3", "mp1i","syl",
    "mpd","rspc3ev","_3cubeslem2_RR","3cubeslem4","pm2.18i","mpan2","syl2anc",
]

const NUMBER_CONSTANTS = ['0','1','2','3','4','5','6','7','8','9']

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

getResponse(await api.setLogApiCallsToConsole(true))

async function setContentIsHidden(contIsHidden) {
    return getResponse(await api.editor.setContentIsHidden(contIsHidden))
}

async function getEditorState() {
    return getResponse(await api.editor.getState())
}

async function unifyAll() {
    getResponse(await api.editor.unifyAll())
    return await getEditorState()
}

async function getEditorStateWithSyntaxTrees() {
    let state = await getEditorState()
    if (state.steps.every(step => step.tree !== null)) {
        return state
    }
    state = await unifyAll()
    if (state.steps.every(step => step.tree !== null)) {
        return state
    }
    exn(\`Cannot get an editor state with syntax trees.\`)
}

async function updateSteps(steps) {
    return getResponse(await api.editor.updateSteps(steps))
}

function undefToNull(value) {
    return value === undefined ? null : value
}

async function addSteps({atIdx, steps, vars}) {
    return getResponse(await api.editor.addSteps({atIdx:undefToNull(atIdx), steps, vars:undefToNull(vars)}))
}

async function buildSyntaxTrees(exprs) {
    return getResponse(await api.editor.buildSyntaxTrees(exprs))
}

async function getTokenType(token) {
    return getResponse(await api.editor.getTokenType(token))
}

async function substitute({what, with_}) {
    return getResponse(await api.editor.substitute({what, with_}))
}

async function mergeDuplicatedSteps() {
    return getResponse(await api.editor.mergeDuplicatedSteps())
}

function isSymbol(tree) {
    return tree.nodeType === "sym"
}

function isVar(tree) {
    return tree.nodeType === "expr" && tree.children.length === 1 && tree.children[0].isVar
}

function getVarName(tree) {
    if (!isVar(tree)) {
        exn('Not a variable')
    }
    return tree.children[0].sym
}

function makeConst(sym) {
    return {nodeType:"sym", isVar:false, sym}
}

function isObj(x) {
    return x !== undefined && x !== null && typeof x === 'object' && !Array.isArray(x)
}

const MATCHER_ANY_SYMBOL = 'MATCHER_ANY_SYMBOL'
function anySym(allowedSymbols) {
    return ({matcherType:MATCHER_ANY_SYMBOL,allowedSymbols})
}
const MATCHER_EXPR_OF_TYPE = 'MATCHER_EXPR_OF_TYPE'
function exprOfType(type) {
    return ({matcherType:MATCHER_EXPR_OF_TYPE,type})
}
const MATCHER_VAR_OF_TYPE = 'MATCHER_VAR_OF_TYPE'
function varOfType(type) {
    return ({matcherType:MATCHER_VAR_OF_TYPE,type})
}
const MATCHER_VAR_OF_ANY_TYPE = 'MATCHER_VAR_OF_ANY_TYPE'
function anyVar() {
    return ({matcherType:MATCHER_VAR_OF_ANY_TYPE})
}
const MATCHER_USER_DEFINED = 'MATCHER_USER_DEFINED'
function makeMatcher(matcher) {
    return ({matcherType:MATCHER_USER_DEFINED, matcher})
}

function match(tree, pattern) {
    if (pattern.length === 0) {
        return tree
    }
    if (tree.nodeType !== "expr" || tree.children.length !== pattern.length) {
        return undefined
    }
    const result = []
    for (let i = 0; i < tree.children.length; i++) {
        const pat = pattern[i]
        const ch = tree.children[i]
        if (Array.isArray(pat)) {
            const subMatchResult = match(ch,pat)
            if (subMatchResult === undefined) {
                return undefined
            }
            result.push(subMatchResult)
        } else if (isObj(pat)) {
            if (pat.matcherType === MATCHER_ANY_SYMBOL && isSymbol(ch) && pat.allowedSymbols.includes(ch.sym)) {
                result.push(ch)
            } else if (pat.matcherType === MATCHER_EXPR_OF_TYPE && ch.exprType === pat.type) {
                result.push(ch)
            } else if (pat.matcherType === MATCHER_VAR_OF_TYPE && isVar(ch) && ch.exprType === pat.type) {
                result.push(ch)
            } else if (pat.matcherType === MATCHER_VAR_OF_ANY_TYPE && isVar(ch)) {
                result.push(ch)
            } else if (pat.matcherType === MATCHER_USER_DEFINED && pat.matcher(ch)) {
                result.push(ch)
            } else {
                return undefined
            }
        } else {
            if (pat === '' || isSymbol(ch) && pat === ch.sym) {
                result.push(ch)
            } else {
                return undefined
            }
        }
    }
    return result
}

function matchExn(tree, pattern) {
    const matchResult = match(tree, pattern)
    if (matchResult === undefined) {
        console.log('pattern', pattern)
        console.log('tree', tree)
        exn(\`the pattern doesn't match the tree (see above)\`)
    }
    return matchResult
}

function matchesAny(tree, patterns) {
    return patterns.some(pattern => match(tree,pattern) !== undefined)
}

async function getAllFrmsFromEditor() {
    return [...new Set((await getEditorState()).steps.filter(step => step.jstf).map(step => step.jstf.asrt))]
}

function arrSub(a, b) {
    return a.filter(e => !b.includes(e))
}

async function getNewAssertionsFromEditor() {
    return arrSub(await getAllFrmsFromEditor(), [...frmsToUse, ...frmsToExclude])
}

function syntaxTreeToText(node) {
    if (isSymbol(node)) {
        return node.sym
    } else {
        return node.children.map(syntaxTreeToText).join(' ')
    }
}

async function prove({step, use, frms, debug, maxSearchDepth}) {
    // debug = 1
    // maxSearchDepth = 30

    let allSteps = (await getEditorState()).steps
    let allHyps = allSteps.filter(s => s.isHyp).map(s => s.label)
    function getPrevLabel(label) {
        for (let i = 1; i < allSteps.length; i++) {
            if (allSteps[i].label === label) {
                return allSteps[i-1].label
            }
        }
        exn(\`Cannot find previous step for '\${label}'\`)
    }
    step = (step??'last') === 'last' ? allSteps[allSteps.length-1].label : step
    use = (use??['prev']).map(label => label === 'prev' ? getPrevLabel(step) : label)
    const debugLevel = debug??0

    getResponse(await api.editor.proveBottomUp({
        stepToProve:step,
        debugLevel,
        args0:[...use, ...allHyps],
        args1:[...use, ...allHyps],
        frmsToUse:frms??frmsToUse,
        maxSearchDepth: maxSearchDepth??20,
        lengthRestrict:'No',
        allowNewStmts:true,
        allowNewVars:false,
        allowNewDisjForExistingVars:false,
        selectFirstFoundProof:debugLevel === 0,
    }))
    if (debugLevel > 0) {
        return undefined
    } else {
        return await getEditorState()
    }
}

async function pr(str, debugLevel) {
    if (str === undefined || str === '') {
        const state = await getEditorState()
        if (state.selectedSteps.length > 0) {
            const sortedSteps = state.selectedSteps
                .map(label => [label,getStepIdx(state,label)]).sort((a,b) => b[1]-a[1])
                .map(labelIdx => labelIdx[0])
            await pr(sortedSteps.join(' '), debugLevel)
        } else {
            await pr('last', debugLevel)
        }
    } else {
        const [step, ...use] = str.split(/\s+/)
        if (use.length === 0) {
            await prove({step, debug: debugLevel})
        } else {
            await prove({step, use, debug: debugLevel})
        }
    }
}

function isGoalProved(editorState) {
    let goalSteps = editorState.steps.filter(step => step.isGoal)
    if (goalSteps.length !== 1) {
        exn(\`Cannot find the goal step.\`)
    } else {
        return goalSteps[0].status === "v"
    }
}

function isStepProved(editorState, label) {
    let steps = editorState.steps.filter(step => step.label === label)
    if (steps.length !== 1) {
        exn(\`Cannot find the step with the label '\${label}'\`)
    } else {
        return steps[0].status === "v"
    }
}

function getFirstUnprovedStep(editorState) {
    let unprovedSteps = editorState.steps.filter(step => !step.isHyp && step.status !== 'v')
    if (unprovedSteps.length === 0) {
        exn('There are no unproved steps.')
    } else {
        return unprovedSteps[0]
    }
}

function getStepIdx(editorState, label) {
    const steps = editorState.steps
    for (let i = 0; i < steps.length; i++) {
        if (steps[i].label === label) {
            return i
        }
    }
    exn(\`Cannot find the step with the label '\${label}'\`)
}

function getStepByLabel(editorState, label) {
    return editorState.steps[getStepIdx(editorState, label)]
}

async function inferenceToDeduction() {
    const allSteps = await getEditorStateWithSyntaxTrees().steps
    await updateSteps(allSteps.map(
        step => ({label:step.label, jstf:'', stmt:\`|- ( ph -> \${syntaxTreeToText(step.tree.root)} )\`})
    ))
    await inferenceToDeductionContinue()
}

async function inferenceToDeductionContinue() {
    let state = await unifyAll()
    while (!isGoalProved(state)) {
        const unprovedStep = getFirstUnprovedStep(state)
        const [lp, ph, arr, expr, rp] = matchExn(unprovedStep.tree.root, ['(', '', '->', '', ')'])
        const [newLabel] = await addSteps({
            atIdx:getStepIdx(state,unprovedStep.label),
            steps:[{stmt:'|- ' + syntaxTreeToText(expr)}]
        })
        state = await unifyAll()
        if (!isStepProved(state, newLabel)) {
            state = await pr(newLabel, [])
            if (!isStepProved(state, newLabel)) {
                exn(\`Cannot prove the step with the label '\${newLabel}'\`)
            }
        }
    }
}

async function deductionToInference() {
    let allSteps = (await getEditorStateWithSyntaxTrees()).steps
    const stepsToUpdate = []
    const stepsToAdd = []
    for (const step of allSteps) {
        const [lp, ph, arr, expr, rp] = matchExn(step.tree.root, ['(', '', '->', '', ')'])
        const exprStr = syntaxTreeToText(expr)
        stepsToAdd.push({stmt:\`|- ( T. -> \${exprStr} )\`})
        stepsToUpdate.push({label:step.label, jstf:'', stmt:\`|- \${exprStr}\`})
    }
    await updateSteps(stepsToUpdate)
    await addSteps({steps:stepsToAdd})
    await unifyAll()
}

function buildConjArr(stmts) {
    if (stmts.length <= 3) {
        return stmts
    }
    const res = [[]]
    for (const stmt of stmts) {
        let lastElem = res[res.length-1]
        if (lastElem.length === 3) {
            lastElem = []
            res.push(lastElem)
        }
        lastElem.push(stmt)
    }
    return buildConjArr(res)
}

function conjToStr(conj) {
    if (!Array.isArray(conj)) {
        return conj
    }
    if (conj.length === 0) {
        return ''
    }
    if (conj.length === 1) {
        return conj[0]
    }
    return '( ' + conj.map(conjToStr).join(' /\\ ') + ' )'
}

async function inferenceToClosed() {
    let allSteps = (await getEditorStateWithSyntaxTrees()).steps
    const allHyps = allSteps.filter(step => step.isHyp)
    const allHypsStr = allHyps.map(hyp => syntaxTreeToText(hyp.tree.root))
    const conj = conjToStr(buildConjArr(allHypsStr))
    await updateSteps(allSteps.map(step => ({
        label:step.label,
        type:step.isGoal?'g':'p',
        jstf:'',
        stmt:\`|- ( \${conj} -> \${syntaxTreeToText(step.tree.root)} )\`
    })))
    await unifyAll()
}

function getSingleSelectedStep(editorState) {
    if (editorState.selectedSteps.length !== 1) {
        exn('One step must be selected')
    }
    return getStepByLabel(editorState, editorState.selectedSteps[0])
}

function getSingleStepWithFragmentSelected(editorState) {
    const stepsWithFragmentSelected = editorState.steps.filter(step => step.fragId !== null)
    if (stepsWithFragmentSelected.length !== 1) {
        exn('One fragment must be selected.')
    }
    return stepsWithFragmentSelected[0]
}

const VARIABLE_HYPOTHESIS_PATTERN = ['(', '', '->', [anyVar(), '=', ''], ')']
function getVarExpr(step) {
    if (!step.isHyp) {
        return undefined
    }
    const matchRes = match(step.tree.root,VARIABLE_HYPOTHESIS_PATTERN)
    if (!matchRes) {
        return undefined
    }
    const [lp, ph, ar, [ v, eq, expr ], rp ] = matchRes
    return {varName:getVarName(v), expr}
}

async function eliminateVar(varHypStep) {
    const [lp, ph, ar, [v, eq, expr], rp] = matchExn(varHypStep.tree.root, VARIABLE_HYPOTHESIS_PATTERN)
    await substitute({what: getVarName(v), with_: syntaxTreeToText(expr)})
    await updateSteps([{label:varHypStep.label, type:'p'}])
    await mergeDuplicatedSteps()
}

async function eliminateVariables(varNames) {
    function isVarToEliminate(step) {
        const varExpr = getVarExpr(step)
        if (varExpr === undefined) {
            return false
        }
        if (varNames === undefined) {
            return true
        }
        return varNames.includes(varExpr.varName)
    }

    let state = await getEditorStateWithSyntaxTrees()
    let varHypStep = state.steps.find(isVarToEliminate)
    while (varHypStep) {
        await eliminateVar(varHypStep)
        state = await getEditorStateWithSyntaxTrees()
        varHypStep = state.steps.find(isVarToEliminate)
    }
}

async function eliminateVariablesInSelectedFragment() {
    await setContentIsHidden(true)

    let state = await getEditorState()
    const selectedStep = getSingleStepWithFragmentSelected(state)
    const fragmentTree = findFirstInTree(selectedStep.tree.root, node => node.id === selectedStep.fragId)
    const varsMap = (await getEditorStateWithSyntaxTrees()).steps
        .map(getVarExpr)
        .filter(v => v !== undefined)
        .reduce((map,varExpr) => ({...map, [varExpr.varName]:varExpr.expr}),{})

    function getAllVarsFromTree(tree) {
        const res = new Set()
        forEachNode(tree, node => {
            if (isVar(node)) {
                res.add(getVarName(node))
            } else if (node.isVar) {
                res.add(node.sym)
            }
        })
        return [...res]
    }

    const varsToProcess = getAllVarsFromTree(fragmentTree)
    const processedVars = new Set()
    while (varsToProcess.length > 0) {
        const [curVar] = varsToProcess.splice(varsToProcess.length-1, 1)
        if (!processedVars.has(curVar) && varsMap[curVar] !== undefined) {
            processedVars.add(curVar)
            varsToProcess.push(...(getAllVarsFromTree(varsMap[curVar])))
        }
    }
    const varsToEliminate = [...processedVars]
    await eliminateVariables(varsToEliminate)

    await setContentIsHidden(false)
}

function mapTree(tree, mapper) {
    let mappedTree = mapper(tree)
    if (mappedTree !== undefined) {
        return mappedTree
    }
    if (tree.nodeType === 'expr') {
        return {...tree, children:tree.children.map(ch => mapTree(ch, mapper))}
    } else {
        return {...tree}
    }
}

function replaceSubtreeWithAnotherSubtree(tree, treeToReplace, treeToReplaceWith) {
    return mapTree(tree, node => {
        if (node === treeToReplace) {
            return treeToReplaceWith
        } else {
            return undefined
        }
    })
}

function findFirstInTree(tree, nodePredicate) {
    if (nodePredicate(tree)) {
        return tree
    }
    if (tree.nodeType === 'expr') {
        for (let i = 0; i < tree.children.length; i++) {
            const found = findFirstInTree(tree.children[i], nodePredicate)
            if (found) {
                return found
            }
        }
    }
    return undefined
}

function forEachNode(tree, nodeConsumer) {
    nodeConsumer(tree)
    if (tree.nodeType === 'expr') {
        tree.children.map(ch => forEachNode(ch, nodeConsumer))
    }
}

function treeEq(a,b) {
    if (a.nodeType !== b.nodeType) {
        return false
    }
    if (a.nodeType === 'sym') {
        return a.sym === b.sym && a.isVar === b.isVar
    }
    if (a.children.length !== b.children.length) {
        return false
    }
    for (let i = 0; i < a.children.length; i++) {
        if (!treeEq(a.children[i],b.children[i])) {
            return false
        }
    }
    return true
}

async function runSequenceOfModificationsForStep({
     stepLabel,
     findSubtreeToModify,
     makeNewSubtreeAndVariables,
}) {
    const labelOfStepToModify = stepLabel
    let state = await getEditorStateWithSyntaxTrees()
    let stepToModify = getStepByLabel(state, labelOfStepToModify)
    let [lp, wffVar, ar, [v, eq, exprToModify]] = matchExn(
        stepToModify.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
    )
    let subtreeToModify = await findSubtreeToModify(exprToModify)
    while (subtreeToModify !== undefined) {
        console.log('subtreeToModify', syntaxTreeToText(subtreeToModify))
        const {newSubtree, variables} = (await makeNewSubtreeAndVariables(exprToModify, subtreeToModify))
        console.log('newSubtree', syntaxTreeToText(newSubtree))
        const newStepTree = replaceSubtreeWithAnotherSubtree(stepToModify.tree.root, subtreeToModify, newSubtree)
        const [newStepLabel] = await addSteps({
            atIdx: state.steps.length,
            steps: [
                {type: 'p', stmt: \`|- \${syntaxTreeToText(newStepTree)}\`},
                ...(variables??[]).map(([varType, varName, varExprTree]) => {
                    return {
                        type: 'h',
                        stmt: \`|- ( \${syntaxTreeToText(wffVar)} -> \${varName} = \${syntaxTreeToText(varExprTree)} )\`
                    }
                })
            ],
            vars: (variables??[]).map(([varType, varName, varExprTree]) => [varType, varName])
        })
        state = await prove({step: newStepLabel, use: [stepToModify.label]})
        if (!isStepProved(state, newStepLabel)) {
            exn(\`Cannot not prove the step with the label '\${newStepLabel}'.\`)
        }
        stepToModify = getStepByLabel(state, newStepLabel)
        let [lp2, wffVar2, ar2, [v2, eq2, exprToModify2]] = matchExn(
            stepToModify.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
        )
        exprToModify = exprToModify2
        subtreeToModify = await findSubtreeToModify(exprToModify)
    }
    return stepToModify.label
}

async function getNewVarName(prefix) {
    if (prefix !== '' && await getTokenType(prefix) === null) {
        return prefix
    }
    let cnt = 1
    let varName = prefix + '_' + cnt
    let tokenType = await getTokenType(varName)
    while (tokenType !== null) {
        cnt++
        varName = prefix + '_' + cnt
        tokenType = await getTokenType(varName)
    }
    return varName
}

async function runSequenceOfModificationsForFragment({
     findSubtreeToModify,
     makeNewSubtreeAndVariables,
}) {
    await setContentIsHidden(true)

    let state = await getEditorState()
    const initialStep = getSingleStepWithFragmentSelected(state)
    const fragmentTree = findFirstInTree(initialStep.tree.root, node => node.id === initialStep.fragId)
    if (await findSubtreeToModify(fragmentTree) === undefined) {
        await setContentIsHidden(false)
        return
    }
    const [lp, wffVar, ar, initExpr, rp] = matchExn(
        initialStep.tree.root, ['(', varOfType('wff'), '->', '', ')']
    )
    const tempVarName = await getNewVarName('tmp')
    const [hypLabel] = await addSteps({
        steps: [
            {
                type: 'h',
                stmt: \`|- ( \${syntaxTreeToText(wffVar)} -> \${tempVarName} = \${syntaxTreeToText(fragmentTree)} )\`
            },
        ],
        vars: [[fragmentTree.exprType, tempVarName]]
    })
    const finalVarStepLabel = await runSequenceOfModificationsForStep({
        stepLabel:hypLabel, findSubtreeToModify, makeNewSubtreeAndVariables
    })
    state = await getEditorStateWithSyntaxTrees()
    const finalVarStep = getStepByLabel(state, finalVarStepLabel)
    const [lp1, wffVar1, arr1, [tmpVar, eq, finalExpr], rp1] = matchExn(
        finalVarStep.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
    )
    await eliminateVariables([tempVarName])
    state = await getEditorState()
    const finalStepTree = replaceSubtreeWithAnotherSubtree(initialStep.tree.root, fragmentTree, finalExpr)
    const [finalStepLabel] = await addSteps({
        atIdx: state.steps.length,
        steps: [{type: 'p', stmt: \`|- \${syntaxTreeToText(finalStepTree)}\`, isBkm:true}],
    })
    await prove({step:finalStepLabel, use:[initialStep.label, finalVarStep.label]})

    await setContentIsHidden(false)
}

function isTerm(tree,allowedConsts) {
    function isAllowedNode(node) {
        return !isSymbol(node) || node.isVar || allowedConsts.includes(node.sym) || NUMBER_CONSTANTS.includes(node.sym)
    }
    function isForbiddenNode(node) {
        return !isAllowedNode(node)
    }
    function endsWithDigits(str) {
        return str.match(/\d+$/) !== null
    }

    return !allowedConsts.includes(syntaxTreeToText(tree))
        && !(isVar(tree) && endsWithDigits(getVarName(tree)))
        && !(tree.isVar && endsWithDigits(tree.sym))
        && findFirstInTree(tree, isForbiddenNode) === undefined
}

function mulPowers(pow1,pow2) {
    if (pow1.length === 0) {
        return pow2
    }
    if (pow2.length === 0) {
        return pow1
    }
    const res = []
    for (const pow1Elem of pow1) {
        const pow2Elem = pow2.find(([varName,powNum]) => varName === pow1Elem[0])
        if (pow2Elem) {
            res.push([pow1Elem[0],pow1Elem[1]+pow2Elem[1]])
        } else {
            res.push(pow1Elem)
        }
    }
    for (const pow2Elem of pow2) {
        const resElem = res.find(([varName,powNum]) => varName === pow2Elem[0])
        if (!resElem) {
            res.push(pow2Elem)
        }
    }
    return res
}

function expPowers(pow,exp) {
    if (pow.length === 0) {
        return pow
    }
    return pow.map(([varName,powNum]) => [varName,powNum*exp])
}

function varNameToPowers(varName) {
    function polynomVarNameToPowers(varName) {
        return [...varName.matchAll(/([a-zA-Z]+)(\d+)/g)].map(match => [match[1].toLowerCase(), parseInt(match[2])])
    }
    const res = polynomVarNameToPowers(varName)
    if (res.length !== 0) {
        return res
    }
    if (varName.startsWith("_")) {
        return []
    }
    return [[varName.toLowerCase(),1]]
}

function collectPowers(tree) {
    if (isVar(tree)) {
        return varNameToPowers(getVarName(tree))
    }
    if (isSymbol(tree)) {
        return []
    }
    let matchRes = match(tree,['-u',anyVar()])
    if (matchRes) {
        const [neg, v] = matchRes
        return varNameToPowers(getVarName(v))
    }
    matchRes = match(tree,['-u',[anySym(NUMBER_CONSTANTS)]])
    if (matchRes) {
        return []
    }
    matchRes = match(tree,['(', '', ['^'], '', ')'])
    if (matchRes) {
        const [lp,lop,pow,rop,rp] = matchRes
        return expPowers(collectPowers(lop), parseInt(syntaxTreeToText(rop)))
    }
    matchRes = match(tree,['-u', ['(', '', ['^'], '', ')']])
    if (matchRes) {
        const [m,[lp,lop,pow,rop,rp]] = matchRes
        return expPowers(collectPowers(lop), parseInt(syntaxTreeToText(rop)))
    }
    matchRes = match(tree,['(', '', ['x.'], '', ')'])
    if (matchRes) {
        const [lp,lop,mul,rop,rp] = matchRes
        return mulPowers(collectPowers(lop), collectPowers(rop))
    }
    matchRes = match(tree,['-u', ['(', '', ['x.'], '', ')']])
    if (matchRes) {
        const [m,[lp,lop,mul,rop,rp]] = matchRes
        return mulPowers(collectPowers(lop), collectPowers(rop))
    }
    console.log('tree to collect powers from', tree)
    exn('Cannot collect powers. See above.')
}

function comparePowersByVarNames(pow1,pow2) {
    if (pow1[0] < pow2[0]) {
        return -1
    }
    if (pow1[0] > pow2[0]) {
        return 1
    }
    return 0
}

function getBasePolynomVarNameForTerm(term) {
    if (findFirstInTree(term, n => isVar(n)) === undefined) {
        return ''
    } else {
        const powers = collectPowers(term).sort(comparePowersByVarNames)
        return powers.map(([varName,powNum]) => varName.toLowerCase()+powNum).join('')
    }
}

async function getPolynomVarNameForTerm(term) {
    const baseVarName = getBasePolynomVarNameForTerm(term)
    return getNewVarName(baseVarName)
}

async function introduceVariables(allowedConsts) {
    function isForbiddenToBeTerm(tree,expr) {
        return undefined !== findFirstInTree(tree, node => {
            const matchResult = match(node, ['(', '', ['^'], '', ')'])
            if (matchResult !== undefined) {
                const [lp, a, exp, b, rp] = matchResult
                return b.id === expr.id || (b.children?.length === 1 && b.children?.[0]?.id === expr.id)
            }
            return false
        })
    }
    await runSequenceOfModificationsForFragment({
        findSubtreeToModify: async tree => {
            return findFirstInTree(tree, node => {
                return isTerm(node, allowedConsts) && !isForbiddenToBeTerm(tree, node)
            })
        },
        makeNewSubtreeAndVariables: async (tree, subtree) => {
            const varName = await getPolynomVarNameForTerm(subtree)
            return {
                newSubtree: makeConst(varName),
                variables: [[subtree.exprType, varName, subtree]]
            }
        },
    })
}

async function introduceVariablesSum() {
    await introduceVariables(['-u', '(', ')', '^', 'x.'])
}

async function introduceVariablesMul() {
    await introduceVariables(['-u', '(', ')', '^'])
}

function makeDistributableLeftPattern(insideOperator,outsideOperator) {
    return ['(', '', [outsideOperator], ['(', '', [insideOperator], '', ')'], ')']
}
function makeDistributableRightPattern(insideOperator,outsideOperator) {
    return ['(', ['(', '', [insideOperator], '', ')'], [outsideOperator], '', ')']
}

function applyDistribution(tree) {
    let matchResult = match(tree,['(', '', '', ['(', '', '', '', ')'], ')'])
    if (matchResult) {
        const [LP,a,oo,[lp,b,io,c,rp],RP] = matchResult
        const aS = syntaxTreeToText(a)
        const bS = syntaxTreeToText(b)
        const cS = syntaxTreeToText(c)
        const ioS = syntaxTreeToText(io)
        const ooS = syntaxTreeToText(oo)
        return makeConst(\`( ( \${aS} \${ooS} \${bS} ) \${ioS} ( \${aS} \${ooS} \${cS} ) )\`)
    }
    const [LP,[lp,b,io,c,rp],oo,a,RP] = matchExn(tree,['(', ['(', '', '', '', ')'], '', '', ')'])
    const aS = syntaxTreeToText(a)
    const bS = syntaxTreeToText(b)
    const cS = syntaxTreeToText(c)
    const ioS = syntaxTreeToText(io)
    const ooS = syntaxTreeToText(oo)
    return makeConst(\`( ( \${bS} \${ooS} \${aS} ) \${ioS} ( \${cS} \${ooS} \${aS} ) )\`)
}

async function distribute(isDistributable) {
    await runSequenceOfModificationsForFragment({
        findSubtreeToModify: async tree => {
            return findFirstInTree(tree, isDistributable)
        },
        makeNewSubtreeAndVariables: async (tree, subtree) => {
            return {newSubtree: applyDistribution(subtree)}
        },
    })
}

async function distributeMul() {
    const patterns = [makeDistributableLeftPattern('+', 'x.'), makeDistributableRightPattern('+', 'x.')]
    await distribute(tree => matchesAny(tree, patterns))
}

async function distributeExp() {
    let pattern = makeDistributableRightPattern('x.', '^');
    await distribute(tree => match(tree, pattern) !== undefined)
}

function getAllSymbols(tree) {
    const res = []
    forEachNode(tree, node => {
        if (node.isVar || NUMBER_CONSTANTS.includes(node.sym)) {
            res.push(node.sym)
        }
    })
    return res
}

function getFirstMisplacedSymbol(expr, sortedSyms, idx) {
    idx = idx??0
    if (idx === sortedSyms.length) {
        return {}
    }

    const curSym = sortedSyms[idx]
    function isCurSymbolEqNode(node) {
        return node.children?.length === 1 && node.children[0].sym === curSym
    }
    const symMatcher = makeMatcher(isCurSymbolEqNode)

    const matchResult = match(expr, ['(', symMatcher, '', '', ')'])
    if (matchResult === undefined) {
        return {misplacedSym:curSym,unorderedTree:expr}
    } else {
        const [lp, head, op, tail, rp] = matchResult
        return getFirstMisplacedSymbol(tail, sortedSyms, idx+1)
    }
}

function getSubtreeToSwap(tree, sym) {
    return findFirstInTree(tree, node => {
        const matchResult = match(node, ['(', '', '', '', ')'])
        if (matchResult === undefined) {
            return false
        }
        const [lp, leftSubtree, op, rightSubtree, rp] = matchResult
        return !getAllSymbols(leftSubtree).includes(sym) && getAllSymbols(rightSubtree).includes(sym)
    })
}

function swapTree(tree) {
    const [lp, leftSubtree, op, rightSubtree, rp] = matchExn(tree, ['(', '', '', '', ')'])
    return makeConst(\`( \${syntaxTreeToText(rightSubtree)} \${syntaxTreeToText(op)} \${syntaxTreeToText(leftSubtree)} )\`)
}

function getSubtreeToAssoc(tree) {
    return findFirstInTree(tree, node => {
        return match(node, ['(', ['(', '', '', '', ')'], '', '', ')']) !== undefined
    })
}

function assocTree(tree) {
    const [olp, [ilp, a, o1, b, irp], o2, c, orp] = match(tree, ['(', ['(', '', '', '', ')'], '', '', ')'])
    const aS = syntaxTreeToText(a)
    const bS = syntaxTreeToText(b)
    const cS = syntaxTreeToText(c)
    const o1S = syntaxTreeToText(o1)
    const o2S = syntaxTreeToText(o2)
    return makeConst(\`( \${aS} \${o1S} ( \${bS} \${o2S} \${cS} ) )\`)
}

function stableSort(arr, cmpFn) {
    const res = [...arr]
    for (let i = 0; i < arr.length-1; i++) {
        for (let j = arr.length-2; j >= i; j--) {
            if (cmpFn(res[j], res[j+1]) > 0) {
                const tmp = res[j]
                res[j] = res[j+1]
                res[j+1] = tmp
            }
        }
    }
    return res
}

function getSubtreeToSort(tree, symCmpFn) {
    let allSyms = getAllSymbols(tree)
    console.log('allSyms', allSyms)
    const sortedSyms = stableSort(allSyms, symCmpFn)
    console.log('sortedSyms', sortedSyms)
    const {misplacedSym,unorderedTree} = getFirstMisplacedSymbol(tree, sortedSyms)
    console.log('misplacedSym', misplacedSym)
    if (misplacedSym === undefined) {
        return {}
    }
    const subtreeToSwap = getSubtreeToSwap(unorderedTree, misplacedSym)
    if (subtreeToSwap !== undefined) {
        return {subtreeToSwap}
    }
    return {subtreeToAssoc:getSubtreeToAssoc(unorderedTree)}
}

async function sortSymbols(symCmpFn) {
    await runSequenceOfModificationsForFragment({
        findSubtreeToModify: async tree => {
            const {subtreeToSwap,subtreeToAssoc} = getSubtreeToSort(tree, symCmpFn)
            return subtreeToSwap??subtreeToAssoc
        },
        makeNewSubtreeAndVariables: async (tree, subtree) => {
            const {subtreeToSwap,subtreeToAssoc} = getSubtreeToSort(tree, symCmpFn)
            if (subtreeToSwap !== undefined) {
                console.log('subtreeToSwap', syntaxTreeToText(subtreeToSwap))
                return {newSubtree: swapTree(subtree)}
            } else {
                console.log('subtreeToAssoc', syntaxTreeToText(subtreeToAssoc))
                return {newSubtree: assocTree(subtree)}
            }
        },
    })
}

async function sortRegularSymbols() {
    await sortSymbols((a, b) => {
        if (a < b) {
            return -1
        }
        if (a === b) {
            return 0
        }
        return 1
    })
}

async function sortPolynomialVariables() {
    function polynomVarNameToBaseName(varName) {
        const powers = varNameToPowers(varName)
        return powers.map(([varName,powNum]) => varName.toLowerCase()+powNum).join('')
    }
    function comparePolynomVarNamesByBaseName(a,b) {
        const aBase = polynomVarNameToBaseName(a)
        const bBase = polynomVarNameToBaseName(b)
        let cmpRes
        if (aBase < bBase) {
            cmpRes = -1
        } else if (aBase === bBase) {
            cmpRes = 0
        } else {
            cmpRes = 1
        }
        return -cmpRes
    }
    await sortSymbols(comparePolynomVarNamesByBaseName)
}

async function combineExponents() {
    function combineExponentsPriv(tree) {
        let matchResult = match(tree, ['(', ['(', '', ['^'], '', ')'], ['x.'], ['(', '', ['^'], '', ')'], ')'])
        if (matchResult !== undefined) {
            const [LP, [lp1, a, exp1, b, rp1], mul, [lp2, c, exp2, d, rp2], RP] = matchResult
            if (!treeEq(a,c)) {
                return undefined
            }
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            const dS = syntaxTreeToText(d)
            return makeConst(\`( \${aS} ^ ( \${bS} + \${dS} ) )\`)
        }
        matchResult = match(tree, ['(', ['(', '', ['^'], '', ')'], ['^'], '', ')'])
        if (matchResult !== undefined) {
            const [LP, [lp, a, exp1, b, rp], exp2, c, RP] = matchResult
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            const cS = syntaxTreeToText(c)
            return makeConst(\`( \${aS} ^ ( \${bS} x. \${cS} ) )\`)
        }
        matchResult = match(tree, ['(', ['(', '', ['^'], '', ')'], ['x.'], '', ')'])
        if (matchResult !== undefined) {
            const [LP, [lp, a, exp1, b, rp], mul, c, RP] = matchResult
            if (!treeEq(a,c)) {
                return undefined
            }
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            return makeConst(\`( \${aS} ^ ( \${bS} + 1 ) )\`)
        }
        matchResult = match(tree, ['(', '', ['x.'], ['(', '', ['^'], '', ')'], ')'])
        if (matchResult !== undefined) {
            const [LP, c, mul, [lp, a, exp1, b, rp], RP] = matchResult
            if (!treeEq(a,c)) {
                return undefined
            }
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            const cS = syntaxTreeToText(c)
            return makeConst(\`( ( \${aS} ^ \${bS} ) x. \${cS} )\`)
        }
        return undefined
    }
    await runSequenceOfModificationsForFragment({
        findSubtreeToModify: async tree => {
            return findFirstInTree(tree, node => combineExponentsPriv(node) !== undefined)
        },
        makeNewSubtreeAndVariables: async (tree, subtree) => {
            return {newSubtree: combineExponentsPriv(subtree)}
        },
    })
}

const m = {
    getEditorState,
    buildSyntaxTrees,
    getNewAssertionsFromEditor,
    inferenceToDeduction,
    deductionToInference,
    inferenceToClosed,
    eliminateAllVariables: eliminateVariables,
    eliminateVariablesInSelectedFragment,
    introduceVariablesSum,
    introduceVariablesMul,
    distributeMul,
    distributeExp,
    sortRegularSymbols,
    sortPolynomialVariables,
    combineExponents,
}
`
