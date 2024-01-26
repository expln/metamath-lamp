let setMmExampleMacros = `
const commonFrameParams = {
    minDist:null,
    maxDist:null,
    allowNewDisjointsForExistingVariables:false,
    allowNewSteps:true,
    allowNewVariables:false,
    statementLengthRestriction:'No',
}

function makeFrmParams(frames, matchers) {
    return {
        ...commonFrameParams,
        frames,
        matches:matchers??null
    }
}

function code(code) {
    return String.fromCharCode(code)
}

const frms = {}

const FPR_ELEM_OF = 'ELEM_OF';
frms[FPR_ELEM_OF] = [
    makeFrmParams([
        '0cn','ax-1cn','2cn','3cn','4cn','5cn','6cn','7cn','8cn','9cn',
        '0nn0','1nn0','2nn0','3nn0','4nn0','5nn0','6nn0','7nn0','8nn0','9nn0','10nn0',
        '1nn','2nn','3nn','4nn','5nn','6nn','7nn','8nn','9nn','10nn',
        'addcld','qre',
        'expcld', 'reexpcld', 'nnexpcld', 'nn0expcld', 'rpexpcld',
        'jca', 'jca31', 'jca32', '3jca', 'eqeltrd', 'sqcld', 'nn0cnd','recnd',
        'mulcld', 'remulcld', 'nnmulcld', 'nn0mulcld', 'zmulcld', 'rpmulcld', 'xmulcld',
        'a1i','nnred','qcn','negcld','expcld','nn0addcld'
    ]),
    makeFrmParams(['syl'], [
        {
            hyps: [
                {idx:0, pat:'|- ( ph -> X e. A )'},
                {idx:1, pat:'|- ( X e. A -> X e. B )'},
            ],
            res:'|- ( ph -> X e. B )'
        }
    ]),
];

const FPR_EQUALS = 'FPR_EQUALS';
frms[FPR_EQUALS] = [
    FPR_ELEM_OF,
    makeFrmParams([
        'oveq1d', 'oveq2d','eqtrd','eqtr2d','eqtr3d', 'eqtr4d', 'eqcomd', 'adddid', 'adddird', 'addcomd', 'addassd', 'negsubd',
        'addid2d','addid1d','negeqd','mulneg1d','mulexpd','mulcomd','mulassd','negidd','sqnegd','mulid1d','mulid2d', 'mulm1d',
        'sq0','sq1','sq2','sq3',
        'expp1d','expaddd','expmuld','mulid2d','syl5reqr','mulcomli','exp1d','2timesd','mulneg2d',
        'df-2','df-3','df-4','df-5','df-6','df-7','df-8','df-9',
        '0p1e1','1p0e1','1p1e2','2p1e3','2p2e4','1p2e3','2t2e4','3p1e4','4p1e5','5p1e6','6p1e7','7p1e8','8p1e9','3p2e5','3p3e6',
        '4p2e6','4p3e7','4p4e8','5p2e7','5p3e8','5p4e9','6p2e8','6p3e9','7p2e9',
        '2m1e1','1e2m1','3m1e2','4m1e3','5m1e4','6m1e5','7m1e6','8m1e7','9m1e8',
        '1t1e1','2t1e2','2t2e4','3t1e3','3t2e6','3t3e9','4t2e8','2t0e0',
    ]),
    makeFrmParams(['syl'], [
        {
            hyps: [{idx:0, pat:[!@#]|- ( ph -> ( X e. A /{!@#}{code(92)} Y e. B ) )[!@#]},],
            res:'|- ( ph -> ( ( X + Y ) ^ 3 ) = Z )'
        },
        {
            hyps: [{idx:0, pat:[!@#]|- ( ph -> X e. A )[!@#]},],
            res:'|- ( ph -> Y = ( X ^ N ) )'
        },
    ]),
]

const FPR_RUN_SEQ_FOR_STEP = 'RUN_SEQ_FOR_STEP';
frms[FPR_RUN_SEQ_FOR_STEP] = []

const FPR_RUN_SEQ_FOR_FRAG = 'RUN_SEQ_FOR_FRAG';
frms[FPR_RUN_SEQ_FOR_FRAG] = [FPR_EQUALS]

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

api.setLogApiCallsToConsole(true)

async function showInfoMsg(msg) {
    getResponse(await api.showInfoMsg(String(msg)))
}

async function showErrMsg(msg) {
    getResponse(await api.showErrMsg(String(msg)))
}

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
    exn([!@#]Cannot get an editor state with syntax trees.[!@#])
}

async function allIsProved() {
    let state = await getEditorState()
    if (state.steps.every(step => step.isHyp || step.status !== null)) {
        return state.steps.every(step => step.isHyp || step.status === 'v')
    }
    state = await unifyAll()
    return state.steps.every(step => step.isHyp || step.status === 'v')
}

async function updateSteps(steps) {
    return getResponse(await api.editor.updateSteps(steps))
}

async function deleteSteps(labels) {
    return getResponse(await api.editor.deleteSteps(labels))
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
        || tree.nodeType === "expr" && tree.children.length === 1 && tree.children[0].nodeType === "sym"
}

function getSymbol(tree) {
    if (!isSymbol(tree)) {
        console.log('tree to get symbol from', tree)
        exn('Not a symbol')
    }
    if (tree.nodeType === "sym") {
        return tree.sym
    } else {
        return tree.children[0].sym
    }
}

function isVar(tree) {
    return tree.nodeType === "expr" && tree.children.length === 1 && tree.children[0].isVar
}

function getVarName(tree) {
    if (!isVar(tree)) {
        console.log('tree to get var name from', tree)
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

function isArr(x) {
    return x !== undefined && x !== null && Array.isArray(x)
}

function isStr(x) {
    return x !== undefined && x !== null && typeof x === 'string'
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
            if (pat.matcherType === MATCHER_ANY_SYMBOL && isSymbol(ch) && pat.allowedSymbols.includes(getSymbol(ch))) {
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
            if (pat === '' || isSymbol(ch) && pat === getSymbol(ch)) {
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
        exn([!@#]the pattern doesn't match the tree (see above)[!@#])
    }
    return matchResult
}

function matchesAny(tree, patterns) {
    return patterns.some(pattern => match(tree,pattern) !== undefined)
}

async function getAllFrmsFromEditor() {
    return [...new Set((await getEditorState()).steps.filter(step => step.jstf).map(step => step.jstf.asrt))]
}

function arDiff(a,...bs) {
    for (const b of bs) {
        const bSet = new Set(b)
        a = a.filter(e => !bSet.has(e))
    }
    return a
}

function arIntersect(a,...bs) {
    for (const b of bs) {
        const bSet = new Set(b)
        a = a.filter(e => bSet.has(e))
    }
    return a
}

function arUnion(a,...bs) {
    return [...new Set(a.concat(...bs))]
}

async function updateFrms() {
    frms.editor = await getAllFrmsFromEditor()
}

function syntaxTreeToText(node) {
    if (isSymbol(node)) {
        return getSymbol(node)
    } else {
        return node.children.map(syntaxTreeToText).join(' ')
    }
}

function isGoalProved(editorState) {
    let goalSteps = editorState.steps.filter(step => step.isGoal)
    if (goalSteps.length !== 1) {
        exn([!@#]Cannot find the goal step.[!@#])
    } else {
        return goalSteps[0].status === "v"
    }
}

function isStepProved(editorState, label) {
    let steps = editorState.steps.filter(step => step.label === label)
    if (steps.length !== 1) {
        exn([!@#]Cannot find the step with the label '{!@#}{label}'[!@#])
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
    exn([!@#]Cannot find the step with the label '{!@#}{label}'[!@#])
}

function getStepByLabel(editorState, label) {
    return editorState.steps[getStepIdx(editorState, label)]
}

function cannotResolveFrmParamsExn(frmParams) {
    console.log('frmParams', frmParams)
    exn('Cannot resolve frmParams (see the actual value in the console).')
}

function resolveFrmParams(frmParams, path) {
    path = path??''
    if (isObj(frmParams)) {
        return [{...frmParams, tag:path}]
    }
    if (isStr(frmParams)) {
        let res = frms[frmParams]
        if (res === undefined) {
            cannotResolveFrmParamsExn(frmParams)
        }
        return resolveFrmParams(res, path + "." + frmParams)
    }
    if (!isArr(frmParams)) {
        cannotResolveFrmParamsExn(frmParams)
    }
    return frmParams.flatMap(p => resolveFrmParams(p, path))
}

async function prove({
                         stepToProve, stepsToDeriveFrom,
                         frmParams,
                         maxSearchDepth, debugLevel
}) {
    let editorState = await getEditorState()
    let allSteps = editorState.steps
    let allHyps = allSteps.filter(s => s.isHyp).map(s => s.label)
    function getPrevLabel(label) {
        for (let i = 1; i < allSteps.length; i++) {
            if (allSteps[i].label === label) {
                return allSteps[i-1].label
            }
        }
        exn([!@#]Cannot find previous step for '{!@#}{label}'[!@#])
    }
    stepToProve = (stepToProve??'last') === 'last' ? allSteps[allSteps.length-1].label : stepToProve
    stepsToDeriveFrom = (stepsToDeriveFrom??[]).map(label => label === 'prev' ? getPrevLabel(stepToProve) : label)
    stepsToDeriveFrom = [...allHyps, ...stepsToDeriveFrom]
    debugLevel = debugLevel??0
    maxSearchDepth = maxSearchDepth??20

    const frameParameters = resolveFrmParams(frmParams).map(p => ({...p, stepsToDeriveFrom}))

    async function doProve(debugLevel) {
        return getResponse(await api.editor.proveBottomUp({
            tags:frmParams,
            stepToProve: stepToProve,
            maxSearchDepth,
            debugLevel,
            selectFirstFoundProof: debugLevel === 0,
            frameParameters,
            delayBeforeStartMs:500,
        }))
    }

    const proved = await doProve(debugLevel)
    if (!proved && debugLevel === 0) {
        await doProve(1)
    }
    return await getEditorState()
}

async function proveSelected({frmParams, maxSearchDepth, debugLevel}) {
    const state = await getEditorState()
    if (state.selectedSteps.length === 0) {
        if (state.steps.length === 0) {
            return
        } else {
            const stepToProve = state.steps[state.steps.length-1].label
            const stepsToDeriveFrom = state.steps.length > 1 ? [state.steps[state.steps.length-2].label] : []
            await prove({
                stepToProve, stepsToDeriveFrom,
                frmParams,
                maxSearchDepth, debugLevel
            })
        }
    } else {
        const [stepToProve, ...stepsToDeriveFrom] =
            state.steps.filter(step => state.selectedSteps.includes(step.label)).map(step => step.label).reverse()
        await prove({
            stepToProve, stepsToDeriveFrom,
            frmParams,
            maxSearchDepth, debugLevel
        })
    }
}

async function deductionToInference() {
    let allSteps = (await getEditorStateWithSyntaxTrees()).steps
    const stepsToUpdate = []
    const stepsToAdd = []
    for (const step of allSteps) {
        const [lp, ph, arr, expr, rp] = matchExn(step.tree.root, ['(', '', '->', '', ')'])
        const exprStr = syntaxTreeToText(expr)
        stepsToAdd.push({stmt:[!@#]|- ( T. -> {!@#}{exprStr} )[!@#]})
        stepsToUpdate.push({label:step.label, jstf:'', stmt:[!@#]|- {!@#}{exprStr}[!@#]})
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
    return '( ' + conj.map(conjToStr).join([!@#] /{!@#}{code(92)} [!@#]) + ' )'
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
        stmt:[!@#]|- ( {!@#}{conj} -> {!@#}{syntaxTreeToText(step.tree.root)} )[!@#]
    })))
    await unifyAll()
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
    const renames = await mergeDuplicatedSteps()
    await deleteSteps([varHypStep.label])
    const stepsToDeleteJstf = (await getEditorState()).steps
        .filter(step => step.jstf?.args?.includes(varHypStep.label))
        .map(step => step.label)
    if (stepsToDeleteJstf.length > 0) {
        await updateSteps(stepsToDeleteJstf.map(label => ({label, jstf:''})))
    }
    if (!(await allIsProved())) {
        exn('There are unproved steps.')
    }
    return renames
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
    let allRenames = []
    while (varHypStep) {
        const renames = await eliminateVar(varHypStep)
        allRenames.push(...renames)
        state = await getEditorStateWithSyntaxTrees()
        varHypStep = state.steps.find(isVarToEliminate)
    }
    return allRenames
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

async function runSequenceOfModificationsForStep({stepLabel, modifySubtree, frmParams}) {
    const labelOfStepToModify = stepLabel
    let state = await getEditorStateWithSyntaxTrees()
    let stepToModify = getStepByLabel(state, labelOfStepToModify)
    let [lp, wffVar, ar, [v, eq, exprToModify]] = matchExn(
        stepToModify.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
    )
    let {subtreeToModify, newSubtree, newVariables} = (await modifySubtree(exprToModify))??{}
    while (subtreeToModify !== undefined) {
        console.log('subtreeToModify', syntaxTreeToText(subtreeToModify))
        console.log('newSubtree', syntaxTreeToText(newSubtree))
        console.log('newVariables', newVariables)
        const newStepTree = replaceSubtreeWithAnotherSubtree(stepToModify.tree.root, subtreeToModify, newSubtree)
        const [newStepLabel] = await addSteps({
            atIdx: state.steps.length,
            steps: [
                {type: 'p', stmt: [!@#]|- {!@#}{syntaxTreeToText(newStepTree)}[!@#]},
                ...(newVariables??[]).map(([varType, varName, varExprTree]) => {
                    return {
                        type: 'h',
                        stmt: [!@#]|- ( {!@#}{syntaxTreeToText(wffVar)} -> {!@#}{varName} = {!@#}{syntaxTreeToText(varExprTree)} )[!@#]
                    }
                })
            ],
            vars: (newVariables??[]).map(([varType, varName, varExprTree]) => [varType, varName])
        })
        state = await prove({
            stepToProve: newStepLabel, stepsToDeriveFrom: [stepToModify.label],
            frmParams: [FPR_RUN_SEQ_FOR_STEP, frmParams]
        })
        if (!isStepProved(state, newStepLabel)) {
            exn([!@#]Cannot not prove the step with the label '{!@#}{newStepLabel}'.[!@#])
        }
        stepToModify = getStepByLabel(state, newStepLabel)
        let [lp2, wffVar2, ar2, [v2, eq2, exprToModify2]] = matchExn(
            stepToModify.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
        )
        exprToModify = exprToModify2
        let modifiedSubtreeParams = (await modifySubtree(exprToModify))??{}
        subtreeToModify = modifiedSubtreeParams.subtreeToModify
        newSubtree = modifiedSubtreeParams.newSubtree
        newVariables = modifiedSubtreeParams.newVariables
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

function rename(renaming, label) {
    for (const [oldName, newName] of renaming) {
        if (label === oldName) {
            label = newName
        }
    }
    return label
}

async function runSequenceOfModificationsForFragment({modifySubtree, frmParams}) {
    await setContentIsHidden(true)

    let state = await getEditorState()
    const initialStep = getSingleStepWithFragmentSelected(state)
    const fragmentTree = findFirstInTree(initialStep.tree.root, node => node.id === initialStep.fragId)
    const {subtreeToModify} = (await modifySubtree(fragmentTree))??{}
    if (subtreeToModify === undefined) {
        await setContentIsHidden(false)
        return
    }
    const [lp, wffVar, ar, initExpr, rp] = matchExn(initialStep.tree.root, ['(', varOfType('wff'), '->', '', ')'])
    const tempVarName = await getNewVarName('tmp')
    const [hypLabel] = await addSteps({
        steps: [
            {
                type: 'h',
                stmt: [!@#]|- ( {!@#}{syntaxTreeToText(wffVar)} -> {!@#}{tempVarName} = {!@#}{syntaxTreeToText(fragmentTree)} )[!@#]
            },
        ],
        vars: [[fragmentTree.exprType, tempVarName]]
    })
    const finalVarStepLabel = await runSequenceOfModificationsForStep({stepLabel:hypLabel, modifySubtree, frmParams})
    state = await getEditorStateWithSyntaxTrees()
    const finalVarStep = getStepByLabel(state, finalVarStepLabel)
    const [lp1, wffVar1, arr1, [tmpVar, eq, finalExpr], rp1] = matchExn(
        finalVarStep.tree.root, ['(', varOfType('wff'), '->', [anyVar(), '=', ''], ')']
    )
    const renaming = await eliminateVariables([tempVarName])
    state = await getEditorState()
    const finalStepTree = replaceSubtreeWithAnotherSubtree(initialStep.tree.root, fragmentTree, finalExpr)
    const [finalStepLabel] = await addSteps({
        atIdx: state.steps.length,
        steps: [{type: 'p', stmt: [!@#]|- {!@#}{syntaxTreeToText(finalStepTree)}[!@#], isBkm:true}],
    })
    await prove({
        stepToProve:finalStepLabel,
        stepsToDeriveFrom:[rename(renaming,initialStep.label), rename(renaming,finalVarStep.label)],
        frmParams:FPR_RUN_SEQ_FOR_FRAG
    })

    await setContentIsHidden(false)
}

const ENDS_WITH_DIGITS_PATTERN = new RegExp([!@#]{!@#}{code(92)}d+{!@#}[!@#])
function isTerm(tree,allowedConsts) {
    function isAllowedNode(node) {
        return node.nodeType !== "sym" || node.isVar || allowedConsts.includes(node.sym) || NUMBER_CONSTANTS.includes(node.sym)
    }
    function isForbiddenNode(node) {
        return !isAllowedNode(node)
    }
    function endsWithDigits(str) {
        return str.match(ENDS_WITH_DIGITS_PATTERN) !== null
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

const VAR_NAME_TO_POWERS_PATTERN = new RegExp([!@#]([a-zA-Z]+)({!@#}{code(92)}d+)[!@#], "g")
function varNameToPowers(varName) {
    function polynomVarNameToPowers(varName) {
        return [...varName.matchAll(VAR_NAME_TO_POWERS_PATTERN)].map(match => [match[1].toLowerCase(), parseInt(match[2])])
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
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const subtreeToModify = findFirstInTree(tree, node => {
                return isTerm(node, allowedConsts) && !isForbiddenToBeTerm(tree, node)
            })
            if (subtreeToModify !== undefined) {
                const varName = await getPolynomVarNameForTerm(subtreeToModify)
                return {
                    subtreeToModify,
                    newSubtree: makeConst(varName),
                    newVariables: [[subtreeToModify.exprType, varName, subtreeToModify]]
                }
            }
            return undefined
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
        return makeConst([!@#]( ( {!@#}{aS} {!@#}{ooS} {!@#}{bS} ) {!@#}{ioS} ( {!@#}{aS} {!@#}{ooS} {!@#}{cS} ) )[!@#])
    }
    const [LP,[lp,b,io,c,rp],oo,a,RP] = matchExn(tree,['(', ['(', '', '', '', ')'], '', '', ')'])
    const aS = syntaxTreeToText(a)
    const bS = syntaxTreeToText(b)
    const cS = syntaxTreeToText(c)
    const ioS = syntaxTreeToText(io)
    const ooS = syntaxTreeToText(oo)
    return makeConst([!@#]( ( {!@#}{bS} {!@#}{ooS} {!@#}{aS} ) {!@#}{ioS} ( {!@#}{cS} {!@#}{ooS} {!@#}{aS} ) )[!@#])
}

async function distribute(isDistributable) {
    await runSequenceOfModificationsForFragment({
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const subtreeToModify = findFirstInTree(tree, isDistributable)
            if (subtreeToModify !== undefined) {
                return {
                    subtreeToModify,
                    newSubtree: applyDistribution(subtreeToModify),
                }
            }
            return undefined
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

function makeUndistributablePattern(insideOperator,outsideOperator) {
    return ['(', ['(', exprOfType('class'), [outsideOperator], exprOfType('class'), ')'], [insideOperator], ['(', exprOfType('class'), [outsideOperator], exprOfType('class'), ')'], ')']
}

function applyUndistribution(tree) {
    const [o1, [o2, A, oo1, B, c2], io, [o3, C, oo2, D, c3], c1] = matchExn(
        tree,
        ['(', ['(', '', '', '', ')'], '', ['(', '', '', '', ')'], ')']
    )
    const aS = syntaxTreeToText(A)
    const bS = syntaxTreeToText(B)
    const cS = syntaxTreeToText(C)
    const dS = syntaxTreeToText(D)
    const ooS = syntaxTreeToText(oo1)
    const ioS = syntaxTreeToText(io)
    if (aS === cS) {
        return makeConst([!@#]( {!@#}{aS} {!@#}{ooS} ( {!@#}{bS} {!@#}{ioS} {!@#}{dS} ) )[!@#])
    } else {
        return makeConst([!@#]( ( {!@#}{aS} {!@#}{ioS} {!@#}{cS} ) {!@#}{ooS} {!@#}{bS}  )[!@#])
    }
}

async function undistribute(isUndistributable) {
    await runSequenceOfModificationsForFragment({
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const subtreeToModify = findFirstInTree(tree, isUndistributable)
            if (subtreeToModify !== undefined) {
                return {
                    subtreeToModify,
                    newSubtree: applyUndistribution(subtreeToModify),
                }
            }
            return undefined
        },
    })
}

async function undistributeMul() {
    const pattern = makeUndistributablePattern('+', 'x.')
    await undistribute(node => {
        const matchResult = match(node, pattern)
        if (matchResult !== undefined) {
            const [o1, [o2, A, oo1, B, c2], io, [o3, C, oo2, D, c3], c1] = matchResult
            return syntaxTreeToText(A) === syntaxTreeToText(C) || syntaxTreeToText(B) === syntaxTreeToText(D)
        }
        return false
    })
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
    return makeConst([!@#]( {!@#}{syntaxTreeToText(rightSubtree)} {!@#}{syntaxTreeToText(op)} {!@#}{syntaxTreeToText(leftSubtree)} )[!@#])
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
    return makeConst([!@#]( {!@#}{aS} {!@#}{o1S} ( {!@#}{bS} {!@#}{o2S} {!@#}{cS} ) )[!@#])
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
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const {subtreeToSwap,subtreeToAssoc} = getSubtreeToSort(tree, symCmpFn)
            if (subtreeToSwap !== undefined) {
                console.log('subtreeToSwap', syntaxTreeToText(subtreeToSwap))
                return {subtreeToModify:subtreeToSwap, newSubtree: swapTree(subtreeToSwap)}
            } else if (subtreeToAssoc !== undefined) {
                console.log('subtreeToAssoc', syntaxTreeToText(subtreeToAssoc))
                return {subtreeToModify:subtreeToAssoc, newSubtree: assocTree(subtreeToAssoc)}
            }
            return undefined
        }
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
    function comparePowers(powersA,powersB) {
        if (powersA.length === 0 && powersB.length > 0) {
            return 1
        } else if (powersA.length > 0 && powersB.length === 0) {
            return -1
        } else if (powersA.length === 0 && powersB.length === 0) {
            return 0
        } else {
            const sumOfPowersA = powersA.reduce((sum,[varName,powNum]) => sum+powNum, 0)
            const sumOfPowersB = powersB.reduce((sum,[varName,powNum]) => sum+powNum, 0)
            if (sumOfPowersA < sumOfPowersB) {
                return 1
            } else if (sumOfPowersA > sumOfPowersB) {
                return -1
            } else {
                const minVarA = powersA[0][0]
                const minVarB = powersB[0][0]
                if (minVarA < minVarB) {
                    return -1
                } else if (minVarA > minVarB) {
                    return 1
                } else {
                    return comparePowers(powersA.slice(1),powersB.slice(1))
                }
            }
        }
    }
    function comparePolynomVarNamesByBaseName(a,b) {
        return comparePowers(varNameToPowers(a), varNameToPowers(b))
    }
    await sortSymbols(comparePolynomVarNamesByBaseName)
}

function getSubtreeToGroup(tree, isSym, symToKey) {
    function getAllKeys(tree) {
        const res = []
        forEachNode(tree, node => {
            if (isSym(node)) {
                res.push(symToKey(node))
            }
        })
        return res
    }
    return findFirstInTree(tree, node => {
        const matchResult = match(node, ['(', '', '', ['(', '', '', '', ')'], ')'])
        if (matchResult === undefined) {
            return false
        }
        const [Lp, A, op1, [lp, B, op2, C, rp], Rp] = matchResult
        const aKeys = getAllKeys(A)
        const bKeys = getAllKeys(B)
        console.log('aKeys', JSON.stringify(aKeys))
        console.log('bKeys', JSON.stringify(bKeys))
        return arIntersect(aKeys, bKeys).length > 0
    })
}

function groupTree(tree) {
    const [Lp, A, op1, [lp, B, op2, C, rp], Rp] = matchExn(tree, ['(', '', '', ['(', '', '', '', ')'], ')'])
    const aStr = syntaxTreeToText(A)
    const bStr = syntaxTreeToText(B)
    const cStr = syntaxTreeToText(C)
    const op1Str = syntaxTreeToText(op1)
    const op2Str = syntaxTreeToText(op2)
    return makeConst([!@#]( ( {!@#}{aStr} {!@#}{op1Str} {!@#}{bStr} ) {!@#}{op2Str} {!@#}{cStr} )[!@#])
}

async function groupSymbols(isSym, symToKey) {
    await runSequenceOfModificationsForFragment({
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const subtreeToGroup = getSubtreeToGroup(tree, isSym, symToKey)
            if (subtreeToGroup !== undefined) {
                console.log('subtreeToGroup', syntaxTreeToText(subtreeToGroup))
                const newSubtree = groupTree(subtreeToGroup)
                console.log('newSubtree', newSubtree)
                return {subtreeToModify:subtreeToGroup, newSubtree}
            }
            return undefined
        }
    })
}

async function groupPolynomialVariables() {
    function isSym(node) {
        return node.isVar
    }
    function symToKey(node) {
        return varNameToPowers(node.sym).map(([varName,powNum]) => varName.toLowerCase()+powNum).join('')
    }
    await groupSymbols(isSym, symToKey)
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
            return makeConst([!@#]( {!@#}{aS} ^ ( {!@#}{bS} + {!@#}{dS} ) )[!@#])
        }
        matchResult = match(tree, ['(', ['(', '', ['^'], '', ')'], ['^'], '', ')'])
        if (matchResult !== undefined) {
            const [LP, [lp, a, exp1, b, rp], exp2, c, RP] = matchResult
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            const cS = syntaxTreeToText(c)
            return makeConst([!@#]( {!@#}{aS} ^ ( {!@#}{bS} x. {!@#}{cS} ) )[!@#])
        }
        matchResult = match(tree, ['(', ['(', '', ['^'], '', ')'], ['x.'], '', ')'])
        if (matchResult !== undefined) {
            const [LP, [lp, a, exp1, b, rp], mul, c, RP] = matchResult
            if (!treeEq(a,c)) {
                return undefined
            }
            const aS = syntaxTreeToText(a)
            const bS = syntaxTreeToText(b)
            return makeConst([!@#]( {!@#}{aS} ^ ( {!@#}{bS} + 1 ) )[!@#])
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
            return makeConst([!@#]( ( {!@#}{aS} ^ {!@#}{bS} ) x. {!@#}{cS} )[!@#])
        }
        return undefined
    }
    await runSequenceOfModificationsForFragment({
        frmParams:FPR_EQUALS,
        modifySubtree: async tree => {
            const subtreeToModify = findFirstInTree(tree, node => combineExponentsPriv(node) !== undefined)
            if (subtreeToModify !== undefined) {
                return {subtreeToModify, newSubtree:combineExponentsPriv(subtreeToModify)}
            }
            return undefined
        }
    })
}

function makeMacro(name, func) {
    return {
        displayName:name,
        run: async globalContext => {
            try {
                await func(globalContext)
            } catch (ex) {
                await showErrMsg([!@#]{!@#}{ex.message}{!@#}{code(10)}{!@#}{ex.stack}[!@#])
                throw ex
            }
        }
    }
}

async function saveEditorState(ctx) {
    ctx.editorState = await getEditorState()
    showInfoMsg('Editor state was saved.')
}

async function showNewAssertions(ctx, frmParams) {
    const prevState = ctx.editorState
    if (prevState === undefined) {
        exn('prevState === undefined')
    }
    const prevStepIds = new Set(prevState.steps.map(step => step.id))
    const frameParameters = resolveFrmParams(frmParams)
    const existingFrms = new Set(frameParameters.flatMap(fp => fp.frames))
    const currState = await getEditorState()
    const newFrms = []
    currState.steps.forEach(step => {
        if (!prevStepIds.has(step.id) && !existingFrms.has(step.jstf?.asrt)) {
            newFrms.push(step.jstf?.asrt)
        }
    })
    console.log('new assertions', newFrms)
    await showInfoMsg([!@#]New assertions not present in {!@#}{JSON.stringify(frmParams)}:{!@#}{code(10)}{!@#}{newFrms.join(code(10))}[!@#])
}

const macros = [
    makeMacro('Prove "equals"', async () => await proveSelected({frmParams:FPR_EQUALS, debugLevel:0})),
    makeMacro([!@#]Prove "element of"[!@#], async () => await proveSelected({frmParams:FPR_ELEM_OF, debugLevel:0})),
    makeMacro('Introduce variables +', introduceVariablesSum),
    makeMacro('Introduce variables x.', introduceVariablesMul),
    makeMacro('Distribute x.', distributeMul),
    makeMacro('Distribute ^', distributeExp),
    makeMacro('Un-distribute x.', undistributeMul),
    makeMacro('Sort polynomial variables', sortPolynomialVariables),
    makeMacro('Group polynomial variables', groupPolynomialVariables),
    makeMacro('Sort regular symbols', sortRegularSymbols),
    makeMacro('Eliminate variables in selection', async () => await eliminateVariablesInSelectedFragment()),
    makeMacro('Eliminate all variables', async () => {
        await setContentIsHidden(true)
        await eliminateVariables()
        await setContentIsHidden(false)
    }),
    makeMacro('Combine exponents', combineExponents),
    makeMacro('Deduction to inference', deductionToInference),
    makeMacro('Inference to closed', inferenceToClosed),
    // makeMacro('Save editor state', saveEditorState),
    // makeMacro('Show new assertions for "element of"', ctx => showNewAssertions(ctx,FPR_ELEM_OF)),
    // makeMacro('Show new assertions for "equals"', ctx => showNewAssertions(ctx,FPR_EQUALS)),
]

return macros
`
