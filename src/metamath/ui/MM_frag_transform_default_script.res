let fragmentTransformsDefaultScript = `
const YELLOW = "#efef40"
const GREEN = "#ABF2BC"
const PURPLE = "rgba(195,94,255,0.63)"
const RED = "#FFC1C0"
const BLUE = "rgba(0,59,255,0.16)"
const nbsp = String.fromCharCode(160)

const NO_PARENS = "no parentheses"
const ALL_PARENS = [NO_PARENS, "( )", "[ ]", "{ }", "[. ].", "[_ ]_", "<. >.", "<< >>", "[s ]s", "(. ).", "(( ))", "[b /b"]

function isSymbol(tree) {
    return tree.nodeType === "sym"
}

function isVar(tree) {
    return tree.nodeType === "expr" && tree.children.length === 1 && tree.children[0].isVar
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

const findMatch = (tree,patterns) => {
    for (const pattern of patterns) {
        const foundMatch = match(tree,pattern)
        if (foundMatch !== undefined) {
            return {pattern, match:foundMatch}
        }
    }
    return undefined
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

function syntaxTreeToText(tree) {
    if (isSymbol(tree)) {
        return tree.sym
    } else {
        return tree.children.map(syntaxTreeToText).join(' ')
    }
}

function getSelection(step) {
    return findFirstInTree(step.tree.root, node => node.id === step.fragId)
}

function textPartToStr(part) {
    if (isObj(part)) {
        return syntaxTreeToText(part)
    }
    return part
}

const mapToTextCmpArr = (arrOfTextParts) => {
    return arrOfTextParts.map(part => {
        if (!Array.isArray(part)) {
            const text = textPartToStr(part)
            if (text.trim() !== "" || text === nbsp) {
                return {cmp:"Text", value: nbsp+text+nbsp}
            } else {
                return {cmp:"Text", value: ""}
            }
        } else {
            const text = textPartToStr(part[0])
            const bkgColor = part[1]

            if (text.trim() !== "" || text === nbsp) {
                return {cmp:"Text", value: nbsp+text+nbsp, backgroundColor: bkgColor.trim() !== "" ? bkgColor : null}
            } else {
                return {cmp:"Text", value: ""}
            }
        }
    })
}

const appendOnSide = ({init, text, right, bkgColor}) => {
    if (text.trim() === "") {
        if (right) {
            return [init, [nbsp,bkgColor], nbsp]
        } else {
            return [nbsp, [nbsp,bkgColor], init]
        }
    } else if (right) {
        return [init, [text,bkgColor], nbsp]
    } else {
        return [nbsp, [text,bkgColor], init]
    }
}

const getAllTextFromComponent = cmp => {
    if (cmp.cmp === 'Col' || cmp.cmp === 'Row' || cmp.cmp === 'span') {
        return cmp.children?.map(getAllTextFromComponent)?.join('')??''
    } else if (cmp.cmp === 'Text') {
        return cmp.value??''
    } else {
        return ''
    }
}

const makeSimpleTransform = ({displayName, pattern, makeInitial, makeResult, isDebug}) => {
    return {
        displayName: ({step}) => displayName,
        canApply: (params) => {
            if (isDebug) {
                console.log(displayName + ': params = ' + JSON.stringify(params));
            }
            const {step} = params
            return undefined !== match(getSelection(step), pattern)
        },
        createInitialState: ({step}) => {
            let selection = getSelection(step)
            return ({
                selection,
                selectionText: syntaxTreeToText(selection),
            })
        },
        renderDialog: ({state, setState}) => {
            const matched = match(state.selection, pattern)
            const resultElem = {cmp: "span", children: mapToTextCmpArr(makeResult(matched))}
            return {
                cmp: "Col",
                children: [
                    {cmp:"Text", value: "Initial:"},
                    {cmp:"span", children: mapToTextCmpArr(makeInitial(matched))},
                    {cmp: "Text", value: "Result:"},
                    resultElem,
                    {cmp: "ApplyButtons", result: getAllTextFromComponent(resultElem)},
                ]
            }
        }
    }
}

const insertSelPat1 = ['', '', '']
const insertSelPat2 = ['', '', '', '', '']
const insertSelPatterns = [insertSelPat1,insertSelPat2]
const insertCanBeTwoSided = selection => findMatch(selection,insertSelPatterns) !== undefined

/**
 * X = Y => [ X + A ] = [ Y + A ] : twoSided && insertSelShape1 = ['', '', '']
 * { X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && insertSelShape2 = ['', '', '', '', '']
 * X => [ X + A ] : else
 */
const trInsert = {
    displayName: () => "Insert: X ⇒ ( X + A )",
    canApply: () => true,
    createInitialState: ({step}) => {
        const selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            selMatch: findMatch(selection, insertSelPatterns),
            paren: "( )",
            text: "",
            right: true,
            twoSided: insertCanBeTwoSided(selection),
        })
    },
    renderDialog: ({state, setState}) => {
        const canBeTwoSided = insertCanBeTwoSided(state.selection)
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            const bkgColor = GREEN
            if (twoSidedUltimate && state.selMatch?.pattern === insertSelPat1) {//X = Y => [ X + A ] = [ Y + A ] : twoSided && insertSelShape1 = ['', '', '']
                const [leftExpr, operator, rightExpr] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftParen,bkgColor],
                    ...appendOnSide({init:leftExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    operator,
                    [leftParen,bkgColor],
                    ...appendOnSide({init:rightExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === insertSelPat2) {//{ X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && insertSelShape2 = ['', '', '', '', '']
                const [begin, leftExpr, operator, rightExpr, end] = state.selMatch.match
                return mapToTextCmpArr([
                    begin,
                    [leftParen,bkgColor],
                    ...appendOnSide({init:leftExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    operator,
                    [leftParen,bkgColor],
                    ...appendOnSide({init:rightExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    end
                ])
            } else {//X => [ X + A ] : else
                return mapToTextCmpArr([
                    [leftParen,bkgColor],
                    ...appendOnSide({init:state.selectionText, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                ])
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col", children:[
            {cmp:"Text", value: "Insert", fontWeight:"bold"},
            {cmp:"Text", value: "Initial:"},
            {cmp:"Text", value: state.selectionText},
            {cmp:"Divider"},
            {cmp:"Row", children:[
                {cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided'), disabled:!canBeTwoSided},
                {cmp:"RadioGroup", row:true, value:state.right+'', onChange: newValue => updateState('right')(newValue==='true'),
                    options: [[false+'', 'Left side'], [true+'', 'Right side']]
                },
            ]},
            {cmp:"TextField", value:state.text, label: "Insert text", onChange: updateState('text'), width:'300px'},
            {cmp:"Divider"},
            {cmp:"RadioGroup", row:true, value:state.paren, onChange:updateState('paren'),
                options: ALL_PARENS.map(paren => [paren,paren])
            },
            {cmp:"Divider"},
            {cmp:"Text", value: "Result:"},
            resultElem,
            {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
        ]}
    }
}

const elideSelPat1 = [['', '', ''], '', ['', '', '']]
const elideSelPat2 = [['', '', '', '', ''], '', ['', '', '', '', '']]
const elideSelPat3 = ['', ['', '', ''], '', ['', '', ''], '']
const elideSelPat4 = ['', ['', '', '', '', ''], '', ['', '', '', '', ''], '']
const elideSelPatterns = [elideSelPat1, elideSelPat2, elideSelPat3, elideSelPat4]
const elideCanBeTwoSided = selection => findMatch(selection,elideSelPatterns) !== undefined

/**
 * Two-sided:
 * X + 1 = Y + 1 => [ X = Y ] : twoSided && elideSelShape1 = [['', '', ''], '', ['', '', '']] // no test stmt
 * ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && elideSelShape2 = [['', '', '', '', ''], '', ['', '', '', '', '']] // test: |- ( X + 1 ) = ( Y + 1 )
 * { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && elideSelShape3 = ['', ['', '', ''], '', ['', '', ''], ''] // test: |- ( X + 1 -> Y + 1 )
 * { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && elideSelShape4 = ['', ['', '', '', '', ''], '', ['', '', '', '', ''], ''] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
 * One-sided:
 * { X + A } => [ X ] : ['', '', '', '', ''] // test: |- ( ph -> ps )
 * X + A => [ X ] : else // test: class X + Y
 */
const trElide = {
    displayName: () => "Elide: ( X + A ) ⇒ X",
    canApply:({step}) => findMatch(getSelection(step), [['', '', ''], ['', '', '', '', '']]) !== undefined,
    createInitialState: ({step}) => {
        let selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            selMatch: findMatch(selection, elideSelPatterns),
            twoSided: elideCanBeTwoSided(selection),
            keepLeft: true,
            paren: NO_PARENS
        })
    },
    renderDialog: ({state, setState}) => {
        const canBeTwoSided = elideCanBeTwoSided(state.selection)
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const keepColor = YELLOW
        const insertColor = GREEN
        const rndInitial = () => {
            if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat1) {
                // X + 1 = Y + 1 => [ X = Y ] : twoSided && elideSelShape1 = [['', '', ''], '', ['', '', '']] // no test stmt
                const [[leftExpr0, operator0, rightExpr0], operator, [leftExpr2, operator2, rightExpr2]] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftExpr0,state.keepLeft?keepColor:""],
                    operator0,
                    [rightExpr0,state.keepLeft?"":keepColor],
                    nbsp,
                    [operator,keepColor],
                    nbsp,
                    [leftExpr2,state.keepLeft?keepColor:""],
                    operator2,
                    [rightExpr2,state.keepLeft?"":keepColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat2) {
                // ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && elideSelShape2 = [['', '', '', '', ''], '', ['', '', '', '', '']] // test: |- ( X + 1 ) = ( Y + 1 )
                const [[begin0, leftExpr0, operator0, rightExpr0, end0], operator, [begin2, leftExpr2, operator2, rightExpr2, end2]] = state.selMatch.match
                return mapToTextCmpArr([
                    begin0,
                    [leftExpr0,state.keepLeft?keepColor:""],
                    operator0,
                    [rightExpr0,state.keepLeft?"":keepColor],
                    end0,
                    [operator,keepColor],
                    begin2,
                    [leftExpr2,state.keepLeft?keepColor:""],
                    operator2,
                    [rightExpr2,state.keepLeft?"":keepColor],
                    end2,
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat3) {
                // { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && elideSelShape3 = ['', ['', '', ''], '', ['', '', ''], ''] // test: |- ( X + 1 -> Y + 1 )
                const [begin, [leftExpr1, operator1, rightExpr1], operator, [leftExpr3, operator3, rightExpr3], end] = state.selMatch.match
                return mapToTextCmpArr([
                    begin,
                    [leftExpr1,state.keepLeft?keepColor:""],
                    operator1,
                    [rightExpr1,state.keepLeft?"":keepColor],
                    nbsp,
                    [operator,keepColor],
                    nbsp,
                    [leftExpr3,state.keepLeft?keepColor:""],
                    operator3,
                    [rightExpr3,state.keepLeft?"":keepColor],
                    end,
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat4) {
                // { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && elideSelShape4 = ['', ['', '', '', '', ''], '', ['', '', '', '', ''], ''] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
                const [begin, [begin1, leftExpr1, operator1, rightExpr1, end1], operator, [begin3, leftExpr3, operator3, rightExpr3, end3], end] = state.selMatch.match
                return mapToTextCmpArr([
                    begin,
                    begin1,
                    [leftExpr1,state.keepLeft?keepColor:""],
                    operator1,
                    [rightExpr1,state.keepLeft?"":keepColor],
                    end1,
                    [operator,keepColor],
                    begin3,
                    [leftExpr3,state.keepLeft?keepColor:""],
                    operator3,
                    [rightExpr3,state.keepLeft?"":keepColor],
                    end3,
                    end,
                ])
            } else {
                const match5 = match(state.selection, ['','','','',''])
                if (match5 !== undefined) {
                    // { X + A } => [ X ] : ['', '', '', '', ''] // test: |- ( ph -> ps )
                    const [begin, leftExpr, operator, rightExpr, end] = match5
                    return mapToTextCmpArr([
                        begin,
                        [leftExpr,state.keepLeft?keepColor:""],
                        operator,
                        [rightExpr,state.keepLeft?"":keepColor],
                        end,
                    ])
                } else {
                    // X + A => [ X ] : else // test: class X + Y
                    const [leftExpr, operator, rightExpr] = match(state.selection, ['','',''])
                    return mapToTextCmpArr([
                        [leftExpr,state.keepLeft?keepColor:""],
                        operator,
                        [rightExpr,state.keepLeft?"":keepColor],
                    ])
                }
            }
        }
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat1) {
                // X + 1 = Y + 1 => [ X = Y ] : twoSided && elideSelShape1 = [['', '', ''], '', ['', '', '']] // no test stmt
                const [[leftExpr0, operator0, rightExpr0], operator, [leftExpr2, operator2, rightExpr2]] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat2) {
                // ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && elideSelShape2 = [['', '', '', '', ''], '', ['', '', '', '', '']] // test: |- ( X + 1 ) = ( Y + 1 )
                const [[begin0, leftExpr0, operator0, rightExpr0, end0], operator, [begin2, leftExpr2, operator2, rightExpr2, end2]] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat3) {
                // { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && elideSelShape3 = ['', ['', '', ''], '', ['', '', ''], ''] // test: |- ( X + 1 -> Y + 1 )
                const [begin, [leftExpr1, operator1, rightExpr1], operator, [leftExpr3, operator3, rightExpr3], end] = state.selMatch.match
                return mapToTextCmpArr([
                    begin,
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr1:rightExpr1,
                    operator,
                    state.keepLeft?leftExpr3:rightExpr3,
                    [rightParen,insertColor],
                    end,
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelPat4) {
                // { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && elideSelShape4 = ['', ['', '', '', '', ''], '', ['', '', '', '', ''], ''] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
                const [begin, [begin1, leftExpr1, operator1, rightExpr1, end1], operator, [begin3, leftExpr3, operator3, rightExpr3, end3], end] = state.selMatch.match
                return mapToTextCmpArr([
                    begin,
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr1:rightExpr1,
                    operator,
                    state.keepLeft?leftExpr3:rightExpr3,
                    [rightParen,insertColor],
                    end,
                ])
            } else {
                const match5 = match(state.selection, ['','','','',''])
                if (match5 !== undefined) {
                    // { X + A } => [ X ] : ['', '', '', '', ''] // test: |- ( ph -> ps )
                    const [begin, leftExpr, operator, rightExpr, end] = match5
                    return mapToTextCmpArr([
                        [leftParen,insertColor],
                        state.keepLeft?leftExpr:rightExpr,
                        [rightParen,insertColor],
                    ])
                } else {
                    // X + A => [ X ] : else // test: class X + Y
                    const [leftExpr, operator, rightExpr] = match(state.selection, ['','',''])
                    return mapToTextCmpArr([
                        [leftParen,insertColor],
                        state.keepLeft?leftExpr:rightExpr,
                        [rightParen,insertColor],
                    ])
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Elide", fontWeight:"bold"},
                {cmp:"Text", value: "Initial:"},
                {cmp:"span", children: rndInitial()},
                {cmp:"Divider"},
                {cmp:"Row", children:[
                    {cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided'), disabled:!canBeTwoSided},
                    {cmp:"RadioGroup", row:true, value:state.keepLeft+'', onChange: newValue => updateState('keepLeft')(newValue==='true'),
                        options: [[true+'', 'Keep left'], [false+'', 'Keep right']]
                    },
                ]},
                {cmp:"Divider"},
                {cmp:"RadioGroup", row:true, value:state.paren, onChange:updateState('paren'),
                    options: ALL_PARENS.map(paren => [paren,paren])
                },
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const swapSelPat1 = ['', '', '']
const swapSelPat2 = ['', '', '', '', '']
const swapSelPatterns = [swapSelPat1, swapSelPat2]

const trSwap = {
    displayName: () => "Swap: X = Y ⇒ Y = X",
    canApply:({step}) => findMatch(getSelection(step),swapSelPatterns) !== undefined,
    createInitialState: ({step}) => {
        let selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            selMatch: findMatch(selection, swapSelPatterns)
        })
    },
    renderDialog: ({state, setState}) => {
        const rndInitial = () => {
            if (state.selMatch?.pattern === swapSelPat1) {
                const [leftExpr, operator, rightExpr] = state.selMatch.match
                return mapToTextCmpArr([[leftExpr,PURPLE], operator, [rightExpr,BLUE],])
            } else {
                const [begin, leftExpr, operator, rightExpr, end] = state.selMatch.match
                return mapToTextCmpArr([begin, [leftExpr,PURPLE], operator, [rightExpr,BLUE], end,])
            }
        }
        const rndResult = () => {
            if (state.selMatch?.pattern === swapSelPat1) {
                const [leftExpr, operator, rightExpr] = state.selMatch.match
                return mapToTextCmpArr([[rightExpr,BLUE], operator, [leftExpr,PURPLE],])
            } else {
                const [begin, leftExpr, operator, rightExpr, end] = state.selMatch.match
                return mapToTextCmpArr([begin, [rightExpr,BLUE], operator, [leftExpr,PURPLE], end,])
            }
        }
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Swap", fontWeight:"bold"},
                {cmp:"Text", value: "Initial:"},
                {cmp:"span", children: rndInitial()},
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const assocSelPat355 = [['','','','',''],'',['','','','','']]
const assocSelPat35_ = [['','','','',''],'','']
const assocSelPat3_5 = ['','',['','','','','']]
const assocSelPat555 = ['',['','','','',''],'',['','','','',''],'']
const assocSelPat55_ = ['',['','','','',''],'','','']
const assocSelPat5_5 = ['','','',['','','','',''],'']
const assocSelPatterns = [assocSelPat355, assocSelPat555, assocSelPat35_, assocSelPat3_5, assocSelPat55_, assocSelPat5_5]

const trAssoc = {
    displayName: () => "Associate: ( A + B ) + C ⇒ A + ( B + C )",
    canApply:({step}) => findMatch(getSelection(step),assocSelPatterns) !== undefined,
    createInitialState: ({step}) => {
        let selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            selMatch: findMatch(selection, assocSelPatterns),
            needSideSelector: findMatch(selection, [assocSelPat355, assocSelPat555]) !== undefined,
            right: findMatch(selection, [assocSelPat35_, assocSelPat55_]) !== undefined
        })
    },
    renderDialog: ({state, setState}) => {
        const bkg = YELLOW
        const rndInitial = () => {
            if (state.selMatch?.pattern === assocSelPat35_) {
                const [[leftParen, a, op1, b, rightParen], op2, c] = state.selMatch.match
                return mapToTextCmpArr([[leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c])
            } else if (state.selMatch?.pattern === assocSelPat3_5) {
                const [a, op1, [leftParen, b, op2, c, rightParen]] = state.selMatch.match
                return mapToTextCmpArr([a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg]])
            } else if (state.selMatch?.pattern === assocSelPat355) {
                const [[leftParen0, a, op0, b, rightParen0], op1, [leftParen2, c, op2, d, rightParen2]] = state.selMatch.match
                if (state.right) {
                    return mapToTextCmpArr([[leftParen0,bkg], a, op0, b, [rightParen0,bkg], op1, leftParen2, c, op2, d, rightParen2])
                } else {
                    return mapToTextCmpArr([leftParen0, a, op0, b, rightParen0, op1, [leftParen2,bkg], c, op2, d, [rightParen2,bkg]])
                }
            } else if (state.selMatch?.pattern === assocSelPat55_) {
                const [begin, [leftParen, a, op1, b, rightParen], op2, c, end] = state.selMatch.match
                return mapToTextCmpArr([begin, [leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c, end])
            } else if (state.selMatch?.pattern === assocSelPat5_5) {
                const [begin, a, op1, [leftParen, b, op2, c, rightParen], end] = state.selMatch.match
                return mapToTextCmpArr([begin, a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg], end])
            } else {
                const [begin, [leftParen1, a, op1, b, rightParen1], op2, [leftParen3, c, op3, d, rightParen3], end] = state.selMatch.match
                if (state.right) {
                    return mapToTextCmpArr([begin, [leftParen1,bkg], a, op1, b, [rightParen1,bkg], op2, leftParen3, c, op3, d, rightParen3, end])
                } else {
                    return mapToTextCmpArr([begin, leftParen1, a, op1, b, rightParen1, op2, [leftParen3,bkg], c, op3, d, [rightParen3,bkg], end])
                }
            }
        }
        const rndResult = () => {
            if (state.selMatch?.pattern === assocSelPat35_) {
                const [[leftParen, a, op1, b, rightParen], op2, c] = state.selMatch.match
                return mapToTextCmpArr([a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg]])
            } else if (state.selMatch?.pattern === assocSelPat3_5) {
                const [a, op1, [leftParen, b, op2, c, rightParen]] = state.selMatch.match
                return mapToTextCmpArr([[leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c])
            } else if (state.selMatch?.pattern === assocSelPat355) {
                const [[leftParen0, a, op0, b, rightParen0], op1, [leftParen2, c, op2, d, rightParen2]] = state.selMatch.match
                if (state.right) {
                    return mapToTextCmpArr([a, op0, [leftParen0,bkg], b, op1, leftParen2, c, op2, d, rightParen2, [rightParen0,bkg]])
                } else {
                    return mapToTextCmpArr([[leftParen2,bkg], leftParen0, a, op0, b, rightParen0, op1, c, [rightParen2,bkg], op2, d])
                }
            } else if (state.selMatch?.pattern === assocSelPat55_) {
                const [begin, [leftParen, a, op1, b, rightParen], op2, c, end] = state.selMatch.match
                return mapToTextCmpArr([begin, a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg], end])
            } else if (state.selMatch?.pattern === assocSelPat5_5) {
                const [begin, a, op1, [leftParen, b, op2, c, rightParen], end] = state.selMatch.match
                return mapToTextCmpArr([begin, [leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c, end])
            } else {
                const [begin, [leftParen1, a, op1, b, rightParen1], op2, [leftParen3, c, op3, d, rightParen3], end] = state.selMatch.match
                if (state.right) {
                    return mapToTextCmpArr([begin, a, op1, [leftParen1,bkg], b, op2, leftParen3, c, op3, d, rightParen3, [rightParen1,bkg], end])
                } else {
                    return mapToTextCmpArr([begin, [leftParen3,bkg], leftParen1, a, op1, b, rightParen1, op2, c, [rightParen3,bkg], op3, d, end])
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Associate", fontWeight:"bold"},
                {cmp:"Text", value: "Initial:"},
                {cmp:"span", children: rndInitial()},
                {cmp:"Divider"},
                {cmp:"RadioGroup", row:true, value:state.right+'', onChange: newValue => updateState('right')(newValue==='true'),
                    disabled:!state.needSideSelector,
                    options: [[false+'', 'Left'], [true+'', 'Right']]
                },
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const trReplace = {
    displayName: ({step}) => "Replace",
    canApply: ({step}) => true,
    createInitialState: ({step}) => {
        let selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            text: "",
        })
    },
    renderDialog: ({state, setState}) => {
        const rndResult = () => mapToTextCmpArr([state.text])
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col", children:[
                {cmp:"Text", value: "Replace", fontWeight:"bold"},
                {cmp:"Text", value: "Initial:"},
                {cmp:"Text", value: state.selectionText},
                {cmp:"Divider"},
                {cmp:"TextField", autoFocus:true, value:state.text, label: "Replace with", onChange: updateState('text'), width:'300px'},
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]}
    }
}

const trExtract = {
    displayName: ({step}) => "Extract: X ⇒ ( ph -> X )",
    canApply: ({step}) => true,
    createInitialState: ({step}) => {
        let deductionMatchResult = match(step.tree.root, ['(', varOfType('wff'), '->', '', ')'])
        let selection = getSelection(step)
        return ({
            selection,
            selectionText: syntaxTreeToText(selection),
            ph: deductionMatchResult?.[1],
            stepType: step.tree.exprType
        })
    },
    renderDialog: ({state, setState}) => {
        const rndResult = () => {
            if (state.ph !== undefined) {
                return mapToTextCmpArr([state.stepType, '(', syntaxTreeToText(state.ph), '->', [state.selectionText, YELLOW], ')'])
            } else {
                return mapToTextCmpArr([state.stepType, '( ph ->', [state.selectionText, YELLOW], ')'])
            }
        }
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col", children:[
                {cmp:"Text", value: "Extract", fontWeight:"bold"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", replaceSelection:false, result: getAllTextFromComponent(resultElem)},
            ]}
    }
}



const allTransforms = [
    trInsert, trElide, trSwap, trAssoc, trReplace, trExtract
]

return allTransforms
`
