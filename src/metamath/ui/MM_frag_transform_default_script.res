let fragmentTransformsDefaultScript = `
const YELLOW = "yellow"
const GREEN = "#ABF2BC"
const BLUE = "rgba(0,59,255,0.16)"
const nbsp = String.fromCharCode(160)

/*
Test statements:
|- ( ( a + b ) + ( c + d ) ) = 0
class a + b
wff a = b
class ( a + b )
|- ( ph -> ps )
 */

const getAllTextFromComponent = cmp => {
    if (cmp.cmp === 'Col' || cmp.cmp === 'Row' || cmp.cmp === 'span') {
        return cmp.children?.map(getAllTextFromComponent)?.join('')??''
    } else if (cmp.cmp === 'Text') {
        return cmp.value??''
    } else {
        return ''
    }
}

const NO_PARENS = "no parentheses"
const ALL_PARENS = [NO_PARENS, "( )", "[ ]", "{ }", "[. ].", "[_ ]_", "<. >.", "<< >>", "[s ]s", "(. ).", "(( ))", "[b /b"]

const match = (selection, idxs) => {
    return idxs.map(idx => {
        if (Array.isArray(idx)) {
            return selection.children[idx[0]].children.map(ch => ch.text)
        } else {
            return selection.children[idx].text
        }
    })
}

const mapToTextCmpArr = (arrOfTextParts) => {
    return arrOfTextParts.map(part => {
        if (!Array.isArray(part)) {
            if (part.trim() === "") {
                return {cmp:"Text", value: ""}
            } else {
                return {cmp:"Text", value: nbsp+part+nbsp}
            }
        } else {
            const text = part[0]
            const backgroundColor = part[1]
            if (text.trim() === "") {
                return {cmp:"Text", value: ""}
            } else {
                return {cmp:"Text", value: nbsp+text+nbsp, backgroundColor}
            }
        }
    })
}

const appendOnSideAsArr = ({init, text, right, backgroundColor}) => {
    if (text.trim() === "") {
        return [init]
    } else if (right) {
        return [init, [text,backgroundColor]]
    } else {
        return [[text,backgroundColor], init]
    }
}

const appendOnSide = ({init, text, right}) => {
    return mapToTextCmpArr(appendOnSideAsArr({init, text, right, backgroundColor:YELLOW}))
}

/**
 * X => ( X + A )
 * X = Y => ( X + A ) = ( Y + A )
 * { X = Y } => { ( X + A ) = ( Y + A ) }
 */
const trInsert = {
    displayName: () => "Insert: X => ( X + A )",
    canApply: () => true,
    createInitialState: ({selection}) => ({
        paren: "( )",
        text: "",
        right: true,
        twoSided: selection.children.length === 3 || selection.children.length === 5,
    }),
    renderDialog: ({selection, state, setState}) => {
        const canBeTwoSided = selection.children.length === 3 || selection.children.length === 5
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            const backgroundColor = GREEN
            if (selection.children.length !== 3 && selection.children.length !== 5) {
                return mapToTextCmpArr([
                    [leftParen,backgroundColor],
                    ...appendOnSideAsArr({init:selection.text, text:state.text, right:state.right, backgroundColor}),
                    [rightParen,backgroundColor],
                ])
            } else if (selection.children.length === 3) {
                if (!state.twoSided) {
                    return mapToTextCmpArr([
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:selection.text, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                    ])
                } else {
                    const [leftExpr, operator, rightExpr] = match(selection, [0,1,2])
                    return mapToTextCmpArr([
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:leftExpr, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                        operator,
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:rightExpr, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                    ])
                }
            } else { //selection.children.length === 5
                if (!state.twoSided) {
                    return mapToTextCmpArr([
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:selection.text, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                    ])
                } else {
                    const [begin, leftExpr, operator, rightExpr, end] = match(selection, [0,1,2,3,4])
                    return mapToTextCmpArr([
                        begin,
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:leftExpr, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                        operator,
                        [leftParen,backgroundColor],
                        ...appendOnSideAsArr({init:rightExpr, text:state.text, right:state.right, backgroundColor}),
                        [rightParen,backgroundColor],
                        end
                    ])
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = {cmp:"span", children: rndResult()}
        return {cmp:"Col", children:[
            {cmp:"Text", value: "Insert", fontWeight:"bold"},
            {cmp:"Text", value: "Initial:"},
            {cmp:"Text", value: selection.text},
            {cmp:"Divider"},
            {cmp:"RadioGroup", row:true, value:state.paren, onChange:updateState('paren'),
                options: ALL_PARENS.map(paren => [paren,paren])
            },
            {cmp:"Divider"},
            {cmp:"Row", children:[
                canBeTwoSided
                    ?{cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided')}
                    :null,
                {cmp:"RadioGroup", row:true, value:state.right+'', onChange: newValue => updateState('right')(newValue==='true'),
                    options: [[false+'', 'Left side'], [true+'', 'Right side']]
                },
            ]},
            {cmp:"TextField", value:state.text, label: "Insert text", onChange: updateState('text'), width:'300px'},
            {cmp:"Divider"},
            {cmp:"Text", value: "Result:"},
            resultElem,
            {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
        ]}
    }
}

const trInsert1 = {
    displayName: ({selection}) => "Insert: X => ( X + A )",
    canApply:({selection})=> true,
    createInitialState: ({selection}) => ({text:"", right:true, paren:"( )"}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            return {cmp:"span",
                children: [
                    {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, backgroundColor:YELLOW},
                    ...appendOnSide({init:selection.text, text:state.text, right:state.right}),
                    {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, backgroundColor:YELLOW},
                ]
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const resultElem = rndResult()
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Initial:"},
                {cmp:"Text", value: selection.text},
                {cmp:"Divider"},
                {cmp:"RadioGroup", row:true, value:state.paren, onChange:updateState('paren'),
                    options: ALL_PARENS.map(paren => [paren,paren])
                },
                {cmp:"Divider"},
                {cmp:"TextField", value:state.text, label: "Insert text", onChange: updateState('text'), width:'300px'},
                {cmp:"Row",
                    children:[
                        {cmp:"Checkbox", checked:!state.right, label: "Left side", onChange: newValue => setState(st => ({...st, right: !newValue}))},
                        {cmp:"Checkbox", checked:state.right, label: "Right side", onChange: updateState('right')},
                    ]
                },
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const trInsert2 = {
    displayName: ({selection}) => "Insert: X = Y => ( X + A ) = ( Y + A )",
    canApply:({selection})=> selection.children.length === 3 || selection.children.length === 5,
    createInitialState: ({selection}) => ({text:"", right:true, paren:"( )"}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, backgroundColor:YELLOW},
                        ...appendOnSide({init:selection.children[0].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, backgroundColor:YELLOW},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, backgroundColor:YELLOW},
                        ...appendOnSide({init:selection.children[2].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, backgroundColor:YELLOW},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: selection.children[0].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, backgroundColor:YELLOW},
                        ...appendOnSide({init:selection.children[1].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, backgroundColor:YELLOW},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, backgroundColor:YELLOW},
                        ...appendOnSide({init:selection.children[3].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, backgroundColor:YELLOW},
                        {cmp:"Text", value: nbsp+selection.children[4].text},
                    ]
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren === paren, label:paren, onChange:onParenChange(paren)}
        }
        const rndParens = () => ({cmp:"Row", children: ALL_PARENS.map(rndParenCheckbox)})
        const resultElem = rndResult()
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Initial:"},
                {cmp:"Text", value: selection.text},
                {cmp:"Divider"},
                rndParens(),
                {cmp:"Divider"},
                {cmp:"TextField", value:state.text, label: "Insert text", onChange: updateState('text'), width:'300px'},
                {cmp:"Row",
                    children:[
                        {cmp:"Checkbox", checked:!state.right, label: "Left side", onChange: newValue => setState(st => ({...st, right: !newValue}))},
                        {cmp:"Checkbox", checked:state.right, label: "Right side", onChange: updateState('right')},
                    ]
                },
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const trElide = {
    displayName: ({selection}) => "Elide: ( X + A ) => X",
    canApply:({selection})=> selection.children.length === 3 || selection.children.length === 5,
    createInitialState: ({selection}) => ({right:false, paren:NO_PARENS}),
    renderDialog: ({selection, state, setState}) => {
        const rndInitial = () => {
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp, backgroundColor:state.right?null:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp, backgroundColor:state.right?GREEN:null},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp, backgroundColor:state.right?null:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].text+nbsp, backgroundColor:state.right?GREEN:null},
                        {cmp:"Text", value: nbsp+selection.children[4].text+nbsp},
                    ]
                }
            }
        }
        const getSelectedParens = () => state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : leftParen+nbsp, backgroundColor:YELLOW},
                        {cmp:"Text", value: nbsp+(state.right ? selection.children[2].text : selection.children[0].text)+nbsp},
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen, backgroundColor:YELLOW},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : leftParen+nbsp, backgroundColor:YELLOW},
                        {cmp:"Text", value: nbsp+(state.right ? selection.children[3].text : selection.children[1].text)+nbsp},
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen, backgroundColor:YELLOW},
                    ]
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren === paren, label:paren, onChange:onParenChange(paren)}
        }
        const rndParens = () => ({cmp:"Row", children: ALL_PARENS.map(rndParenCheckbox)})
        const resultElem = rndResult()
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Initial:"},
                rndInitial(),
                {cmp:"Divider"},
                rndParens(),
                {cmp:"Divider"},
                {cmp:"Row",
                    children:[
                        {cmp:"Checkbox", checked:!state.right, label: "Left", onChange: newValue => setState(st => ({...st, right: !newValue}))},
                        {cmp:"Checkbox", checked:state.right, label: "Right", onChange: updateState('right')},
                    ]
                },
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const trSwap = {
    displayName: ({selection}) => "Swap: X = Y => Y = X",
    canApply:({selection}) => selection.children.length === 3 || selection.children.length === 5,
    createInitialState: ({selection}) => ({}),
    renderDialog: ({selection, state, setState}) => {
        const rndInitial = () => {
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp, backgroundColor:BLUE},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].text+nbsp, backgroundColor:BLUE},
                        {cmp:"Text", value: nbsp+selection.children[4].text+nbsp},
                    ]
                }
            }
        }
        const rndResult = () => {
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp, backgroundColor:BLUE},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp, backgroundColor:GREEN},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].text+nbsp, backgroundColor:BLUE},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[4].text+nbsp},
                    ]
                }
            }
        }
        const resultElem = rndResult()
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Initial:"},
                rndInitial(),
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

const trAssoc = {
    displayName: ({selection}) => "Associate: ( A + B ) + C => A + ( B + C )",
    canApply:({selection}) =>
        selection.children.length === 3 && (selection.children[0].children.length === 5 || selection.children[2].children.length === 5)
        || selection.children.length === 5 && (selection.children[1].children.length === 5 || selection.children[3].children.length === 5),
    createInitialState: ({selection}) => ({
        needSideSelector:
            selection.children.length === 3 && (selection.children[0].children.length === 5 && selection.children[2].children.length === 5)
            || selection.children.length === 5 && (selection.children[1].children.length === 5 && selection.children[3].children.length === 5),
        right:false
    }),
    renderDialog: ({selection, state, setState}) => {
        const rndInitial = () => {
            if (
                selection.children.length === 3
                && selection.children[0].children.length === 5
                && selection.children[2].children.length !== 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[0].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[0].children[3].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[0].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].text},
                    ]
                }
            } else if (
                selection.children.length === 3
                && selection.children[0].children.length !== 5
                && selection.children[2].children.length === 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[2].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[2].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[2].children[3].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp, backgroundColor:GREEN},
                    ]
                }
            } else if (
                selection.children.length === 3
                && selection.children[0].children.length === 5
                && selection.children[2].children.length === 5
            ) {
                if (state.right) {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[3].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[0].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[2].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp},
                        ]
                    }
                } else {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[4].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[2].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[2].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[3].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp, backgroundColor:GREEN},
                        ]
                    }
                }
            } else if (
                selection.children.length === 5
                && selection.children[1].children.length === 5
                && selection.children[3].children.length !== 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[1].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[1].children[3].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[3].text},
                        {cmp:"Text", value: nbsp+selection.children[4].text},
                    ]
                }
            } else if (
                selection.children.length === 5
                && selection.children[1].children.length !== 5
                && selection.children[3].children.length === 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text},
                        {cmp:"Text", value: nbsp+selection.children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[3].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[3].children[3].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[4].text},
                    ]
                }
            } else {
                if (state.right) {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[3].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[3].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[4].text},
                        ]
                    }
                } else {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[4].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[3].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[3].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[3].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[4].text},
                        ]
                    }
                }
            }
        }
        const rndResult = () => {
            if (
                selection.children.length === 3
                && selection.children[0].children.length === 5
                && selection.children[2].children.length !== 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[0].children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[0].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[0].children[3].text},
                        {cmp:"Text", value: nbsp+selection.children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[0].children[4].text+nbsp, backgroundColor:GREEN},
                    ]
                }
            } else if (
                selection.children.length === 3
                && selection.children[0].children.length !== 5
                && selection.children[2].children.length === 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[2].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[0].text},
                        {cmp:"Text", value: nbsp+selection.children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[2].children[3].text},
                    ]
                }
            } else if (
                selection.children.length === 3
                && selection.children[0].children.length === 5
                && selection.children[2].children.length === 5
            ) {
                if (state.right) {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[2].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[0].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[0].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[0].children[4].text+nbsp, backgroundColor:GREEN},
                        ]
                    }
                } else {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[2].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[0].children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[0].children[4].text},
                            {cmp:"Text", value: nbsp+selection.children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[1].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[2].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[2].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[2].children[3].text},
                        ]
                    }
                }
            } else if (
                selection.children.length === 5
                && selection.children[1].children.length === 5
                && selection.children[3].children.length !== 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text},
                        {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[1].children[2].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].children[3].text},
                        {cmp:"Text", value: nbsp+selection.children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[3].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[1].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[4].text},
                    ]
                }
            } else if (
                selection.children.length === 5
                && selection.children[1].children.length !== 5
                && selection.children[3].children.length === 5
            ) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].children[0].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[1].text},
                        {cmp:"Text", value: nbsp+selection.children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[3].children[1].text+nbsp},
                        {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp, backgroundColor:GREEN},
                        {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                        {cmp:"Text", value: nbsp+selection.children[3].children[3].text},
                        {cmp:"Text", value: nbsp+selection.children[4].text},
                    ]
                }
            } else {
                if (state.right) {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[2].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[1].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[1].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[4].text},
                        ]
                    }
                } else {
                    return {cmp:"span",
                        children: [
                            {cmp:"Text", value: nbsp+selection.children[0].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[3].children[0].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[1].children[0].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[1].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[1].children[4].text},
                            {cmp:"Text", value: nbsp+selection.children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[1].text+nbsp},
                            {cmp:"Text", value: nbsp+selection.children[3].children[4].text+nbsp, backgroundColor:GREEN},
                            {cmp:"Text", value: nbsp+selection.children[3].children[2].text},
                            {cmp:"Text", value: nbsp+selection.children[3].children[3].text},
                            {cmp:"Text", value: nbsp+selection.children[4].text},
                        ]
                    }
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const rndSideSelector = () => {
            if (state.needSideSelector) {
                return [
                    {cmp:"Divider"},
                    {cmp:"Row",
                        children:[
                            {cmp:"Checkbox", checked:!state.right, label: "Left", onChange: newValue => setState(st => ({...st, right: !newValue}))},
                            {cmp:"Checkbox", checked:state.right, label: "Right", onChange: updateState('right')},
                        ]
                    },
                ]
            } else {
                return []
            }
        }
        const resultElem = rndResult()
        return {cmp:"Col",
            children:[
                {cmp:"Text", value: "Initial:"},
                rndInitial(),
                ...rndSideSelector(),
                {cmp:"Divider"},
                {cmp:"Text", value: "Result:"},
                resultElem,
                {cmp:"ApplyButtons", result: getAllTextFromComponent(resultElem)},
            ]
        }
    }
}

return [trInsert, trElide, trSwap, trAssoc]
`