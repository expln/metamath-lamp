const bkgColor = "yellow"

const getAllTextFromComponent = cmp => {
    if (cmp.cmp === 'Col' || cmp.cmp === 'Row' || cmp.cmp === 'Array') {
        return cmp.children?.map(getAllTextFromComponent)?.join()??''
    } else if (cmp.cmp === 'Text') {
        return cmp.value??''
    } else {
        return ''
    }
}

const NO_PARENS = "no parentheses"
const allParens = [NO_PARENS, "( )", "[ ]", "{ }", "[. ].", "[_ ]_", "<. >.", "<< >>", "[s ]s", "(. ).", "(( ))", "[b /b"]

const trInsert1 = {
    displayName: ({selection}) => "Insert: X => ( X + A )",
    canApply:({selection})=> true,
    createInitialState: ({selection}) => ({text:"", right:true, paren:"( )"}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren == NO_PARENS ? ["", ""] : state.paren.split(" ")
        const composeBody = (init, text, right) => {
            if (text.trim() === "") {
                return {cmp:"Text", value: init}
            } else if (right) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: init},
                        {cmp:"Text", value: " " + text, bkgColor},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: text + " ", bkgColor},
                        {cmp:"Text", value: init},
                    ]
                }
            }
        }
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            return {cmp:"span",
                children: [
                    {cmp:"Text", value: leftParen === "" ? "" : leftParen + " ", bkgColor},
                    {cmp:"Text", value: composeBody(selection.text, state.text, state.right)},
                    {cmp:"Text", value: rightParen === "" ? "" : " " + rightParen, bkgColor},
                ]
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren == paren, label:paren, onChange:onParenChange(paren)}
        }
        const rndParens = () => ({cmp:"Row", children: allParens.map(rndParenCheckbox)})
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

const trInsert2 = {
    displayName: ({selection}) => "Insert: X = Y => ( X + A ) = ( Y + A )",
    canApply:({selection}) => selection.children.length == 3 || selection.children.length == 5,
    createInitialState: ({selection}) => ({text:"", right:true, paren:"( )"}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren == NO_PARENS ? ["", ""] : state.paren.split(" ")
        const composeBody = (right, init, text) => right ? init + " " + text : text + " " + init
        const getResult = init => {
            const [leftParen, rightParen] = getSelectedParens()
            if (init.length == 3) {
                const body1 = composeBody(state.right, init[0], state.text)
                const body2 = composeBody(state.right, init[2], state.text)
                return leftParen + " " + body1 + " " + rightParen + " " + init[1] + " " + leftParen + " " + body2 + " " + rightParen
            } else {
                const body1 = composeBody(state.right, init[1], state.text)
                const body2 = composeBody(state.right, init[3], state.text)
                return init[0] + leftParen + " " + body1 + " " + rightParen + " " + init[2] + " " + leftParen + " " + body2 + " " + rightParen + init[4]
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren == paren, label:paren, onChange:onParenChange(paren)}
        }
        const rndParens = () => ({cmp:"Row", children: allParens.map(rndParenCheckbox)})
        const result = getResult(selection.children.map(ch => ch.text))
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
                {cmp:"Text", value: result, bkgColor},
                {cmp:"ApplyButtons", result},
            ]
        }
    }
}

return [trInsert1, trInsert2]