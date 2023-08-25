const bkgColor = "yellow"
const nbsp = String.fromCharCode(160)

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

const appendOnSide = ({init, text, right}) => {
    if (text.trim() === "") {
        return [{cmp:"Text", value: nbsp+init+nbsp}]
    } else if (right) {
        return [
            {cmp:"Text", value: nbsp+init+nbsp},
            {cmp:"Text", value: nbsp+text+nbsp, bkgColor},
        ]
    } else {
        return [
            {cmp:"Text", value: nbsp+text+nbsp, bkgColor},
            {cmp:"Text", value: nbsp+init+nbsp},
        ]
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
                    {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, bkgColor},
                    ...appendOnSide({init:selection.text, text:state.text, right:state.right}),
                    {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, bkgColor},
                ]
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren === paren, label:paren, onChange:onParenChange(paren)}
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
    canApply:({selection})=> selection.children.length === 3 || selection.children.length === 5,
    createInitialState: ({selection}) => ({text:"", right:true, paren:"( )"}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, bkgColor},
                        ...appendOnSide({init:selection.children[0].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, bkgColor},
                        {cmp:"Text", value: nbsp+selection.children[1].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, bkgColor},
                        ...appendOnSide({init:selection.children[2].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, bkgColor},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: selection.children[0].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, bkgColor},
                        ...appendOnSide({init:selection.children[1].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, bkgColor},
                        {cmp:"Text", value: nbsp+selection.children[2].text+nbsp},
                        {cmp:"Text", value: leftParen === "" ? "" : nbsp+leftParen+nbsp, bkgColor},
                        ...appendOnSide({init:selection.children[3].text, text:state.text, right:state.right}),
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen+nbsp, bkgColor},
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

const trElide = {
    displayName: ({selection}) => "Elide: ( X + A ) => X",
    canApply:({selection})=> selection.children.length === 3 || selection.children.length === 5,
    createInitialState: ({selection}) => ({text:"", right:false, paren:NO_PARENS}),
    renderDialog: ({selection, state, setState}) => {
        const getSelectedParens = () => state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
        const rndResult = () => {
            const [leftParen, rightParen] = getSelectedParens()
            if (selection.children.length === 3) {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : leftParen+nbsp, bkgColor},
                        {cmp:"Text", value: nbsp+(state.right ? selection.children[2].text : selection.children[0].text)+nbsp},
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen, bkgColor},
                    ]
                }
            } else {
                return {cmp:"span",
                    children: [
                        {cmp:"Text", value: leftParen === "" ? "" : leftParen+nbsp, bkgColor},
                        {cmp:"Text", value: nbsp+(state.right ? selection.children[3].text : selection.children[1].text)+nbsp},
                        {cmp:"Text", value: rightParen === "" ? "" : nbsp+rightParen, bkgColor},
                    ]
                }
            }
        }
        const updateState = attrName => newValue => setState(st => ({...st, [attrName]: newValue}))
        const onParenChange = paren => checked => updateState('paren')(checked ? paren : NO_PARENS)
        const rndParenCheckbox = paren => {
            return {cmp:"Checkbox", checked: state.paren === paren, label:paren, onChange:onParenChange(paren)}
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

return [trInsert1, trInsert2, trElide]