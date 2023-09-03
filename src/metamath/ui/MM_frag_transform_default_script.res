let fragmentTransformsDefaultScript = `
const YELLOW = "#efef40"
const GREEN = "#ABF2BC"
const RED = "#FFC1C0"
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
            const text = part
            if (text.trim() !== "" || text === nbsp) {
                return {cmp:"Text", value: nbsp+text+nbsp}
            } else {
                return {cmp:"Text", value: ""}
            }
        } else {
            const text = part[0]
            const bkgColor = part[1]

            if (text.trim() !== "" || text === nbsp) {
                return {cmp:"Text", value: nbsp+text+nbsp, backgroundColor: bkgColor.trim() !== "" ? bkgColor : null}
            } else {
                return {cmp:"Text", value: ""}
            }
        }
    })
}

const appendOnSideAsArr = ({init, text, right, bkgColor}) => {
    if (text.trim() === "") {
        return [init]
    } else if (right) {
        return [init, [text,bkgColor]]
    } else {
        return [[text,bkgColor], init]
    }
}

const appendOnSide = ({init, text, right}) => {
    return mapToTextCmpArr(appendOnSideAsArr({init, text, right, bkgColor:YELLOW}))
}

const hasNChildren = (selection,n) => selection.children.length === n
const has3Children = selection => hasNChildren(selection,3)
const has5Children = selection => hasNChildren(selection,5)

/**
 * X = Y => [ X + A ] = [ Y + A ] : twoSided && has3Children
 * { X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && has5Children
 * X => [ X + A ] : else
 */
const trInsert = {
    displayName: () => "Insert: X => ( X + A )",
    canApply: () => true,
    createInitialState: ({selection}) => ({
        paren: "( )",
        text: "",
        right: true,
        twoSided: true,
    }),
    renderDialog: ({selection, state, setState}) => {
        const canBeTwoSided = has3Children(selection) || has5Children(selection)
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            const bkgColor = GREEN
            if (twoSidedUltimate && has3Children(selection)) {//X = Y => [ X + A ] = [ Y + A ] : twoSided && has3Children
                const [leftExpr, operator, rightExpr] = match(selection, [0,1,2])
                return mapToTextCmpArr([
                    [leftParen,bkgColor],
                    ...appendOnSideAsArr({init:leftExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    operator,
                    [leftParen,bkgColor],
                    ...appendOnSideAsArr({init:rightExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                ])
            } else if (twoSidedUltimate && has5Children(selection)) {//{ X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && has5Children
                const [begin, leftExpr, operator, rightExpr, end] = match(selection, [0,1,2,3,4])
                return mapToTextCmpArr([
                    begin,
                    [leftParen,bkgColor],
                    ...appendOnSideAsArr({init:leftExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    operator,
                    [leftParen,bkgColor],
                    ...appendOnSideAsArr({init:rightExpr, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                    end
                ])
            } else {//X => [ X + A ] : else
                return mapToTextCmpArr([
                    [leftParen,bkgColor],
                    ...appendOnSideAsArr({init:selection.text, text:state.text, right:state.right, bkgColor}),
                    [rightParen,bkgColor],
                ])
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

/**
 * Two-sided:
 * X + 1 = Y + 1 => [ X = Y ] : twoSided && 3[3,3] // no test stmt
 * ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && 3[5,5] // test: |- ( X + 1 ) = ( Y + 1 )
 * { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && 5[3,3] // test: |- ( X + 1 -> Y + 1 )
 * { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && 5[5,5] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
 * One-sided:
 * { X + A } => [ X ] : 5[] // test: |- ( ph -> ps )
 * X + A => [ X ] : else // test: class X + Y
 */
const trElide = {
    displayName: () => "Elide: ( X + A ) => X",
    canApply:({selection}) => has3Children(selection) || has5Children(selection),
    createInitialState: ({selection}) => ({
        twoSided:true,
        keepLeft:true,
        paren:NO_PARENS
    }),
    renderDialog: ({selection, state, setState}) => {
        const canBeTwoSided =
            has3Children(selection) && (
                (has3Children(selection.children[0]) && has3Children(selection.children[2]))
                || (has5Children(selection.children[0]) && has5Children(selection.children[2]))
            )
            || has5Children(selection) && (
                (has3Children(selection.children[1]) && has3Children(selection.children[3]))
                || (has5Children(selection.children[1]) && has5Children(selection.children[3]))
            )
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const keepColor = YELLOW
        const insertColor = GREEN
        const rndInitial = () => {
            if (twoSidedUltimate && has3Children(selection) && has3Children(selection.children[0]) && has3Children(selection.children[2])) {
                // X + 1 = Y + 1 => [ X = Y ] : twoSided && 3[3,3] // no test stmt
                const [[leftExpr0, operator0, rightExpr0], operator, [leftExpr2, operator2, rightExpr2]] = match(selection, [[0],1,[2]])
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
            } else if (twoSidedUltimate && has3Children(selection) && has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                // ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && 3[5,5] // test: |- ( X + 1 ) = ( Y + 1 )
                const [[begin0, leftExpr0, operator0, rightExpr0, end0], operator, [begin2, leftExpr2, operator2, rightExpr2, end2]] = match(selection, [[0],1,[2]])
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
            } else if (twoSidedUltimate && has5Children(selection) && has3Children(selection.children[1]) && has3Children(selection.children[3])) {
                // { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && 5[3,3] // test: |- ( X + 1 -> Y + 1 )
                const [begin, [leftExpr1, operator1, rightExpr1], operator, [leftExpr3, operator3, rightExpr3], end] = match(selection, [0,[1],2,[3],4])
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
            } else if (twoSidedUltimate && has5Children(selection) && has5Children(selection.children[1]) && has5Children(selection.children[3])) {
                // { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && 5[5,5] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
                const [begin, [begin1, leftExpr1, operator1, rightExpr1, end1], operator, [begin3, leftExpr3, operator3, rightExpr3, end3], end] = match(selection, [0,[1],2,[3],4])
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
            } else if (has5Children(selection)) {
                // { X + A } => [ X ] : 5[] // test: |- ( ph -> ps )
                const [begin, leftExpr, operator, rightExpr, end] = match(selection, [0,1,2,3,4])
                return mapToTextCmpArr([
                    begin,
                    [leftExpr,state.keepLeft?keepColor:""],
                    operator,
                    [rightExpr,state.keepLeft?"":keepColor],
                    end,
                ])
            } else {
                // X + A => [ X ] : else // test: class X + Y
                const [leftExpr, operator, rightExpr] = match(selection, [0,1,2])
                return mapToTextCmpArr([
                    [leftExpr,state.keepLeft?keepColor:""],
                    operator,
                    [rightExpr,state.keepLeft?"":keepColor],
                ])
            }
        }
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            if (twoSidedUltimate && has3Children(selection) && has3Children(selection.children[0]) && has3Children(selection.children[2])) {
                // X + 1 = Y + 1 => [ X = Y ] : twoSided && 3[3,3] // no test stmt
                const [[leftExpr0, operator0, rightExpr0], operator, [leftExpr2, operator2, rightExpr2]] = match(selection, [[0],1,[2]])
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && has3Children(selection) && has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                // ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && 3[5,5] // test: |- ( X + 1 ) = ( Y + 1 )
                const [[begin0, leftExpr0, operator0, rightExpr0, end0], operator, [begin2, leftExpr2, operator2, rightExpr2, end2]] = match(selection, [[0],1,[2]])
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && has5Children(selection) && has3Children(selection.children[1]) && has3Children(selection.children[3])) {
                // { X + 1 = Y + 1 } => { [ X = Y ] } : twoSided && 5[3,3] // test: |- ( X + 1 -> Y + 1 )
                const [begin, [leftExpr1, operator1, rightExpr1], operator, [leftExpr3, operator3, rightExpr3], end] = match(selection, [0,[1],2,[3],4])
                return mapToTextCmpArr([
                    begin,
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr1:rightExpr1,
                    operator,
                    state.keepLeft?leftExpr3:rightExpr3,
                    [rightParen,insertColor],
                    end,
                ])
            } else if (twoSidedUltimate && has5Children(selection) && has5Children(selection.children[1]) && has5Children(selection.children[3])) {
                // { ( X + 1 ) = ( Y + 1 ) } => { [ X ] = [ Y ] } : twoSided && 5[5,5] // test: |- ( ( ph -> ps ) -> ( th -> ch ) )
                const [begin, [begin1, leftExpr1, operator1, rightExpr1, end1], operator, [begin3, leftExpr3, operator3, rightExpr3, end3], end] = match(selection, [0,[1],2,[3],4])
                return mapToTextCmpArr([
                    begin,
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr1:rightExpr1,
                    operator,
                    state.keepLeft?leftExpr3:rightExpr3,
                    [rightParen,insertColor],
                    end,
                ])
            } else if (has5Children(selection)) {
                // { X + A } => [ X ] : 5[] // test: |- ( ph -> ps )
                const [begin, leftExpr, operator, rightExpr, end] = match(selection, [0,1,2,3,4])
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr:rightExpr,
                    [rightParen,insertColor],
                ])
            } else {
                // X + A => [ X ] : else // test: class X + Y
                const [leftExpr, operator, rightExpr] = match(selection, [0,1,2])
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr:rightExpr,
                    [rightParen,insertColor],
                ])
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
                {cmp:"RadioGroup", row:true, value:state.paren, onChange:updateState('paren'),
                    options: ALL_PARENS.map(paren => [paren,paren])
                },
                {cmp:"Divider"},
                {cmp:"Row", children:[
                    canBeTwoSided
                        ?{cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided')}
                        :null,
                    {cmp:"RadioGroup", row:true, value:state.keepLeft+'', onChange: newValue => updateState('keepLeft')(newValue==='true'),
                        options: [[true+'', 'Keep left'], [false+'', 'Keep right']]
                    },
                ]},
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