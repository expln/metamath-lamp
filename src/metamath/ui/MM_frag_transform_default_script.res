let fragmentTransformsDefaultScript = `
const YELLOW = "#efef40"
const GREEN = "#ABF2BC"
const PURPLE = "rgba(195,94,255,0.63)"
const RED = "#FFC1C0"
const BLUE = "rgba(0,59,255,0.16)"
const nbsp = String.fromCharCode(160)

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

const match2 = (selection, pattern) => {
    if (selection.children.length !== pattern.length) {
        return undefined
    } else {
        const result = []
        for (let i = 0; i < selection.children.length; i++) {
            const pat = pattern[i]
            const ch = selection.children[i]
            if (typeof pat === 'function') {
                result.push(pat(ch))
            } else if (Array.isArray(pat)) {
                const subMatchResult = match2(ch,pat)
                if (subMatchResult === undefined) {
                    return undefined
                } else {
                    result.push(subMatchResult)
                }
            } else {
                if (pat === '' || pat === ch.text) {
                    result.push(ch.text)
                } else {
                    return undefined
                }
            }
        }
        return result
    }
}

const findMatch = (selection,patterns) => {
    for (const pattern of patterns) {
        const foundMatch = match2(selection,pattern)
        if (foundMatch != undefined) {
            return {pattern, match:foundMatch}
        }
    }
    return undefined
}

const matches = (selection, pattern) => match2(selection, pattern) !== undefined

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

const appendOnSide = ({init, text, right, bkgColor}) => {
    if (text.trim() === "") {
        return [init]
    } else if (right) {
        return [init, [text,bkgColor]]
    } else {
        return [[text,bkgColor], init]
    }
}

const hasNChildren = (selection,n) => selection.children.length === n
const has3Children = selection => hasNChildren(selection,3)
const has5Children = selection => hasNChildren(selection,5)

const insertSelShape1 = ['', '', '']
const insertSelShape2 = ['', '', '', '', '']
const insertSelShapes = [insertSelShape1,insertSelShape2]
const insertCanBeTwoSided = selection => findMatch(selection,insertSelShapes) !== undefined

/**
 * X = Y => [ X + A ] = [ Y + A ] : twoSided && insertSelShape1 = ['', '', '']
 * { X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && insertSelShape2 = ['', '', '', '', '']
 * X => [ X + A ] : else
 */
const trInsert = {
    displayName: () => "Insert: X => ( X + A )",
    canApply: () => true,
    createInitialState: ({selection}) => ({
        selMatch: findMatch(selection,insertSelShapes),
        paren: "( )",
        text: "",
        right: true,
        twoSided: insertCanBeTwoSided(selection),
    }),
    renderDialog: ({selection, state, setState}) => {
        const canBeTwoSided = insertCanBeTwoSided(selection)
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            const bkgColor = GREEN
            if (twoSidedUltimate && state.selMatch?.pattern === insertSelShape1) {//X = Y => [ X + A ] = [ Y + A ] : twoSided && insertSelShape1 = ['', '', '']
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
            } else if (twoSidedUltimate && state.selMatch?.pattern === insertSelShape2) {//{ X = Y } => { [ X + A ] = [ Y + A ] } : twoSided && insertSelShape2 = ['', '', '', '', '']
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
                    ...appendOnSide({init:selection.text, text:state.text, right:state.right, bkgColor}),
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
                {cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided'), disabled:!canBeTwoSided},
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

const elideSelShape1 = [['', '', ''], '', ['', '', '']]
const elideSelShape2 = [['', '', '', '', ''], '', ['', '', '', '', '']]
const elideSelShape3 = ['', ['', '', ''], '', ['', '', ''], '']
const elideSelShape4 = ['', ['', '', '', '', ''], '', ['', '', '', '', ''], '']
const elideSelShapes = [elideSelShape1, elideSelShape2, elideSelShape3, elideSelShape4]
const elideCanBeTwoSided = selection => findMatch(selection,elideSelShapes) !== undefined

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
    displayName: () => "Elide: ( X + A ) => X",
    canApply:({selection}) => matches(selection,['','','']) || matches(selection,['','','','','']),
    createInitialState: ({selection}) => ({
        selMatch: findMatch(selection,elideSelShapes),
        twoSided:elideCanBeTwoSided(selection),
        keepLeft:true,
        paren:NO_PARENS
    }),
    renderDialog: ({selection, state, setState}) => {
        const canBeTwoSided = elideCanBeTwoSided(selection)
        const twoSidedUltimate = canBeTwoSided && state.twoSided
        const keepColor = YELLOW
        const insertColor = GREEN
        const rndInitial = () => {
            if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape1) {
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
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape2) {
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
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape3) {
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
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape4) {
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
            } else if (matches(selection, ['','','','',''])) {
                // { X + A } => [ X ] : ['', '', '', '', ''] // test: |- ( ph -> ps )
                const [begin, leftExpr, operator, rightExpr, end] = match2(selection, ['','','','',''])
                return mapToTextCmpArr([
                    begin,
                    [leftExpr,state.keepLeft?keepColor:""],
                    operator,
                    [rightExpr,state.keepLeft?"":keepColor],
                    end,
                ])
            } else {
                // X + A => [ X ] : else // test: class X + Y
                const [leftExpr, operator, rightExpr] = match2(selection, ['','',''])
                return mapToTextCmpArr([
                    [leftExpr,state.keepLeft?keepColor:""],
                    operator,
                    [rightExpr,state.keepLeft?"":keepColor],
                ])
            }
        }
        const rndResult = () => {
            const [leftParen, rightParen] = state.paren === NO_PARENS ? ["", ""] : state.paren.split(" ")
            if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape1) {
                // X + 1 = Y + 1 => [ X = Y ] : twoSided && elideSelShape1 = [['', '', ''], '', ['', '', '']] // no test stmt
                const [[leftExpr0, operator0, rightExpr0], operator, [leftExpr2, operator2, rightExpr2]] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape2) {
                // ( X + 1 ) = ( Y + 1 ) => [ X = Y ] : twoSided && elideSelShape2 = [['', '', '', '', ''], '', ['', '', '', '', '']] // test: |- ( X + 1 ) = ( Y + 1 )
                const [[begin0, leftExpr0, operator0, rightExpr0, end0], operator, [begin2, leftExpr2, operator2, rightExpr2, end2]] = state.selMatch.match
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr0:rightExpr0,
                    operator,
                    state.keepLeft?leftExpr2:rightExpr2,
                    [rightParen,insertColor],
                ])
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape3) {
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
            } else if (twoSidedUltimate && state.selMatch?.pattern === elideSelShape4) {
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
            } else if (matches(selection, ['','','','',''])) {
                // { X + A } => [ X ] : ['', '', '', '', ''] // test: |- ( ph -> ps )
                const [begin, leftExpr, operator, rightExpr, end] = match2(selection, ['','','','',''])
                return mapToTextCmpArr([
                    [leftParen,insertColor],
                    state.keepLeft?leftExpr:rightExpr,
                    [rightParen,insertColor],
                ])
            } else {
                // X + A => [ X ] : else // test: class X + Y
                const [leftExpr, operator, rightExpr] = match2(selection, ['','',''])
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
                    {cmp:"Checkbox", checked:state.twoSided, label: "Two-sided", onChange: updateState('twoSided'), disabled:!canBeTwoSided},
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

const swapSelShape1 = ['', '', '']
const swapSelShape2 = ['', '', '', '', '']
const swapSelShapes = [swapSelShape1, swapSelShape2]

const trSwap = {
    displayName: () => "Swap: X = Y => Y = X",
    canApply:({selection}) => findMatch(selection,swapSelShapes) !== undefined,
    createInitialState: ({selection}) => ({
        selMatch: findMatch(selection,swapSelShapes)
    }),
    renderDialog: ({selection, state, setState}) => {
        const rndInitial = () => {
            if (state.selMatch?.pattern === swapSelShape1) {
                const [leftExpr, operator, rightExpr] = state.selMatch.match
                return mapToTextCmpArr([[leftExpr,PURPLE], operator, [rightExpr,BLUE],])
            } else {
                const [begin, leftExpr, operator, rightExpr, end] = state.selMatch.match
                return mapToTextCmpArr([begin, [leftExpr,PURPLE], operator, [rightExpr,BLUE], end,])
            }
        }
        const rndResult = () => {
            if (state.selMatch?.pattern === swapSelShape1) {
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

const assocSelShape355 = [['','','','',''],'',['','','','','']]
const assocSelShape35_ = [['','','','',''],'','']
const assocSelShape3_5 = ['','',['','','','','']]
const assocSelShape555 = ['',['','','','',''],'',['','','','',''],'']
const assocSelShape55_ = ['',['','','','',''],'','','']
const assocSelShape5_5 = ['','','',['','','','',''],'']
const assocSelShapes = [assocSelShape355, assocSelShape555, assocSelShape35_, assocSelShape3_5, assocSelShape55_, assocSelShape5_5]

const trAssoc = {
    displayName: () => "Associate: ( A + B ) + C => A + ( B + C )",
    canApply:({selection}) => assocSelShapes.some(shape => matches(selection,shape)),
    createInitialState: ({selection}) => ({
        selShape: assocSelShapes.find(shape => matches(selection,shape)),
        needSideSelector: [assocSelShape355, assocSelShape555].some(shape => matches(selection,shape)),
        right: [assocSelShape35_, assocSelShape55_].some(shape => matches(selection,shape))
    }),
    renderDialog: ({selection, state, setState}) => {
        const bkg = YELLOW
        const rndInitial = () => {
            if (has3Children(selection) && has5Children(selection.children[0]) && !has5Children(selection.children[2])) {
                const [[leftParen, a, op1, b, rightParen], op2, c] = match(selection, [[0],1,2])
                return mapToTextCmpArr([[leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c])
            } else if (has3Children(selection) && !has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                const [a, op1, [leftParen, b, op2, c, rightParen]] = match(selection, [0,1,[2]])
                return mapToTextCmpArr([a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg]])
            } else if (has3Children(selection) && has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                const [[leftParen0, a, op0, b, rightParen0], op1, [leftParen2, c, op2, d, rightParen2]] = match(selection, [[0],1,[2]])
                if (state.right) {
                    return mapToTextCmpArr([[leftParen0,bkg], a, op0, b, [rightParen0,bkg], op1, leftParen2, c, op2, d, rightParen2])
                } else {
                    return mapToTextCmpArr([leftParen0, a, op0, b, rightParen0, op1, [leftParen2,bkg], c, op2, d, [rightParen2,bkg]])
                }
            } else if (has5Children(selection) && has5Children(selection.children[1]) && !has5Children(selection.children[3])) {
                const [begin, [leftParen, a, op1, b, rightParen], op2, c, end] = match(selection, [0,[1],2,3,4])
                return mapToTextCmpArr([begin, [leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c, end])
            } else if (has5Children(selection) && !has5Children(selection.children[1]) && has5Children(selection.children[3])) {
                const [begin, a, op1, [leftParen, b, op2, c, rightParen], end] = match(selection, [0,1,2,[3],4])
                return mapToTextCmpArr([begin, a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg], end])
            } else {
                const [begin, [leftParen1, a, op1, b, rightParen1], op2, [leftParen3, c, op3, d, rightParen3], end] = match(selection, [0,[1],2,[3],4])
                if (state.right) {
                    return mapToTextCmpArr([begin, [leftParen1,bkg], a, op1, b, [rightParen1,bkg], op2, leftParen3, c, op3, d, rightParen3, end])
                } else {
                    return mapToTextCmpArr([begin, leftParen1, a, op1, b, rightParen1, op2, [leftParen3,bkg], c, op3, d, [rightParen3,bkg], end])
                }
            }
        }
        const rndResult = () => {
            if (has3Children(selection) && has5Children(selection.children[0]) && !has5Children(selection.children[2])) {
                const [[leftParen, a, op1, b, rightParen], op2, c] = match(selection, [[0],1,2])
                return mapToTextCmpArr([a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg]])
            } else if (has3Children(selection) && !has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                const [a, op1, [leftParen, b, op2, c, rightParen]] = match(selection, [0,1,[2]])
                return mapToTextCmpArr([[leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c])
            } else if (has3Children(selection) && has5Children(selection.children[0]) && has5Children(selection.children[2])) {
                const [[leftParen0, a, op0, b, rightParen0], op1, [leftParen2, c, op2, d, rightParen2]] = match(selection, [[0],1,[2]])
                if (state.right) {
                    return mapToTextCmpArr([a, op0, [leftParen0,bkg], b, op1, leftParen2, c, op2, d, rightParen2, [rightParen0,bkg]])
                } else {
                    return mapToTextCmpArr([[leftParen2,bkg], leftParen0, a, op0, b, rightParen0, op1, c, [rightParen2,bkg], op2, d])
                }
            } else if (has5Children(selection) && has5Children(selection.children[1]) && !has5Children(selection.children[3])) {
                const [begin, [leftParen, a, op1, b, rightParen], op2, c, end] = match(selection, [0,[1],2,3,4])
                return mapToTextCmpArr([begin, a, op1, [leftParen,bkg], b, op2, c, [rightParen,bkg], end])
            } else if (has5Children(selection) && !has5Children(selection.children[1]) && has5Children(selection.children[3])) {
                const [begin, a, op1, [leftParen, b, op2, c, rightParen], end] = match(selection, [0,1,2,[3],4])
                return mapToTextCmpArr([begin, [leftParen,bkg], a, op1, b, [rightParen,bkg], op2, c, end])
            } else {
                const [begin, [leftParen1, a, op1, b, rightParen1], op2, [leftParen3, c, op3, d, rightParen3], end] = match(selection, [0,[1],2,[3],4])
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

return [trInsert, trElide, trSwap, trAssoc]
`