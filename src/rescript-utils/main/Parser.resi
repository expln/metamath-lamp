type parserInput<'t> = {
    tokens: array<'t>, 
    begin: int
}

type parsed<'t, 'd> = {
    tokens: array<'t>, 
    begin: int, 
    end: int, 
    data: 'd
}

type parser<'t, 'd> = parserInput<'t> => result<parsed<'t, 'd>, unit>

let makeParserInput: array<'t> => parserInput<'t>
let toInput: parsed<'t, 'd> => parserInput<'t>
let isEmpty: parserInput<'t> => bool

let match: ('t => bool) => parser<'t, 't>
let rep: (parser<'t, 'd>, ~minCnt: int=?) => parser<'t, array<'d>>
let seq: array<parser<'t, 'd>> => parser<'t, array<'d>>
let any: array<parser<'t, 'd>> => parser<'t, 'd>

let map: (parser<'t, 'a>, parsed<'t,'a> => 'b) => parser<'t, 'b>