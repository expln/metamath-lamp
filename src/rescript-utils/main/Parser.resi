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

let mapParsed: (parser<'t, 'a>, parsed<'t,'a> => 'b) => parser<'t, 'b>
let map: (parser<'t, 'a>, 'a => 'b) => parser<'t, 'b>
let mapRes: (parser<'t, 'a>, 'a => result<'b,unit>) => parser<'t, 'b>

let match: ('t => bool) => parser<'t, 't>
let rep: (()=>parser<'t, 'd>, ~minCnt: int=?) => parser<'t, array<'d>>
let seq: array<()=>parser<'t, 'd>> => parser<'t, array<'d>>
let seq2: (()=>parser<'t,'d1>, ()=>parser<'t,'d2>) => parser<'t,('d1,'d2)>
let seq3: (()=>parser<'t,'d1>, ()=>parser<'t,'d2>, ()=>parser<'t,'d3>) => parser<'t,('d1,'d2,'d3)>
let any: array<()=>parser<'t, 'd>> => parser<'t, 'd>
let take: int => parser<'t, array<'t>>
let nonEmpty: parser<'t,array<'d>> => parser<'t,array<'d>>
let end: parser<'t,'d> => parser<'t,'d>

let parse: (array<'t>, ()=>parser<'t,'d>) => result<'d,unit>