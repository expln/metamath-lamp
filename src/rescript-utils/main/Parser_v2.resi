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

type parser<'t, 'd> = parserInput<'t> => option<parsed<'t, 'd>>

let parseL: (array<'t>, ()=>parser<'t, 'd>) => option<'d>
let parse: (array<'t>, parser<'t, 'd>) => option<'d>

let makeParserInput: array<'t> => parserInput<'t>
let toInput: parsed<'t, 'd> => parserInput<'t>
let isEmpty: parserInput<'t> => bool

let mapParsed: (parser<'t, 'a>, parsed<'t, 'a> => 'b) => parser<'t, 'b>
let map: (parser<'t, 'a>, 'a => 'b) => parser<'t, 'b>
let mapOpt: (parser<'t, 'a>, 'a => option<'b>) => parser<'t, 'b>
let flatMap: (parser<'t, array<'a>>, parser<'a,'b>) => parser<'t, 'b>

let match: ('t => option<'d>) => parser<'t, 'd>
let oneOf: (array<'t>) => parser<'t, 't>
let val: 't => parser<'t, 't>
let rep: (parser<'t, 'd>, ~minCnt: int=?, ~maxCnt: int=?) => parser<'t, array<'d>>
let repL: (()=>parser<'t, 'd>, ~minCnt: int=?, ~maxCnt: int=?) => parser<'t, array<'d>>
let opt: parser<'t, 'd> => parser<'t, option<'d>>
let optL: (()=>parser<'t, 'd>) => parser<'t, option<'d>>
let seq: array<parser<'t, 'd>> => parser<'t, array<'d>>
let seq2: (parser<'t, 'd1>, parser<'t, 'd2>) => parser<'t, ('d1, 'd2)>
let seq3: (parser<'t, 'd1>, parser<'t, 'd2>, parser<'t, 'd3>) => parser<'t, ('d1, 'd2, 'd3)>
let seqL: array<()=>parser<'t, 'd>> => parser<'t, array<'d>>
let seq2L: (()=>parser<'t, 'd1>, ()=>parser<'t, 'd2>) => parser<'t, ('d1, 'd2)>
let seq3L: (()=>parser<'t, 'd1>, ()=>parser<'t, 'd2>, ()=>parser<'t, 'd3>) => parser<'t, ('d1, 'd2, 'd3)>
let any: array<parser<'t, 'd>> => parser<'t, 'd>
let anyL: array<()=>parser<'t, 'd>> => parser<'t, 'd>

let nonEmpty: parser<'t,array<'d>> => parser<'t,array<'d>>
let end: parser<'t, 'd> => parser<'t, 'd>
