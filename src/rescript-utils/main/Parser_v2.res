type parserInput<'t> = {
    tokens:array<'t>,
    begin:int,
}

type parsed<'t,'d> = {
    tokens:array<'t>,
    begin:int,
    end:int,
    data:'d,
}

type parser<'t,'d> = parserInput<'t> => option<parsed<'t,'d>>

let makeParserInput = (tokens:array<'t>):parserInput<'t> => {
    { tokens, begin: 0 }
}

let parseL = (tokens:array<'t>, parser:()=>parser<'t,'d>):option<'d> => {
    parser()(tokens->makeParserInput)->Option.map(({data}) => data)
}

let parse = (tokens:array<'t>, parser:parser<'t,'d>):option<'d> => {
    parseL(tokens, ()=>parser)
}

let toInput = (parsed:parsed<'t,'d>):parserInput<'t> => {
    {
        tokens:parsed.tokens,
        begin:parsed.end+1,
    }
}

let isEmpty = (inp:parserInput<'t>):bool => {
    inp.tokens->Array.length <= inp.begin
}

let mapParsed = (parser:parser<'t,'a>, func:parsed<'t,'a>=>'b):parser<'t,'b> => inp => {
    parser(inp)->Option.map(parsed => {...parsed, data:func(parsed)})
}

let map = (parser:parser<'t,'a>, func:'a=>'b):parser<'t,'b> => inp => {
    parser(inp)->Option.map(parsed => {...parsed, data:func(parsed.data)})
}

let mapOpt = (parser:parser<'t,'a>, func:'a=>option<'b>):parser<'t,'b> => inp => {
    switch parser(inp) {
        | None => None
        | Some(parsed) => parsed.data->func->Option.map(b => {...parsed, data:b})
    }
}

let flatMap = (parser1:parser<'t, array<'a>>, parser2:parser<'a,'b>):parser<'t, 'b> => {
    parser1->mapOpt(parse(_, parser2))
}

let match = (matcher:'t=>option<'d>):parser<'t,'d> => inp => {
    if (isEmpty(inp)) {
        None
    } else {
        let token = inp.tokens->Array.getUnsafe(inp.begin)
        matcher(token)->Option.map(data => {tokens:inp.tokens, begin:inp.begin, end:inp.begin, data})
    }
}

let repL = (parser:()=>parser<'t,'d>, ~minCnt:int=0, ~maxCnt:option<int>=?):parser<'t,array<'d>> => inp => {
    let tokens = inp.tokens
    let begin = inp.begin
    let inp = ref(inp)
    let res:array<'d> = []
    let end = ref(-1)
    let mismatchFound = ref(false)
    while (!mismatchFound.contents && maxCnt->Option.mapOr(true, maxCnt => res->Array.length < maxCnt)) {
        switch parser()(inp.contents) {
            | None => mismatchFound := true
            | Some(parsed) => {
                res->Array.push(parsed.data)
                end := parsed.end
                inp := toInput(parsed)
            }
        }
    }
    if (res->Array.length < minCnt) {
        None
    } else {
        Some({tokens, begin, end:end.contents, data:res})
    }
}

let rep = (parser:parser<'t,'d>, ~minCnt:int=0, ~maxCnt:option<int>=?):parser<'t,array<'d>> => {
    repL(()=>parser, ~minCnt, ~maxCnt?)
}

let optL = (parser:()=>parser<'t, 'd>):parser<'t, option<'d>> => {
    repL(parser, ~maxCnt=1)->map(found => found->Array.length == 1 ? Some(found->Array.getUnsafe(0)) : None)
}

let opt = (parser:parser<'t, 'd>):parser<'t, option<'d>> => {
    optL(()=>parser)
}

let seqL = (parsers:array<()=>parser<'t,'d>>):parser<'t,array<'d>> => inp => {
    parsers->Array.reduce(
        Some([], -1, -1, inp),
        (ctx, parser) => {
            switch ctx {
                | None => ctx
                | Some((datas, begin, _, inp)) => {
                    switch parser()(inp) {
                        | None => None
                        | Some(parsed) => {
                            datas->Array.push(parsed.data)
                            Some((
                                datas, 
                                datas->Array.length == 1 ? parsed.begin : begin,
                                parsed.end, 
                                parsed->toInput
                            ))
                        }
                    }
                }
            }
        }
    )->Option.map(((datas, begin, end, inp)) => {tokens:inp.tokens, begin, end, data:datas})
}

let seq = (parsers:array<parser<'t,'d>>):parser<'t,array<'d>> => {
    seqL(parsers->Array.map(p => {()=>p}))
}

let seq2L = (parser1:()=>parser<'t,'d1>, parser2:()=>parser<'t,'d2>):parser<'t,('d1,'d2)> => inp => {
    parser1()(inp)->Option.flatMap(parsed1 => {
        parser2()(toInput(parsed1))->Option.map(parsed2 => {
            {
                tokens: parsed2.tokens,
                begin: parsed1.begin,
                end: parsed2.end,
                data: (parsed1.data, parsed2.data)
            }
        })
    })
}

let seq2 = (parser1:parser<'t,'d1>, parser2:parser<'t,'d2>):parser<'t,('d1,'d2)> => {
    seq2L(()=>parser1, ()=>parser2)
}

let seq3L = (
    parser1:()=>parser<'t,'d1>, 
    parser2:()=>parser<'t,'d2>, 
    parser3:()=>parser<'t,'d3>
):parser<'t,('d1,'d2,'d3)> => {
    seq2L(() => seq2L(parser1, parser2), parser3)->map((((d1,d2),d3)) => (d1,d2,d3))
}

let seq3 = ( parser1:parser<'t,'d1>, parser2:parser<'t,'d2>, parser3:parser<'t,'d3> ):parser<'t,('d1,'d2,'d3)> => {
    seq3L(()=>parser1, ()=>parser2, ()=>parser3)
}

let anyL = (parsers:array<()=>parser<'t,'d>>):parser<'t,'d> => inp => {
    parsers->Array.reduce(
        None,
        (res, parser) => {
            switch res {
                | Some(_) => res
                | None => parser()(inp)
            }
        }
    )
}

let any = (parsers:array<parser<'t,'d>>):parser<'t,'d> => {
    anyL(parsers->Array.map(p => {()=>p}))
}

let oneOf = (values:array<'t>):parser<'t, 't> => {
    any(values->Array.map(val => match(t => t == val ? Some(t) : None)))
}

let val = (value:'t):parser<'t, 't> => {
    oneOf([value])
}

let nonEmpty = (parser:parser<'t,array<'d>>):parser<'t,array<'d>> => {
    parser->mapOpt(ds => ds->Array.length == 0 ? None : Some(ds))
}

let end = (parser:parser<'t,'d>):parser<'t,'d> => inp => {
    switch parser(inp) {
        | None => None
        | Some(parsed) => {
            if (isEmpty(parsed->toInput)) {
                Some(parsed)
            } else {
                None
            }
        }
    }
}