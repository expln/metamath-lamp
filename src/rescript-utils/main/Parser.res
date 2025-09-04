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

type parser<'t,'d> = parserInput<'t> => result<parsed<'t,'d>, unit>

let makeParserInput = (tokens:array<'t>):parserInput<'t> => {
    { tokens, begin: 0 }
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
    switch parser(inp) {
        | Error(_) => Error(())
        | Ok(parsed) => Ok({...parsed, data:func(parsed)})
    }
}

let map = (parser:parser<'t,'a>, func:'a=>'b):parser<'t,'b> => inp => {
    switch parser(inp) {
        | Error(_) => Error(())
        | Ok(parsed) => Ok({...parsed, data:func(parsed.data)})
    }
}

let mapRes = (parser:parser<'t,'a>, func:'a=>result<'b,unit>):parser<'t,'b> => inp => {
    switch parser(inp) {
        | Error(_) => Error(())
        | Ok(parsed) => parsed.data->func->Result.map(b => {...parsed, data:b})
    }
}

let match = (predicate:'t=>option<'d>):parser<'t,'d> => inp => {
    if (isEmpty(inp)) {
        Error(())
    } else {
        let token = inp.tokens->Array.getUnsafe(inp.begin)
        switch predicate(token) {
            | None => Error(())
            | Some(data) => Ok({tokens:inp.tokens, begin:inp.begin, end:inp.begin, data})
        }
    }
}

let rep = (parser:()=>parser<'t,'d>, ~minCnt:int=0, ~maxCnt:option<int>=?):parser<'t,array<'d>> => inp => {
    let tokens = inp.tokens
    let begin = inp.begin
    let inp = ref(inp)
    let res:array<'d> = []
    let end = ref(-1)
    let mismatchFound = ref(false)
    while (!mismatchFound.contents && maxCnt->Option.mapOr(true, maxCnt => res->Array.length < maxCnt)) {
        switch parser()(inp.contents) {
            | Error(_) => mismatchFound := true
            | Ok(parsed) => {
                res->Array.push(parsed.data)
                end := parsed.end
                inp := toInput(parsed)
            }
        }
    }
    if (res->Array.length < minCnt) {
        Error(())
    } else {
        Ok({tokens, begin, end:end.contents, data:res})
    }
}

let opt = (parser:()=>parser<'t, 'd>):parser<'t, option<'d>> => {
    rep(parser, ~maxCnt=1)->map(found => found->Array.length == 1 ? Some(found->Array.getUnsafe(0)) : None)
}

let seq = (parsers:array<()=>parser<'t,'d>>):parser<'t,array<'d>> => inp => {
    parsers->Array.reduce(
        Ok([], -1, -1, inp),
        (ctx, parser) => {
            switch ctx {
                | Error(_) => ctx
                | Ok((datas, begin, _, inp)) => {
                    switch parser()(inp) {
                        | Error(_) => Error(())
                        | Ok(parsed) => {
                            datas->Array.push(parsed.data)
                            Ok((
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
    )->Result.map(((datas, begin, end, inp)) => {tokens:inp.tokens, begin, end, data:datas})
}

let seq2 = (parser1:()=>parser<'t,'d1>, parser2:()=>parser<'t,'d2>):parser<'t,('d1,'d2)> => inp => {
    switch parser1()(inp) {
        | Error(_) => Error(())
        | Ok(parsed1) => {
            switch parser2()(toInput(parsed1)) {
                | Error(_) => Error(())
                | Ok(parsed2) => {
                    Ok({
                        tokens: parsed2.tokens,
                        begin: parsed1.begin,
                        end: parsed2.end,
                        data: (parsed1.data, parsed2.data)
                    })
                }
            }
        }
    }
}

let seq3 = (
    parser1:()=>parser<'t,'d1>, 
    parser2:()=>parser<'t,'d2>, 
    parser3:()=>parser<'t,'d3>
):parser<'t,('d1,'d2,'d3)> => {
    seq2(() => seq2(parser1, parser2), parser3)->map((((d1,d2),d3)) => (d1,d2,d3))
}

let any = (parsers:array<()=>parser<'t,'d>>):parser<'t,'d> => inp => {
    switch parsers->Array.reduce(
        None,
        (ctx, parser) => {
            switch ctx {
                | Some(_) => ctx
                | None => {
                    switch parser()(inp) {
                        | Error(_) => None
                        | Ok(parsed) => Some(parsed)
                    }
                }
            }
        }
    ) {
        | None => Error(())
        | Some(parsed) => Ok(parsed)
    }
}

let take = (cnt:int):parser<'t, array<'t>> => inp => {
    let tokens = inp.tokens
    let begin = inp.begin
    if (cnt >= 0) {
        let end = begin + cnt
        let data = tokens->Array.slice(~start=begin, ~end=end+1)
        Ok({tokens, begin, end, data})
    } else {
        let end = tokens->Array.length + cnt - 1
        let data = tokens->Array.slice(~start=begin, ~end=end+1)
        Ok({tokens, begin, end, data})
    }
}

let nonEmpty = (parser:parser<'t,array<'d>>):parser<'t,array<'d>> => {
    parser->mapRes(ds => ds->Array.length == 0 ? Error(()) : Ok(ds))
}

let end = (parser:parser<'t,'d>):parser<'t,'d> => inp => {
    switch parser(inp) {
        | Error(_) => Error(())
        | Ok(parsed) => {
            if (isEmpty(parsed->toInput)) {
                Ok(parsed)
            } else {
                Error(())
            }
        }
    }
}

let parse = (tokens:array<'t>, parser:()=>parser<'t,'d>):result<'d,unit> => {
    parser()(tokens->makeParserInput)->Result.map(({data}) => data)
}