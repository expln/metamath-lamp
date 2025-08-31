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

let mapRes = (parser:parser<'t,'a>, func:parsed<'t,'a>=>'b):parser<'t,'b> => inp => {
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

let match = (predicate:'t=>bool):parser<'t,'t> => inp => {
    if (isEmpty(inp)) {
        Error(())
    } else {
        let token = inp.tokens->Array.getUnsafe(inp.begin)
        if (predicate(token)) {
            Ok({tokens:inp.tokens, begin:inp.begin, end:inp.begin, data:token})
        } else {
            Error(())
        }
    }
}

let rep = (parser:()=>parser<'t,'d>, ~minCnt:int=0):parser<'t,array<'d>> => inp => {
    let tokens = inp.tokens
    let begin = inp.begin
    let inp = ref(inp)
    let res:array<'p> = []
    let end = ref(-1)
    let mismatchFound = ref(false)
    while (!mismatchFound.contents) {
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

let end: parser<'t,()> = inp => {
    if (isEmpty(inp)) {
        let tokens = inp.tokens
        let numOfTokens = tokens->Array.length
        Ok({
            tokens,
            begin:numOfTokens, 
            end:numOfTokens,
            data: ()
        })
    } else {
        Error(())
    }
}