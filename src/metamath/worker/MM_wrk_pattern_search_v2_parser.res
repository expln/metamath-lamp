type rec symSeq = {
    flags:string,
    elems: seqGrp
}
and seqGrp =
    | Symbols(array<string>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

let toSymSeq = (elems:seqGrp, ~flags:string=""):symSeq => { flags, elems }

let symSeq:Parser.parser<string,symSeq> = {
    open Parser
    let rec symSeq = ():parser<string,symSeq> =>
        any([
            symbols,
            unordered,
            ordered,
            symSeqWithParens
        ])
    and sym = ():parser<string,string> =>
        match(str => !(str->String.includes("$")))
    and symbols = ():parser<string,symSeq> =>
        rep(sym, ~minCnt=1)->end->map(symbols => Symbols(symbols)->toSymSeq)
    and ordered = ():parser<string,symSeq> =>
        seqGrp("$**")->map(elems => Ordered(elems)->toSymSeq)
    and unordered = ():parser<string,symSeq> =>
        seqGrp("$||")->map(elems => Unordered(elems)->toSymSeq)
    and symSeqWithParens = ():parser<string,symSeq> =>
        seq3(openParen, ()=>allButLast, closeParen)->end
            ->mapRes(((flags, symbols, _)) => {
                switch symSeq()(symbols->makeParserInput) {
                    | Error(_) => Error(())
                    | Ok(parsed) => {
                        if (parsed.data.flags == "") {
                            Ok({...parsed.data, flags})
                        } else {
                            Ok(parsed.data)
                        }
                    }
                }
            })
    and openParen = ():parser<string,string> =>
        match(str => str->String.startsWith("$["))->map(str => str->String.substringToEnd(~start=2))
    and closeParen = ():parser<string,unit> =>
        match(str => str =="$]")->map(_ => ())
    and seqGrp = (operator:string):parser<string,array<symSeq>> =>
        seq2(
            ()=>rep(
                ~minCnt=1,
                ()=>seq2(
                    ()=>allUntil(operator),
                    ()=>match(str => str == operator)
                )->map(((operand:array<string>,_)) => operand)
            ),
            nonEmpty
        )->end
            ->map(((operands:array<array<string>>,lastOperand:array<string>)) => [...operands, lastOperand])
            ->mapRes(operands => {
                operands->Array.reduce(Ok([]), (res,operand:array<string>) => {
                    switch res {
                        | Error(_) => Error(())
                        | Ok(res) => {
                            switch symSeq()(operand->makeParserInput) {
                                | Error(_) => Error(())
                                | Ok(parsed) => {
                                    res->Array.push(parsed.data)
                                    Ok(res)
                                }
                            }
                        }
                    }
                })
            })
    and allUntil = (until:string):parser<string,array<string>> =>
        rep(()=>match(str => str != until), ~minCnt=1)
    and nonEmpty = ():parser<string,array<string>> =>
        rep(()=>match(_ => true), ~minCnt=1)
    symSeq()
}

let parsePattern = (patternStr:string):result<symSeq,()> => {
    patternStr->String.trim->Common.getSpaceSeparatedValuesAsArray->Parser.makeParserInput->symSeq
        ->Result.map(({data}) => data)
}