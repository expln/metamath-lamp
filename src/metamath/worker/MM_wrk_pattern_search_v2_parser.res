type rec symSeq = {
    flags:string,
    elems: seqGrp
}
and seqGrp =
    | Symbols(array<string>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

let toSymSeq = (elems:seqGrp, ~flags:string=""):symSeq => { flags, elems }

let isOpenParenthesis = (str:string) => str->String.startsWith("$[")
let isCloseParenthesis = (str:string) => str == "$]"

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
        seq3(openParen, ()=>take(-1), closeParen)->end
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
        match(isOpenParenthesis)->map(String.substringToEnd(_, ~start=2))
    and closeParen = ():parser<string,unit> =>
        match(isCloseParenthesis)->map(_ => ())
    and seqGrp = (operator:string):parser<string,array<symSeq>> =>
        seq2(
            ()=>rep(
                ~minCnt=1,
                ()=>seq2(
                    ()=>nonEmptyUntil(operator),
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
    and nonEmptyUntil = (until:string):parser<string,array<string>> => inp => {
        let {tokens, begin} = inp
        let res = []
        let i = ref(begin)
        let maxI = tokens->Array.length-1
        let parenCnt = ref(0)
        let continue = ref(true)
        while (i.contents <= maxI && continue.contents) {
            let token = tokens->Array.getUnsafe(i.contents)
            if (isOpenParenthesis(token)) {
                parenCnt := parenCnt.contents + 1
            } else if (isCloseParenthesis(token)) {
                parenCnt := parenCnt.contents - 1
            }
            if (parenCnt.contents < 0 || parenCnt.contents == 0 && token == until) {
                continue := false
            } else {
                res->Array.push(token)
            }
            i := i.contents + 1
        }
        let resLen = res->Array.length
        if (parenCnt.contents < 0 || resLen == 0) {
            Error(())
        } else {
            Ok({ tokens, begin, end: begin + resLen - 1, data: res })
        }
    }
    and nonEmpty = ():parser<string,array<string>> =>
        rep(()=>match(_ => true), ~minCnt=1)
    symSeq()
}

let parsePattern = (patternStr:string):result<symSeq,()> => {
    patternStr->String.trim->Common.getSpaceSeparatedValuesAsArray->Parser.makeParserInput->symSeq
        ->Result.map(({data}) => data)
}