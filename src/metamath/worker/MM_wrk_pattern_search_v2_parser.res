type rec symSeq = {
    flags:string,
    elems: seqGrp
}
and seqGrp =
    | Symbols(array<string>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

let toSymSeq = (elems:seqGrp):symSeq => {
    {
        flags:"",
        elems
    }
}

let end = (parser:()=>Parser.parser<'t,'d>):(()=>Parser.parser<'t,'d>) => () => {
    Parser.seq2(parser, () => Parser.end)->Parser.map(((res,_)) => res)
}

let rec symSeqEnd:()=>Parser.parser<string,symSeq> = () =>
    Parser.any([
        symbols->end,
        unordered->end,
        ordered->end,
    ])
and symSeq:()=>Parser.parser<string,symSeq> = () =>
    Parser.any([
        symbols,
        unordered,
        ordered,
    ])
and sym: () => Parser.parser<string,string> = () =>
    Parser.match(str => !(str->String.includes("$")))
and symbols: ()=>Parser.parser<string,symSeq> = () =>
    Parser.rep(sym, ~minCnt=1)->Parser.map(symbols => {flags:"", elems:Symbols(symbols)})
and ordered: ()=>Parser.parser<string,symSeq> = () =>
    seqGrp("$**")->Parser.map(elems => Ordered(elems)->toSymSeq)
and unordered: ()=>Parser.parser<string,symSeq> = () =>
    seqGrp("$||")->Parser.map(elems => Unordered(elems)->toSymSeq)
and seqGrp: string=>Parser.parser<string,array<symSeq>> = operator =>
    Parser.seq2(
        ()=>Parser.rep(
            ~minCnt=1,
            ()=>Parser.seq2(
                symSeq, 
                ()=>Parser.match(str => str == operator)
            )->Parser.map(((seq,_)) => seq)
        ),
        symSeq
    )->Parser.map(((elems,lastElem)) => [...elems, lastElem])

let parsePattern = (patternStr:string):result<symSeq,()> => {
    symSeqEnd()(patternStr->String.trim->Common.getSpaceSeparatedValuesAsArray->Parser.makeParserInput)
        ->Result.map(({data}) => data)
}

// let seqWithoutParens:Parser.parser<string,symSeq> = 
//     Parser.rep(sym, ~minCnt=1)->Parser.map(symbols => {flags:"", symbols})

// let openSeqParen:Parser.parser<string,string> = 
//     Parser.match(str => str->String.startsWith("$["))
// let closeSeqParen:Parser.parser<string,string> = 
//     Parser.match(str => str == "$]")
// let seqWithParens:Parser.parser<string,symSeq> = 
//     openSeqParen->Parser.andThen(seqWithoutParens)->Parser.andThen(closeSeqParen)
//         ->Parser.map((((openParen,seq),_)) => {
//             {
//                 flags: openParen->String.sliceToEnd(~start=2),
//                 symbols: seq.symbols
//             }
//         })

// let symSeq:Parser.parser<string,symSeq> =
//     Parser.any([seqWithoutParens, seqWithParens])