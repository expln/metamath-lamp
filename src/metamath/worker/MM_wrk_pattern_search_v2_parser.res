type symSeq = {
    flags:string, 
    symbols:array<string>
}

type rec seqGrp =
    | SymSeq(array<symSeq>)
    | Ordered(array<seqGrp>)
    | Unordered(array<seqGrp>)


let sym:Parser.parser<string,string> = 
    Parser.match(str => !(str->String.includes("$")))

let seqWithoutParens:Parser.parser<string,symSeq> = 
    Parser.rep(sym, ~minCnt=1)->Parser.map(symbols => {flags:"", symbols})

let openSeqParen:Parser.parser<string,string> = 
    Parser.match(str => str->String.startsWith("$["))
let closeSeqParen:Parser.parser<string,string> = 
    Parser.match(str => str == "$]")
let seqWithParens:Parser.parser<string,symSeq> = 
    openSeqParen->Parser.andThen(seqWithoutParens)->Parser.andThen(closeSeqParen)
        ->Parser.map((((openParen,seq),_)) => {
            {
                flags: openParen->String.sliceToEnd(~start=2),
                symbols: seq.symbols
            }
        })

let symSeq:Parser.parser<string,symSeq> =
    Parser.any([seqWithoutParens, seqWithParens])