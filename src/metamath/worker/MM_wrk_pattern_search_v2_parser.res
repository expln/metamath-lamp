type rec symSeq = {
    flags:string,
    elems: seqGrp
}
and seqGrp =
    | Symbols(array<string>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

type patTarget = Frm | Hyps | Asrt

type stmtPat = {
    target: patTarget,
    symSeq: symSeq,
}

let orderedOperator = "$**"
let unorderedOperator = "$||"
let openParen = "$["
let closeParen = "$]"

let toSymSeq = (elems:seqGrp, ~flags:string=""):symSeq => { flags, elems }

let isOpenParenthesis = (str:string) => str->String.startsWith(openParen)
let isCloseParenthesis = (str:string) => str == closeParen

let openParen:Parser.parser<string,string> =
    Parser.match(isOpenParenthesis)->Parser.map(String.substringToEnd(_, ~start=2))

let closeParen:Parser.parser<string,unit> =
    Parser.match(isCloseParenthesis)->Parser.map(_ => ())

let nonEmptyUntil = (until:string):Parser.parser<string,array<string>> => inp => {
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

let symSeq:()=>Parser.parser<string,symSeq> = {
    open Parser
    let rec symSeq = ():parser<string,symSeq> =>
        any([symbols, symSeqWithParens, unordered, ordered])->end
    and sym = ():parser<string,string> =>
        match(str => !(str->String.includes("$")))
    and symbols = ():parser<string,symSeq> =>
        rep(sym, ~minCnt=1)->end->map(symbols => Symbols(symbols)->toSymSeq)
    and symSeqWithParens = ():parser<string,symSeq> =>
        seq3(()=>openParen, ()=>take(-1), ()=>closeParen)->end
            ->mapRes(((flags, symbols, _)) => {
                switch parse(symbols, symSeq) {
                    | Error(_) => Error(())
                    | Ok(seq) => if (seq.flags == "") { Ok({...seq, flags}) } else { Ok(seq) }
                }
            })
    and ordered = ():parser<string,symSeq> =>
        seqGrp(orderedOperator)->map(elems => Ordered(elems)->toSymSeq)
    and unordered = ():parser<string,symSeq> =>
        seqGrp(unorderedOperator)->map(elems => Unordered(elems)->toSymSeq)
    and seqGrp = (operator:string):parser<string,array<symSeq>> =>
        seq2(
            ()=>rep(
                ~minCnt=1,
                ()=>seq2(
                    ()=>nonEmptyUntil(operator),
                    ()=>match(str => str == operator)
                )->map(((operand:array<string>,_)) => operand)
            ),
            ()=>rep(()=>Parser.match(_ => true))->nonEmpty
        )->end
            ->map(((operands:array<array<string>>,lastOperand:array<string>)) => [...operands, lastOperand])
            ->mapRes((operands:array<array<string>>) => {
                operands->Array.reduce(Ok([]), (res,operand:array<string>) => {
                    switch res {
                        | Error(_) => Error(())
                        | Ok(res) => {
                            switch parse(operand, symSeq) {
                                | Error(_) => Error(())
                                | Ok(seq) => {
                                    res->Array.push(seq)
                                    Ok(res)
                                }
                            }
                        }
                    }
                })
            })
    symSeq
}

let parseSymSeq = (text:string):result<symSeq,()> => {
    Parser.parse( text->String.trim->Common.getSpaceSeparatedValuesAsArray, symSeq )
}

// let isPatternBegin = (str:string):option<stmtPat> => {
//     if (
//         str->String.startsWith("$")
//         && !(
//             str->String.startsWith(orderedOperator)
//             || str->String.startsWith(unorderedOperator)
//             || str->String.startsWith(openParen)
//             || str->String.startsWith(closeParen)
//         )
//     ) {

//     } else {
//         None
//     }

// }

// let patternParser:()=>Parser.parser<string,array<stmtPat>> = {
//     open Parser
//     let rec pattern = ():parser<string,array<stmtPat>> =>
//         any([symbols, symSeqWithParens, unordered, ordered])
//     and patBegin = ():parser<string,array<stmtPat>> =>
//         any([symbols, symSeqWithParens, unordered, ordered])
//     pattern
// }

// let parsePattern = (text:string):result<array<stmtPat>,()> => {
//     Parser.parse( text->String.trim->Common.getSpaceSeparatedValuesAsArray, patternParser )
// }