type flags = string

type rec symSeq = {
    flags:flags,
    elems:seqGrp
}
and seqGrp =
    | Symbols(array<string>)
    | Ordered(array<symSeq>)
    | Unordered(array<symSeq>)

type subpatTarget = Frm | Hyps | Asrt

type subpat = {
    target: subpatTarget,
    symSeq: symSeq,
}

let operatorOrdered = "$**"
let operatorUnordered = "$||"
let openParenthesis = "$["
let closeParenthesis = "$]"

let toSymSeq = (elems:seqGrp, ~flags:string=""):symSeq => { flags, elems }

let isSubpatternBegin = (str:string):option<subpat> => {
    if (
        str->String.startsWith("$")
        && !(
            str->String.startsWith(operatorOrdered)
            || str->String.startsWith(operatorUnordered)
            || str->String.startsWith(openParenthesis)
            || str->String.startsWith(closeParenthesis)
        )
    ) {
        Some({
            target: 
                if (str->String.includes("h")) {
                    Hyps
                } else if (str->String.includes("a")) {
                    Asrt
                } else {
                    Frm
                },
            symSeq: {
                flags: str
                    ->String.replaceAll("$", "")
                    ->String.replaceAll("h", "")
                    ->String.replaceAll("a", ""),
                elems: Symbols([])
            },
        })
    } else {
        None
    }
}

let mergeFlags = (parentFlags:string, childFlags:string):string => {
    childFlags->String.length == 0 ? parentFlags : childFlags
}

let makeSubpattern = (beginOpt:option<subpat>, seq:symSeq):subpat => {
    switch beginOpt {
        | None => { target: Frm, symSeq: seq }
        | Some(stmtPat) => {
            { 
                target: stmtPat.target, 
                symSeq: {
                    ...seq,
                    flags: mergeFlags(stmtPat.symSeq.flags, seq.flags)
                }, 
            }
        }
    }
}

let inpToStr = (inp:Parser.parserInput<string>, cnt:int):string => {
    inp.tokens->Array.slice(~start=inp.begin, ~end=inp.begin+cnt)->Array.join(" ")
}

let logParsers = false
let log = (parser:Parser.parser<string,'d>, name:string):Parser.parser<string,'d> => {
    if (logParsers) {
        let tokensToPrint = 10
        parser->Parser.withCallbacks(
            ~before=inp=>Console.log(`${name} trying: '${inpToStr(inp, tokensToPrint)}'`),
            ~onSuccess=parsed=>Console.log(`${name} parsed: ${Expln_utils_common.stringify(parsed.data)}`),
            ~onFail=inp=>Console.log(`${name} failed: '${inpToStr(inp, tokensToPrint)}'`),
        )
    } else {
        parser
    }
}

module PatternParser = {
    open Parser
    type parser<'d> = parser<string,'d>

    type seqOrOperator =
        | Seq(symSeq)
        | Operator(string)

    let openParen:parser<flags> =
        match(str => str->String.startsWith(openParenthesis) ? Some(String.substringToEnd(str, ~start=2)) : None)
        ->log("openParen")

    let closeParen:parser<unit> =
        val(closeParenthesis)->map(_=>())
        ->log("closeParen")

    let symbol:parser<string> =
        match(str => str->String.includes("$") ? None : Some(str))
        ->log("symbol")

    let symbols:parser<symSeq> =
        rep(symbol)->nonEmpty->map(seq => Symbols(seq)->toSymSeq)
        ->log("symbols")

    let operator:parser<string> =
        oneOf([operatorOrdered, operatorUnordered])
        ->log("operator")

    let seqOperand:Parser.parser<seqOrOperator, symSeq> =
        match(elem => switch elem {|Seq(seq)=>Some(seq) |Operator(_)=>None})

    let seqGrpForOperator = (
        operator:string, 
        operand:Parser.parser<seqOrOperator, symSeq>,
        makeGrp:array<symSeq>=>seqGrp
    ):Parser.parser<seqOrOperator, symSeq> =>
        seq2(
            rep(
                seq2(
                    operand,
                    match(elem => switch elem {|Seq(_)=>None |Operator(op)=>op==operator?Some(()):None})
                )->map(((seq,_)) => seq)
            )->nonEmpty,
            operand
        )->map(((begin:array<symSeq>, end:symSeq)) => begin->Array.concat([end]))
        ->map(elems => makeGrp(elems)->toSymSeq)

    let ordered:Parser.parser<seqOrOperator, symSeq> =
        seqGrpForOperator(operatorOrdered, seqOperand, elems=>Ordered(elems))

    let unordered:Parser.parser<seqOrOperator, symSeq> =
        seqGrpForOperator(operatorUnordered, any([ordered, seqOperand]), elems=>Unordered(elems))

    let seqGrpParser:Parser.parser<seqOrOperator, symSeq> =
        any([unordered, ordered])

    let rec symSeq = ():parser<symSeq> =>
        anyL([seqGrp, seqWithParens, ()=>symbols])
        ->log("symSeq")
    and seqWithParens = ():parser<symSeq> =>
        seq3(
            openParen, symSeq(), closeParen
        )->map(((flags,seq,_)) => {...seq, flags: mergeFlags(flags, seq.flags)})
        ->log("seqWithParens")
    and operand = ():parser<symSeq> =>
        any([seqWithParens(), symbols])
        ->log("operand")
    and operators = ():parser<array<seqOrOperator>> =>
        seq2(
            rep(
                seq2(
                    operand(),
                    operator
                )->map(((seq:symSeq,op:string)) => [Seq(seq), Operator(op)])
            )->nonEmpty->map(Array.concatMany([], _)),
            operand()
        )->map(((begin:array<seqOrOperator>,end:symSeq)) => Array.concat(begin, [Seq(end)]))
        ->log("operators")
    and seqGrp = ():parser<symSeq> =>
        operators()->flatMap(_ => seqGrpParser)
        ->log("seqGrp")

    let subpattern:parser<subpat> =
        seq2(opt(match(isSubpatternBegin)), symSeq())
        ->map(((beginOpt, seq)) => makeSubpattern(beginOpt, seq))
        ->log("subpattern")

    let pattern:parser<array<subpat>> =
        rep(subpattern)->nonEmpty->end
        ->log("pattern")
}

let parsePattern = (text:string):option<array<subpat>> => {
    Parser.parse(text->String.trim->Common.getSpaceSeparatedValuesAsArray, PatternParser.pattern)
}