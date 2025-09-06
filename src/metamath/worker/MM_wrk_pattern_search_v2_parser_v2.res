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

let isOpenParenthesis = (str:string):bool => str->String.startsWith(openParenthesis)
let isCloseParenthesis = (str:string):bool => str == closeParenthesis

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

module PatternParser = {
    open Parser_v2
    type parser<'d> = parser<string,'d>

    type seqOrOperator =
        | Seq(symSeq)
        | Operator(string)

    let openParen:parser<flags> =
        match(str => isOpenParenthesis(str) ? Some(String.substringToEnd(str, ~start=2)) : None)

    let closeParen:parser<unit> =
        match(str => isCloseParenthesis(str) ? Some(()) : None)

    let symbol:parser<string> =
        match(str => str->String.includes("$") ? None : Some(str))

    let symbols:parser<symSeq> =
        rep(symbol)->nonEmpty->map(seq => Symbols(seq)->toSymSeq)

    let operator:parser<string> =
        oneOf([operatorOrdered, operatorUnordered])

    let seqGrpForOp = (operator:string):Parser_v2.parser<seqOrOperator, array<symSeq>> =>
        seq2(
            rep(
                seq2(
                    match(elem => switch elem {|Seq(seq)=>Some(seq) |Operator(_)=>None}),
                    match(elem => switch elem {|Seq(_)=>None |Operator(op)=>op==operator?Some(()):None})
                )->map(((seq,_)) => seq)
            ),
            match(elem => switch elem {|Seq(seq)=>Some(seq) |Operator(_)=>None})
        )->map(((begin:array<symSeq>, end:symSeq)) => begin->Array.concat([end]))

    let unordered:Parser_v2.parser<seqOrOperator, symSeq> =
        seqGrpForOp(operatorUnordered)->map(elems => Unordered(elems)->toSymSeq)

    let ordered:Parser_v2.parser<seqOrOperator, symSeq> =
        seqGrpForOp(operatorUnordered)->map(elems => Ordered(elems)->toSymSeq)

    let seqGrp:Parser_v2.parser<seqOrOperator, symSeq> =
        any([unordered, ordered])
    
    

    let rec symSeq = ():parser<symSeq> =>
        _ => None
    and seqWithParens = ():parser<symSeq> =>
        seq3(
            match(str => isOpenParenthesis(str) ? Some(str->String.substringToEnd(~start=2)) : None),
            symSeq(),
            val(closeParenthesis)
        )->map(((flags,seq,_)) => {...seq, flags: mergeFlags(flags, seq.flags)})
    and operand = ():parser<symSeq> =>
        any([seqWithParens(), symbols])
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

    let subpattern:parser<subpat> =
        seq2(opt(match(isSubpatternBegin)), symSeq())
            ->map(((beginOpt, seq)) => makeSubpattern(beginOpt, seq))

    let pattern:parser<array<subpat>> =
        rep(subpattern)->nonEmpty->end
}

