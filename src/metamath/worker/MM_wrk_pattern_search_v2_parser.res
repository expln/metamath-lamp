type patternTarget = Frm | Hyps | Asrt

type flags = {
    adj:option<bool>,
    target:option<patternTarget>,
    singleStmt:option<bool>,
}

type rec symSeq = {
    flags: flags, 
    elems: seqGrp
}
and seqGrp = 
    | Symbols(array<string>) 
    | Ordered(array<symSeq>) 
    | Unordered(array<symSeq>)
    | OneOf(array<symSeq>)

type pattern = {
    flags: flags, 
    neg:bool,
    symSeq: symSeq,
}

let operatorOrdered = "$*"
let operatorUnordered = "$/"
let operatorOneOf = "$|"
let openParenthesis = "$["
let closeParenthesis = "$]"

let flagAdj = "+"
let flagNonAdj = "-"
let flagHyps = "H"
let flagHyp = "h"
let flagAsrt = "a"
let flagSingleStmt = "s"
let flagNegation = "!"

let toSymSeq = (elems:seqGrp, ~flags:flags={adj:None, target:None, singleStmt:None}):symSeq => { flags, elems }

let flagSingleHyp = flagHyps ++ flagSingleStmt
let parseFlags = (str:string):flags => {
    let str = str->String.replaceAll(flagHyp, flagSingleHyp)
    {
        adj: str->String.includes(flagAdj) ? Some(true) : str->String.includes(flagNonAdj) ? Some(false) : None,
        target: str->String.includes(flagAsrt) ? Some(Asrt) 
            : str->String.includes(flagHyps) ? Some(Hyps) : None,
        singleStmt: str->String.includes(flagSingleStmt) ? Some(true) : None,
    }
}

let isPatternBegin = (str:string):option<pattern> => {
    if (
        str->String.startsWith("$")
        && !(
            str->String.startsWith(operatorOrdered)
            || str->String.startsWith(operatorUnordered)
            || str->String.startsWith(operatorOneOf)
            || str->String.startsWith(openParenthesis)
            || str->String.startsWith(closeParenthesis)
        )
    ) {
        Some({
            flags: parseFlags(str),
            neg:str->String.includes(flagNegation),
            symSeq: {
                flags: {adj:None, target:None, singleStmt:None},
                elems: Symbols([]),
            },
        })
    } else {
        None
    }
}

let passFlagsFromParentToChild = (parentFlags:flags, childFlags:flags):flags => {
    {
        adj: childFlags.adj->Option.orElse(parentFlags.adj),
        target: switch parentFlags.target {
            | Some(Asrt) | Some(Hyps) => parentFlags.target
            | None | Some(Frm) => childFlags.target 
        },
        singleStmt: switch parentFlags.singleStmt {
            | Some(true) => Some(true)
            | None | Some(false) => childFlags.singleStmt 
        },
    }
}

let makePattern = (beginOpt:option<pattern>, seq:symSeq):pattern => {
    switch beginOpt {
        | None => { flags:{adj:None, target:None, singleStmt:None}, neg:false, symSeq: seq, }
        | Some(stmtPat) => { ...stmtPat, symSeq: seq, }
    }
}

module PatternParser = {
    open Parser
    type parser<'d> = parser<string,'d>
    let enableLog = false
    let log = (parser,name) => if enableLog {Parser.log(parser,name,~tokenSep=" ")} else {parser}

    type seqOrOperator =
        | Seq(symSeq)
        | Operator(string)

    let openParen:parser<flags> =
        match(str => str->String.startsWith(openParenthesis) ? Some(parseFlags(str)) : None)
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
        oneOf([operatorOrdered, operatorUnordered, operatorOneOf])
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

    let oneOf:Parser.parser<seqOrOperator, symSeq> =
        seqGrpForOperator(operatorOneOf, any([unordered, ordered, seqOperand]), elems=>OneOf(elems))

    let seqGrpParser:Parser.parser<seqOrOperator, symSeq> =
        any([oneOf, unordered, ordered])

    let rec symSeq = ():parser<symSeq> =>
        anyL([seqGrp, seqWithParens, ()=>symbols])
        ->log("symSeq")
    and seqWithParens = ():parser<symSeq> =>
        seq3(openParen, symSeq(), closeParen)
        ->map(((flags,seq,_)) => {...seq, flags: passFlagsFromParentToChild(flags, seq.flags)})
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

    let singlePattern:parser<pattern> =
        seq2(opt(match(isPatternBegin)), symSeq())
        ->map(((beginOpt, seq)) => makePattern(beginOpt, seq))
        ->log("singlePattern")

    let nonSinglePattern:parser<pattern> =
        seq2(match(isPatternBegin), symSeq())
        ->map(((begin, seq)) => makePattern(Some(begin), seq))
        ->log("nonSinglePattern")

    let patterns:parser<array<pattern>> =
        any([
            rep(nonSinglePattern)->nonEmpty,
            singlePattern->map(pat => [pat]),
        ])->end
        ->log("patterns")
}

let parsePattern = (text:string):option<array<pattern>> => {
    Parser.parse(text->String.trim->Common.getSpaceSeparatedValuesAsArray, PatternParser.patterns)
}