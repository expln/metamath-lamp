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

    let openParen:parser<flags> =
        match(str => isOpenParenthesis(str) ? Some(String.substringToEnd(str, ~start=2)) : None)

    let closeParen:parser<unit> =
        match(str => isCloseParenthesis(str) ? Some(()) : None)

    let rec symSeq = ():parser<symSeq> =>
        _ => None

    let subpattern:parser<subpat> =
        seq2(opt(match(isSubpatternBegin)), symSeq())
            ->map(((beginOpt, seq)) => makeSubpattern(beginOpt, seq))

    let pattern:parser<array<subpat>> =
        rep(subpattern)->nonEmpty->end
}

