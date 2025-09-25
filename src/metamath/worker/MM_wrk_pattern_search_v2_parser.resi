type rec symSeq = {
    flags: flags, 
    elems: seqGrp
}
and seqGrp = 
    | Symbols(array<string>) 
    | Ordered(array<symSeq>) 
    | Unordered(array<symSeq>)
    | OneOf(array<symSeq>)
and patternTarget = Frm | Hyps | Asrt
and flags = {
    adj:option<bool>,
    target:patternTarget,
    singleStmt:bool,
}

type pattern = {
    symSeq: symSeq
}

let parseFlags: string => flags

let passFlagsFromParentToChild: (flags, flags) => flags

let parsePattern: string => option<array<pattern>>

let operatorOrdered:string
let operatorUnordered:string
let operatorOneOf:string
let openParenthesis:string
let closeParenthesis:string

let isPatternBegin: string => option<pattern>