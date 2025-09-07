type flags = {
    adj:option<bool>
}

type rec symSeq = {
    flags: flags, 
    elems: seqGrp
}
and seqGrp = 
    | Symbols(array<string>) 
    | Ordered(array<symSeq>) 
    | Unordered(array<symSeq>)

type patternTarget = Frm | Hyps | Asrt

type pattern = {
    target: patternTarget, 
    symSeq: symSeq
}

let parseFlags: string => flags

let passFlagsFromParentToChild: (flags, flags) => flags

let parsePattern: string => option<array<pattern>>
