type flags = string
type rec symSeq = {flags: flags, elems: seqGrp}
and seqGrp = Symbols(array<string>) | Ordered(array<symSeq>) | Unordered(array<symSeq>)
type patternTarget = Frm | Hyps | Asrt
type pattern = {target: patternTarget, symSeq: symSeq}

let parsePattern: string => option<array<pattern>>
