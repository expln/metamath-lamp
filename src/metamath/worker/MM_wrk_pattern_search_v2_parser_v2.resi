type flags = string
type rec symSeq = {flags: flags, elems: seqGrp}
and seqGrp = Symbols(array<string>) | Ordered(array<symSeq>) | Unordered(array<symSeq>)
type subpatTarget = Frm | Hyps | Asrt
type subpat = {target: subpatTarget, symSeq: symSeq}

let parsePattern: string => option<array<subpat>>
