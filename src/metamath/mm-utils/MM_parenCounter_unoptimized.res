open Common

type state = Balanced | Opened | Failed

type rec paren = {
    code: int,
    isOpen: bool,
    opposite: int,
}

type parenCnt = {
    min:int,
    parens: array<paren>,
    parentStack: array<paren>,
    mutable failed: bool
}

let parenCntMake = (parentheses, ~checkParensOptimized:bool=true) => {
    let parenLen = parentheses->Array.length
    if (mod(parenLen, 2)  != 0) {
        raise(MmException({msg:`mod(parenLen, 2)  != 0 in parenCntMake`}))
    } else if (parenLen == 0) {
        {
            min: 0,
            parens:[],
            parentStack: [],
            failed: false,
        }
    } else {
        let parens = []
        let maxI = parenLen / 2 - 1
        for i in 0 to maxI {
            let openCode = parentheses->Array.getUnsafe(i*2)
            let closeCode = parentheses->Array.getUnsafe(i*2+1)
            parens->Array.push({code: openCode, isOpen: true, opposite: closeCode})
            parens->Array.push({code: closeCode, isOpen: false, opposite: openCode})
        }
        let min = parentheses->Array.reduce(parentheses->Array.getUnsafe(0), (min,p) => if (min <= p) {min} else {p})
        if (checkParensOptimized && Math.Int.abs(min) != parenLen) {
            Console.log("Warning: parentheses are not optimized (this may slow down the unification process).")
        }
        {
            min,
            parens,
            parentStack: [],
            failed: false,
        }
    }
}

let parenCntReset: parenCnt => unit = cnt => {
    cnt.parentStack->Expln_utils_common.clearArray
    cnt.failed = false
}

let parenCntPut: (parenCnt,int) => state = (cnt,i) => {
    if (!cnt.failed && cnt.min <= i && i < 0) {
        switch cnt.parens->Array.find(({code}) => code == i) {
            | Some(paren) => {
                if (paren.isOpen) {
                    cnt.parentStack->Array.push(paren)
                } else {
                    switch cnt.parentStack->Array.pop {
                        | None => cnt.failed = true
                        | Some(lastParen) => {
                            if (lastParen.opposite != paren.code) {
                                cnt.failed = true
                            }
                        }
                    }
                }
            }
            | None => ()
        }
    }
    if (cnt.failed) {
        Failed
    } else if (cnt.parentStack->Array.length == 0) {
        Balanced
    } else {
        Opened
    }
}