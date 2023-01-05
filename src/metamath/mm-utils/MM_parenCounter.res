open MM_parser

type state = Balanced | Opened | Failed

type rec paren = {
    code: int,
    isOpen: bool,
    opposite: int,
}

type parenCnt = {
    parens: array<paren>,
    parentStack: array<paren>,
    mutable failed: bool
}

let parenCntMake = parentheses => {
    let parenLen = parentheses->Js_array2.length
    if (mod(parenLen, 2)  != 0) {
        raise(MmException({msg:`mod(parenLen, 2)  != 0`}))
    } else {
        let parens = []
        let maxI = parenLen / 2 - 1
        for i in 0 to maxI {
            let openCode = parentheses[i*2]
            let closeCode = parentheses[i*2+1]
            parens->Js_array2.push({code: openCode, isOpen: true, opposite: closeCode})->ignore
            parens->Js_array2.push({code: closeCode, isOpen: false, opposite: openCode})->ignore
        }
        {
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
    if (!cnt.failed) {
        switch cnt.parens->Js_array2.find(({code}) => code == i) {
            | Some(paren) => {
                if (paren.isOpen) {
                    cnt.parentStack->Js_array2.push(paren)->ignore
                } else {
                    switch cnt.parentStack->Js_array2.pop {
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
    } else if (cnt.parentStack->Js_array2.length == 0) {
        Balanced
    } else {
        Opened
    }
}