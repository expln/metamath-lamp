open MM_parser

type state = Balanced | Opened | Failed

type parenCnt = {
    stack: array<int>,
    mutable len:int,
    mutable failed: bool,
    parenMin:int,
    canBeFirstMin:int,
    canBeFirstMax:int,
    canBeLastMin:int,
    canBeLastMax:int,
}

let parenCntStackPush = (cnt:parenCnt, i:int):unit => {
    if (cnt.stack->Js_array2.length == cnt.len) {
        for _ in 1 to 1000 {
            cnt.stack->Array.push(0)
        }
    }
    cnt.stack[cnt.len] = i
    cnt.len = cnt.len + 1
}

let parenCntMake = (
    ~parenMin:int,
    ~canBeFirstMin:int,
    ~canBeFirstMax:int,
    ~canBeLastMin:int,
    ~canBeLastMax:int,
):parenCnt => {
    if (mod(parenMin, 2)  != 0) {
        raise(MmException({msg:`mod(parenMin, 2)  != 0 in parenCntMake`}))
    } else {
        {
            stack: Expln_utils_common.createArray(1000),
            len:0,
            failed: false,
            parenMin,
            canBeFirstMin,
            canBeFirstMax,
            canBeLastMin,
            canBeLastMax,
        }
    }
}

let parenCntReset = (cnt:parenCnt):unit => {
    cnt.len = 0
    cnt.failed = false
}

let parenCntPut = (cnt:parenCnt, i:int):state => {
    if (cnt.failed) {
        Failed
    } else if (cnt.parenMin <= i && i < 0) {
        let isOpen = mod(i, 2) == -1
        if (isOpen) {
            cnt->parenCntStackPush(i)
            Opened
        } else if (cnt.len == 0 || cnt.stack->Array.getUnsafe(cnt.len-1) != i+1) {
            cnt.failed = true
            Failed
        } else {
            cnt.len = cnt.len - 1
            if (cnt.len == 0) {
                Balanced
            } else {
                Opened
            }
        }
    } else {
        if (cnt.len == 0) {
            Balanced
        } else {
            Opened
        }
    }
}

let parenCntCanBeFirst = (cnt:parenCnt, i:int):bool => {
    0 <= i /* is not a constant */
    || (cnt.parenMin < i && mod(i,2) == -1) /* is open paren */
    || (cnt.canBeFirstMin <= i && i <= cnt.canBeFirstMax)
}

let parenCntCanBeLast = (cnt:parenCnt, i:int):bool => {
    0 <= i /* is not a constant */
    || (cnt.parenMin <= i && mod(i,2) == 0) /* is close paren */
    || (cnt.canBeLastMin <= i && i <= cnt.canBeLastMax)
}