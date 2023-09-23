
type state = Balanced | Opened | Failed
type parenCnt

let parenCntMake: (
    ~parenMin:int,
    ~canBeFirstMin:int,
    ~canBeFirstMax:int,
    ~canBeLastMin:int,
    ~canBeLastMax:int,
) => parenCnt
let parenCntReset: parenCnt => unit
let parenCntPut: (parenCnt,int) => state
let parenCntCanBeFirst: (parenCnt, int) => bool
let parenCntCanBeLast: (parenCnt, int) => bool