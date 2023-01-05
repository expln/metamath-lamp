
type state = Balanced | Opened | Failed
type parenCnt

let parenCntMake: array<int> => parenCnt
let parenCntReset: parenCnt => unit
let parenCntPut: (parenCnt,int) => state