
type state = Balanced | Opened | Failed
type parenCnt

let parenCntMake: (array<int>, ~checkParensOptimized:bool=?) => parenCnt
let parenCntReset: parenCnt => unit
let parenCntPut: (parenCnt,int) => state