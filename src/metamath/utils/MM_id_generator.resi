type idGenState
let idGenMake: unit => idGenState
let idGenNext: idGenState => (idGenState, string)
