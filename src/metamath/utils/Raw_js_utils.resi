let isString: 'a => bool
let isBool: 'a => bool
let isArray: 'a => bool
let isObject: 'a => bool
let isFunction: 'a => bool
let reqStrExn: (Nullable.t<string>, string) => string
let optStrExn: (Nullable.t<string>, string) => option<string>
let reqBoolExn: (Nullable.t<bool>, string) => bool
let optBoolExn: (Nullable.t<bool>, string) => option<bool>
let reqArrExn: (Nullable.t<array<'b>>, string) => array<'b>
let reqObjExn: (Nullable.t<'a>, string) => 'a
let optObjExn: (Nullable.t<'a>, string) => option<'a>
let reqFuncExn: (Nullable.t<'a>, string) => 'a
let invokeExnFunc: (string, unit => 'a) => result<'a, string>
let executeFunctionBody: string => {..}
let executeAsyncFunctionBody: string => promise<{..}>
let makeFunction: (~args:string, ~body:string) => 'a