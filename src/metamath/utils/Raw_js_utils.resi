let isString: 'a => bool
let isBool: 'a => bool
let isArray: 'a => bool
let isObject: 'a => bool
let isFunction: 'a => bool
let reqStrExn: (Js.Nullable.t<string>, string) => string
let optStrExn: (Js.Nullable.t<string>, string) => option<string>
let reqBoolExn: (Js.Nullable.t<bool>, string) => bool
let optBoolExn: (Js.Nullable.t<bool>, string) => option<bool>
let reqArrExn: (Js.Nullable.t<array<'b>>, string) => array<'b>
let reqObjExn: (Js.Nullable.t<'a>, string) => 'a
let optObjExn: (Js.Nullable.t<'a>, string) => option<'a>
let reqFuncExn: (Js.Nullable.t<'a>, string) => 'a
let invokeExnFunc: (string, unit => 'a) => result<'a, string>
let executeFunctionBody: string => {..}
