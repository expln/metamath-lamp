
let isString: 'a => bool = %raw(`obj => typeof obj === 'string'`)
let isBool: 'a => bool = %raw(`obj => typeof obj === 'boolean'`)
let isArray: 'a => bool = %raw(`obj => Array.isArray(obj)`)
let isObject: 'a => bool = 
    %raw(`obj => obj !== undefined && obj !== null && !Array.isArray(obj) && typeof obj === 'object'`)
let isFunction: 'a => bool = %raw(`obj => typeof obj === 'function'`)

let reqExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    switch nullable->Js.Nullable.toOption {
        | None => Js_exn.raiseError(`A required attribute is missing: ${msg}`)
        | Some(value) => value
    }
}

let reqStrExn = (nullable:Js.Nullable.t<'a>, msg:string):string => {
    let res = reqExn(nullable, msg)
    if (!isString(res)) {
        Js_exn.raiseError(`Not a string: ${msg}`)
    } else {
        res
    }
}

let optStrExn = (nullable:Js.Nullable.t<'a>, msg:string):option<string> => {
    switch nullable->Js.Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isString(res)) {
                Js_exn.raiseError(`Not a string: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqBoolExn = (nullable:Js.Nullable.t<'a>, msg:string):bool => {
    let res = reqExn(nullable, msg)
    if (!isBool(res)) {
        Js_exn.raiseError(`Not a boolean: ${msg}`)
    } else {
        res
    }
}

let optBoolExn = (nullable:Js.Nullable.t<'a>, msg:string):option<bool> => {
    switch nullable->Js.Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isBool(res)) {
                Js_exn.raiseError(`Not a boolean: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqArrExn = (nullable:Js.Nullable.t<'a>, msg:string):array<'b> => {
    let res = reqExn(nullable, msg)
    if (!isArray(res)) {
        Js_exn.raiseError(`Not an array: ${msg}`)
    } else {
        res
    }
}

let reqObjExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isObject(res)) {
        Js_exn.raiseError(`Not an object: ${msg}`)
    } else {
        res
    }
}

let optObjExn = (nullable:Js.Nullable.t<'a>, msg:string):option<'a> => {
    switch nullable->Js.Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isObject(res)) {
                Js_exn.raiseError(`Not an object: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqFuncExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isFunction(res)) {
        Js_exn.raiseError(`Not a function: ${msg}`)
    } else {
        res
    }
}

let invokeExnFunc = (title:string, func:unit=>'a):result<'a,string> => {
    try {
        Ok(func())
    } catch {
        | Js.Exn.Error(exn) => {
            let errMsg = `${title}: ${exn->Js.Exn.message->Belt_Option.getWithDefault("unknown error.")}.`
            let stack = exn->Js.Exn.stack->Belt_Option.getWithDefault("")
            Error([errMsg,stack]->Js.Array2.joinWith("\n"))
        }
        | _ => {
            let errMsg = `${title}: unknown error.`
            Error(errMsg)
        }
    }
}

let executeFunctionBody: string => {..} = %raw(`body => new Function("", body)()`)
