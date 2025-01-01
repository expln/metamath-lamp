
let isString: 'a => bool = %raw(`obj => typeof obj === 'string'`)
let isBool: 'a => bool = %raw(`obj => typeof obj === 'boolean'`)
let isArray: 'a => bool = %raw(`obj => Array.isArray(obj)`)
let isObject: 'a => bool = 
    %raw(`obj => obj !== undefined && obj !== null && !Array.isArray(obj) && typeof obj === 'object'`)
let isFunction: 'a => bool = %raw(`obj => typeof obj === 'function'`)

let reqExn = (nullable:Nullable.t<'a>, msg:string):'a => {
    switch nullable->Nullable.toOption {
        | None => Exn.raiseError(`A required attribute is missing: ${msg}`)
        | Some(value) => value
    }
}

let reqStrExn = (nullable:Nullable.t<'a>, msg:string):string => {
    let res = reqExn(nullable, msg)
    if (!isString(res)) {
        Exn.raiseError(`Not a string: ${msg}`)
    } else {
        res
    }
}

let optStrExn = (nullable:Nullable.t<'a>, msg:string):option<string> => {
    switch nullable->Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isString(res)) {
                Exn.raiseError(`Not a string: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqBoolExn = (nullable:Nullable.t<'a>, msg:string):bool => {
    let res = reqExn(nullable, msg)
    if (!isBool(res)) {
        Exn.raiseError(`Not a boolean: ${msg}`)
    } else {
        res
    }
}

let optBoolExn = (nullable:Nullable.t<'a>, msg:string):option<bool> => {
    switch nullable->Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isBool(res)) {
                Exn.raiseError(`Not a boolean: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqArrExn = (nullable:Nullable.t<'a>, msg:string):array<'b> => {
    let res = reqExn(nullable, msg)
    if (!isArray(res)) {
        Exn.raiseError(`Not an array: ${msg}`)
    } else {
        res
    }
}

let reqObjExn = (nullable:Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isObject(res)) {
        Exn.raiseError(`Not an object: ${msg}`)
    } else {
        res
    }
}

let optObjExn = (nullable:Nullable.t<'a>, msg:string):option<'a> => {
    switch nullable->Nullable.toOption {
        | None => None
        | Some(res) => {
            if (!isObject(res)) {
                Exn.raiseError(`Not an object: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

let reqFuncExn = (nullable:Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isFunction(res)) {
        Exn.raiseError(`Not a function: ${msg}`)
    } else {
        res
    }
}

let invokeExnFunc = (title:string, func:unit=>'a):result<'a,string> => {
    switch Common.catchExn(func) {
        | Ok(res) => Ok(res)
        | Error({msg,stack}) => Error(`${title}: ${msg}.${stack!=""?("\n"++stack):""}`)
    }
}

let executeFunctionBody: string => {..} = %raw(`body => new Function("", body)()`)
let executeAsyncFunctionBody: string => promise<{..}> = %raw(`
body => {
    const AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;
    return (new AsyncFunction(body))();
}
`)
