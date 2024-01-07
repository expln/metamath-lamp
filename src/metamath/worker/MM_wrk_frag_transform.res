
type fragmentTransformState = string
type reactElemDto = string

type fragmentTransform = {
    canApply: {"step":Js_json.t} => bool,
    displayName: {"step":Js_json.t} => string,
    createInitialState: {"step":Js_json.t} => fragmentTransformState,
    renderDialog: 
        {
            "state":fragmentTransformState, 
            "setState":(fragmentTransformState => fragmentTransformState) => unit
        } => reactElemDto,
}

external reactElemDtoToObj: reactElemDto => {..} = "%identity"
external objToObj: {..} => {..} = "%identity"
external objToFragmentTransformState: {..} => fragmentTransformState = "%identity"

let unsafeFunc = (title:string, func:unit=>'a):result<'a,string> => {
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

let reqExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    switch nullable->Js.Nullable.toOption {
        | None => Js_exn.raiseError(`A required attribute is missing: ${msg}`)
        | Some(value) => value
    }
}

let isString: 'a => bool = %raw(`obj => typeof obj === 'string'`)
let isBool: 'a => bool = %raw(`obj => typeof obj === 'boolean'`)
let isArray: 'a => bool = %raw(`obj => Array.isArray(obj)`)
let isObject: 'a => bool = %raw(`obj => obj !== undefined && obj !== null && !Array.isArray(obj) && typeof obj === 'object'`)
let isFunction: 'a => bool = %raw(`obj => typeof obj === 'function'`)

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

let createTransformFromObject = (obj:{..}):fragmentTransform => {
    {
        canApply: selection => obj["canApply"](. selection),
        displayName: selection => obj["displayName"](. selection),
        createInitialState: selection => obj["createInitialState"](. selection),
        renderDialog: params => obj["renderDialog"](. params),
    }
}

let stringToFragTransformsUnsafe: string => {..} = %raw(`body => new Function("", body)()`)

let stringToFragTransforms = (str:string):result<array<fragmentTransform>,string> => {
    let transforms = try {
        Ok(stringToFragTransformsUnsafe(str))
    } catch {
        | Js.Exn.Error(exn) => {
            Error( 
                exn->Js.Exn.message
                    ->Belt_Option.mapWithDefault(
                        "[1] Cannot parse transforms.", 
                        msg => `[1] Cannot parse transforms: ${msg}`
                    )
            )
        }
        | _ => Error( "[2] Cannot parse transforms." )
    }
    try {
        switch transforms {
            | Error(msg) => Error(msg)
            | Ok(transforms) => Ok(transforms["map"](. createTransformFromObject))
        }
    } catch {
        | Js.Exn.Error(exn) => {
            Error( 
                exn->Js.Exn.message
                    ->Belt_Option.mapWithDefault(
                        "[3] Cannot parse transforms.", 
                        msg => `[3] Cannot parse transforms: ${msg}`
                    )
            )
        }
        | _ => Error( "[4] Cannot parse transforms." )
    }
}

let arrStrToFragTransforms = (texts:array<string>):result<array<fragmentTransform>,string> => {
    texts->Js_array2.reduce(
        (res,text) => {
            switch res {
                | Error(_) => res
                | Ok(arr) => {
                    switch stringToFragTransforms(text) {
                        | Error(msg) => Error(msg)
                        | Ok(newArr) => Ok(arr->Js_array2.concat(newArr))
                    }
                }
            }
        },
        Ok([])
    )
}