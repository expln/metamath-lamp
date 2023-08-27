open MM_syntax_tree

type rec selection = {
    "text": string,
    "children": array<selection>,
}

type fragmentTransformState = string
type reactElemDto = string

type fragmentTransform = {
    canApply: {"selection":selection} => bool,
    displayName: {"selection":selection} => string,
    createInitialState: {"selection":selection} => fragmentTransformState,
    renderDialog: 
        {
            "selection":selection, 
            "state":fragmentTransformState, 
            "setState":(fragmentTransformState => fragmentTransformState) => unit
        } => reactElemDto,
}

external stateToObj: fragmentTransformState => {..} = "%identity"
external reactElemDtoToObj: reactElemDto => {..} = "%identity"
external objToObj: {..} => {..} = "%identity"
external objToFragmentTransformState: {..} => fragmentTransformState = "%identity"

let unsafeFunc = (title:string, func:unit=>'a):result<'a,string> => {
    try {
        Ok(func())
    } catch {
        | Js.Exn.Error(exn) => {
            let errMsg = `${title}: ${exn->Js.Exn.message->Belt_Option.getWithDefault("unknown error.")}.`
            Error(errMsg)
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

let optExn = (nullable:Js.Nullable.t<'a>):option<'a> => {
    nullable->Js.Nullable.toOption
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

let reqBoolExn = (nullable:Js.Nullable.t<'a>, msg:string):bool => {
    let res = reqExn(nullable, msg)
    if (!isBool(res)) {
        Js_exn.raiseError(`Not a boolean: ${msg}`)
    } else {
        res
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

let reqFuncExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isFunction(res)) {
        Js_exn.raiseError(`Not a function: ${msg}`)
    } else {
        res
    }
}

let optStrExn = (nullable:Js.Nullable.t<'a>, msg:string):option<string> => {
    switch optExn(nullable) {
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

let rec syntaxTreeToSelection = (tree:childNode):selection => {
    let text = tree->syntaxTreeToText
    {
        "text": text,
        "children":
            switch tree {
                | Symbol(_) => []
                | Subtree({children}) => children->Js.Array2.map(syntaxTreeToSelection)
            },
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