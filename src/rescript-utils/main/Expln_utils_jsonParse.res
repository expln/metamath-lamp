let {exn} = module(Expln_utils_common)

type path = list<string>

type jsonAny = 
    | JsonNull(path)
    | JsonBool(bool, path)
    | JsonNum(float, path)
    | JsonStr(string, path)
    | JsonArr(array<JSON.t>, path)
    | JsonObj(Dict.t<JSON.t>, path)

let rootPath = list{}

let pathToStr = path => {
    switch path {
        | list{} => "/"
        | _ => path->Belt_List.reduceReverse("", (a,e) => a ++ "/" ++ e)
    }
}

let jsonToAny = (json:JSON.t, path:path):jsonAny => {
    switch json->JSON.Classify.classify {
        | Null => JsonNull(path)
        | Bool(b) => JsonBool(b,path)
        | Number(num) => JsonNum(num,path)
        | String(str) => JsonStr(str,path)
        | Array(arr) => JsonArr(arr,path)
        | Object(dict) => JsonObj(dict,path)
    }
}
    

let getPath = jsonAny => {
    switch jsonAny {
        | JsonNull(path) | JsonBool(_,path) | JsonNum(_,path) | JsonStr(_,path) | JsonArr(_,path) | JsonObj(_,path) => path
    }
}

let pathToStr2 = (path,attrName) => pathToStr(list{attrName, ...path})

let getLocation2 = (jsonAny,nextPathElem) => pathToStr2(getPath(jsonAny),nextPathElem)

let anyToBool = (jsonAny):result<option<bool>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonBool(val,_) => Ok(Some(val))
        | _ => Error(`a boolean value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToNum = (jsonAny):result<option<float>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonNum(val,_) => Ok(Some(val))
        | _ => Error(`a number value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToInt = (jsonAny):result<option<int>,string> => {
    anyToNum(jsonAny)->Belt.Result.map(numOpt => numOpt->Belt_Option.map(num => num->Belt_Float.toInt))
}

let anyToStr = (jsonAny):result<option<string>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonStr(val,_) => Ok(Some(val))
        | _ => Error(`a string value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToArr = (jsonAny, mapper: jsonAny=>'a):result<option<array<'a>>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonArr(val,path) => {
            Ok(Some(
                val->Array.mapWithIndex((json,i) => mapper(jsonToAny(json,list{i->Belt.Int.toString, ...path})))
            ))
        }
        | _ => Error(`an array was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToObj = (jsonAny, mapper: jsonAny=>'a):result<option<'a>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonObj(_,_) => Ok(Some(mapper(jsonAny)))
        | _ => Error(`an object was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToJson = (jsonAny):result<option<JSON.t>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonBool(bool, _) => Ok(Some(bool->JSON.Encode.bool))
        | JsonNum(float, _) => Ok(Some(float->JSON.Encode.float))
        | JsonStr(string, _) => Ok(Some(string->JSON.Encode.string))
        | JsonArr(array, _) => Ok(Some(array->JSON.Encode.array))
        | JsonObj(dict, _) => Ok(Some(dict->JSON.Encode.object))
    }
}

let getByPath = (obj:jsonAny, attrName: string):result<option<jsonAny>,string> => {
    switch obj {
        | JsonObj(dict,path) => {
            switch dict->Dict.get(attrName) {
                | Some(json) => Ok(Some(jsonToAny(json,list{attrName, ...path})))
                | None => Ok(None)
            }
        }
        | _ => Error(`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

type anyToVal<'v> = jsonAny=>result<option<'v>,string>

type validator<'v> = 'v => result<'v,string>
type default<'v> = () => 'v
type asVal<'v> = ((jsonAny, ~validator:validator<'v>=?, ~default:default<'v>=?)=>'v)
type val<'v> = ((jsonAny, string, ~validator:validator<'v>=?, ~default:default<'v>=?)=>'v)

let validate = (val:'v, validator:option<validator<'v>>):'v => {
    switch validator {
        | None => val
        | Some(validator) => {
            switch validator(val) {
                | Ok(val) => val
                | Error(msg) => exn(msg)
            }
        }
    }
}

let getDefaultOrExn = (default:option<default<'v>>, errMsg:()=>string):'v => {
    switch default {
        | Some(default) => default()
        | None => exn(errMsg())
    }
}

let makeAsValOpt = (anyToVal:anyToVal<'v>):asVal<option<'v>> => {
    (jsonAny, ~validator=?, ~default=?) => {
        switch anyToVal(jsonAny) {
            | Ok(valOpt) => validate(valOpt, validator)
            | Error(msg) => getDefaultOrExn(default, ()=>msg)
        }
    }
}

let makeAsVal = (anyToVal:anyToVal<'v>, descrOfExpectedValue:string):asVal<'v> => {
    (jsonAny, ~validator=?, ~default=?) => {
        switch anyToVal(jsonAny) {
            | Ok(None) => getDefaultOrExn(default, ()=>`${descrOfExpectedValue} was expected at '${jsonAny->getPath->pathToStr}'.`)
            | Ok(Some(val)) => validate(val, validator)
            | Error(msg) => getDefaultOrExn(default, ()=>msg)
        }
    }
}

let makeValOpt = (asValOpt:asVal<option<'v>>):val<option<'v>> => {
    (jsonAny, attrName, ~validator=?, ~default=?) => {
        switch getByPath(jsonAny, attrName) {
            | Ok(None) => validate(None, validator)
            | Ok(Some(attrVal)) => asValOpt(attrVal, ~validator?, ~default?)
            | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${jsonAny->getPath->pathToStr}'.`)
        }
    }
}

let makeVal = (asVal:asVal<'v>, descrOfExpectedValue:string):val<'v> => {
    (jsonAny, attrName, ~validator=?, ~default=?) => {
        switch getByPath(jsonAny, attrName) {
            | Ok(None) => getDefaultOrExn(default, ()=>`${descrOfExpectedValue} was expected at '${getLocation2(jsonAny, attrName)}'.`)
            | Ok(Some(attrVal)) => asVal(attrVal, ~validator?, ~default?)
            | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${jsonAny->getPath->pathToStr}'.`)
        }
    }
}

let asBoolOpt = makeAsValOpt(anyToBool)
let asBool = makeAsVal(anyToBool, "a boolean")
let boolOpt = makeValOpt(asBoolOpt)
let bool = makeVal(asBool, "a boolean")

let asNumOpt = makeAsValOpt(anyToNum)
let asNum = makeAsVal(anyToNum, "a number")
let numOpt = makeValOpt(asNumOpt)
let num = makeVal(asNum, "a number")

let asIntOpt = makeAsValOpt(anyToInt)
let asInt = makeAsVal(anyToInt, "an integer")
let intOpt = makeValOpt(asIntOpt)
let int = makeVal(asInt, "an integer")

let asStrOpt = makeAsValOpt(anyToStr)
let asStr = makeAsVal(anyToStr, "a string")
let strOpt = makeValOpt(asStrOpt)
let str = makeVal(asStr, "a string")

let asJsonOpt = makeAsValOpt(anyToJson)
let asJson = makeAsVal(anyToJson, "a json")
let jsonOpt = makeValOpt(asJsonOpt)
let json = makeVal(asJson, "a json")

let asArrOpt = (
    arr:jsonAny, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<option<array<'a>>>>=?,
    ~default:option<default<option<array<'a>>>>=?
):option<array<'a>> => {
    switch anyToArr(arr, mapper) {
        | Ok(arrOpt) => validate(arrOpt, validator)
        | Error(msg) => getDefaultOrExn(default, () => msg)
    }
}

let asArr = (
    arr:jsonAny, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<array<'a>>>=?,
    ~default:option<default<array<'a>>>=?
):array<'a> => {
    switch anyToArr(arr, mapper) {
        | Ok(None) => getDefaultOrExn(default, ()=>`an array was expected at '${arr->getPath->pathToStr}'.`)
        | Ok(Some(arrVal)) => validate(arrVal, validator)
        | Error(msg) => getDefaultOrExn(default, () => msg)
    }
}

let arrOpt = (
    obj:jsonAny, 
    attrName:string,
    mapper:jsonAny=>'a, 
    ~validator:option<validator<option<array<'a>>>>=?,
    ~default:option<default<option<array<'a>>>>=?
):option<array<'a>> => {
    switch getByPath(obj, attrName) {
        | Ok(None) => validate(None, validator)
        | Ok(Some(attrVal)) => asArrOpt(attrVal, mapper, ~validator?, ~default?)
        | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

let arr = (
    obj:jsonAny, 
    attrName:string,
    mapper:jsonAny=>'a, 
    ~validator:option<validator<array<'a>>>=?,
    ~default:option<default<array<'a>>>=?
):array<'a> => {
    switch getByPath(obj, attrName) {
        | Ok(None) => getDefaultOrExn(default, ()=>`an array was expected at '${getLocation2(obj,attrName)}'.`)
        | Ok(Some(attrVal)) => asArr(attrVal, mapper, ~validator?, ~default?)
        | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

let asObjOpt = (
    obj:jsonAny, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<option<'a>>>=?,
    ~default:option<default<option<'a>>>=?
):option<'a> => {
    switch anyToObj(obj, mapper) {
        | Ok(objOpt) => validate(objOpt, validator)
        | Error(msg) => getDefaultOrExn(default, () => msg)
    }
}

let asObj = (
    arr:jsonAny, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<'a>>=?,
    ~default:option<default<'a>>=?
):'a => {
    switch anyToObj(arr, mapper) {
        | Ok(None) => getDefaultOrExn(default, ()=>`an object was expected at '${arr->getPath->pathToStr}'.`)
        | Ok(Some(objVal)) => validate(objVal, validator)
        | Error(msg) => getDefaultOrExn(default, () => msg)
    }
}

let objOpt = (
    obj:jsonAny, 
    attrName:string,
    mapper:jsonAny=>'a, 
    ~validator:option<validator<option<'a>>>=?,
    ~default:option<default<option<'a>>>=?
):option<'a> => {
    switch getByPath(obj, attrName) {
        | Ok(None) => validate(None, validator)
        | Ok(Some(attrVal)) => asObjOpt(attrVal, mapper, ~validator?, ~default?)
        | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

let obj = (
    obj:jsonAny, 
    attrName:string,
    mapper:jsonAny=>'a, 
    ~validator:option<validator<'a>>=?,
    ~default:option<default<'a>>=?
):'a => {
    switch getByPath(obj, attrName) {
        | Ok(None) => getDefaultOrExn(default, ()=>`an object was expected at '${getLocation2(obj,attrName)}'.`)
        | Ok(Some(attrVal)) => asObj(attrVal, mapper, ~validator?, ~default?)
        | Error(_) => getDefaultOrExn(default, ()=>`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

let fromJson = (
    json:JSON.t, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<'a>>=?, 
    ~default:option<default<'a>>=?
):result<'a,string> => {
    try {
        let jsonAny = jsonToAny(json, rootPath)
        Ok(validate(mapper(jsonAny), validator))
    } catch {
        | ex => {
            switch default {
                | Some(default) => Ok(default())
                | None => {
                    let msg = ex 
                        -> Exn.asJsExn
                        -> Belt.Option.flatMap(Exn.message)
                        -> Belt.Option.getWithDefault("no message was provided.")
                    Error("Parse error: " ++ msg)
                }
            }
        }
    }
}

let parseJson = (
    jsonStr:string, 
    mapper:jsonAny=>'a, 
    ~validator:option<validator<'a>>=?, 
    ~default:option<default<'a>>=?
):result<'a,string> => {
    try {
        fromJson(jsonStr->JSON.parseExn, mapper, ~validator?, ~default?)
    } catch {
        | ex => {
            switch default {
                | Some(default) => Ok(default())
                | None => {
                    let msg = ex 
                        -> Exn.asJsExn
                        -> Belt.Option.flatMap(Exn.message)
                        -> Belt.Option.getWithDefault("no message was provided.")
                    Error("Parse error: " ++ msg)
                }
            }
        }
    }
}

let test_pathToStr = (path:list<string>):string => pathToStr(path)