
let strToBool = (str:string):option<bool> => {
    switch str {
        | "true" => Some(true)
        | "false" => Some(false)
        | _ => None
    }
}

let locStorWriteString = (key:string, str:string):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, str)
}

let locStorReadString = (key:string):option<string> => {
    Dom_storage2.localStorage->Dom_storage2.getItem(key)
}

let locStorWriteBool = (key:string, bool:bool):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, Expln_utils_common.stringify(bool))
}

let locStorReadBool = (key:string):option<bool> => {
    locStorReadString(key)->Belt_Option.flatMap(strToBool)
}

let locStorWriteInt = (key:string, int:int):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, int->Belt_Int.toString)
}

let locStorReadInt = (key:string):option<int> => {
    locStorReadString(key)->Belt_Option.flatMap(Belt_Int.fromString)
}

let locStorWriteFloat = (key:string, fl:float):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, fl->Belt_Float.toString)
}

let locStorReadFloat = (key:string):option<float> => {
    locStorReadString(key)->Belt_Option.flatMap(Belt_Float.fromString)
}

let useStateFromLocalStorage = (
    ~key:string,
    ~fromString:option<string>=>'a,
    ~toString:'a=>string,
):('a, ('a=>'a) => unit) => {
    let (value, setValuePriv) = React.useState(_ => fromString(locStorReadString(key)))

    let setValue = (mapper:('a=>'a)) => {
        setValuePriv(old => {
            let new = mapper(old)
            locStorWriteString(key, toString(new))
            new
        })
    }

    ( value, setValue )
}

let useStateFromLocalStorageBool = (~key:string,~default:bool):(bool, (bool=>bool) => unit) => {
    useStateFromLocalStorage(
        ~key,
        ~fromString = boolStrOpt => {
            switch boolStrOpt {
                | None => default
                | Some("true") => true
                | Some("false") => false
                | Some(_) => default
            }
        },
        ~toString = Expln_utils_common.stringify,
    )
}

let useStateFromLocalStorageStr = (~key:string,~default:string):(string, (string=>string) => unit) => {
    useStateFromLocalStorage(
        ~key,
        ~fromString = strOpt => {
            switch strOpt {
                | None => default
                | Some(str) => str
            }
        },
        ~toString = str => str,
    )
}

let useStateFromLocalStorageInt = (~key:string,~default:int):(int, (int=>int) => unit) => {
    useStateFromLocalStorage(
        ~key,
        ~fromString = strOpt => {
            switch strOpt {
                | None => default
                | Some(str) => str->Belt_Int.fromString->Belt.Option.getWithDefault(default)
            }
        },
        ~toString = Belt_Int.toString,
    )
}