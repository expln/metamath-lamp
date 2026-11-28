
let strToBool = (str:string):option<bool> => {
    switch str {
        | "true" => Some(true)
        | "false" => Some(false)
        | _ => None
    }
}

/*
Debounced writes.

A value passed to locStorWriteStringDebounced is not written to the local storage immediately.
All pending values are written debounceDelayMs after the first of them was submitted,
or earlier when the page gets hidden or closed.
Values are computed lazily at the moment of writing,
so frequent updates of the same key don't serialize the same data many times.
A value is not written if the local storage already contains the same value under the same key.

The read, delete and enumeration functions of this module take pending writes into account.
*/

let debounceDelayMs = 1000
let pendingWrites:dict<unit=>string> = Dict.make()
let flushTimer:ref<option<timeoutId>> = ref(None)
let flushOnPageHideIsInstalled = ref(false)

@val @scope("window") external addWindowEventListener: (string, unit=>unit) => unit = "addEventListener"
@val @scope("document") external addDocumentEventListener: (string, unit=>unit) => unit = "addEventListener"
@val @scope("document") external documentVisibilityState: string = "visibilityState"

let flushPendingWrite = (key:string):unit => {
    switch pendingWrites->Dict.get(key) {
        | None => ()
        | Some(getValue) => {
            pendingWrites->Dict.delete(key)
            let newValue = getValue()
            if (Dom_storage2.localStorage->Dom_storage2.getItem(key) != Some(newValue)) {
                Dom_storage2.localStorage->Dom_storage2.setItem(key, newValue)
            }
        }
    }
}

let locStorFlushPendingWrites = ():unit => {
    flushTimer.contents->Option.forEach(clearTimeout)
    flushTimer := None
    pendingWrites->Dict.keysToArray->Array.forEach(flushPendingWrite)
}

let installFlushOnPageHide = ():unit => {
    if (!flushOnPageHideIsInstalled.contents) {
        flushOnPageHideIsInstalled := true
        addWindowEventListener("pagehide", locStorFlushPendingWrites)
        addDocumentEventListener("visibilitychange", () => {
            if (documentVisibilityState == "hidden") {
                locStorFlushPendingWrites()
            }
        })
    }
}

let locStorWriteStringDebounced = (key:string, getValue:unit=>string):unit => {
    installFlushOnPageHide()
    pendingWrites->Dict.set(key, getValue)
    if (flushTimer.contents->Option.isNone) {
        flushTimer := Some(setTimeout(locStorFlushPendingWrites, debounceDelayMs))
    }
}

let locStorWriteString = (key:string, str:string):unit => {
    pendingWrites->Dict.delete(key)
    Dom_storage2.localStorage->Dom_storage2.setItem(key, str)
}

let locStorReadString = (key:string):option<string> => {
    flushPendingWrite(key)
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

let locStorLength = ():int => {
    // Pending writes are flushed, so that enumeration of keys sees all of them.
    locStorFlushPendingWrites()
    Dom_storage2.localStorage->Dom_storage2.length
}

let locStorKey = (idx:int):option<string> => {
    Dom_storage2.localStorage->Dom_storage2.key(idx)
}

let locStorDeleteKey = (key:string):unit => {
    pendingWrites->Dict.delete(key)
    Dom_storage2.localStorage->Dom_storage2.removeItem(key)
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
        ~toString = Belt_Int.toString(_),
    )
}