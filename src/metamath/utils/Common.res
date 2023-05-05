let currTimeStr = () => Js.Date.now()->Js.Date.fromFloat->Js.Date.toISOString

type cache<'depVer,'dep,'data> = {
    recalc:'dep=>'data,
    depVerEq: ('depVer,'depVer) => bool,
    mutable depVer: option<'depVer>,
    mutable data: option<'data>,
}

let cacheMake = ( ~recalc:'dep=>'data, ~depVerEq: ('depVer,'depVer) => bool ) => {
    {recalc, depVerEq, depVer:None, data:None}
}

let cacheGetByDepVer = (cache, depVer) => {
    switch cache.depVer {
        | None => None
        | Some(cachedDepVer) => {
            if (cache.depVerEq(cachedDepVer,depVer)) {
                cache.data
            } else {
                None
            }
        }
    }
}

let cacheGet = (cache, depVer, dep) => {
    switch cacheGetByDepVer(cache, depVer) {
        | Some(data) => data
        | None => {
            cache.depVer = Some(depVer)
            let data = cache.recalc(dep)
            cache.data = Some(data)
            data
        }
    }
}

let splitByRegex = (str,regex) => {
    str
        ->Js_string2.splitByRe(regex)
        ->Js_array2.map(strOpt => strOpt->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(str => str->Js_string2.length > 0)
}

let newLineRegex = %re("/[\n\r]/")
let multilineTextToNonEmptyLines = splitByRegex(_, newLineRegex)

let whitespaceDelimRegex = %re("/[\s\n\r]/")
let getSpaceSeparatedValuesAsArray = splitByRegex(_, whitespaceDelimRegex)

@val external strToBase64: string => string = "btoa"
@val external base64ToStr: string => string = "atob"

let plusSignRegex = %re("/\+/g")
let minusSignRegex = %re("/-/g")
let forwardSlashSignRegex = %re("/\//g")
let underscoreSignRegex = %re("/_/g")
let strToSafeBase64 = str => {
    str
        ->strToBase64
        ->Js.String2.replaceByRe(plusSignRegex, "-")
        ->Js.String2.replaceByRe(forwardSlashSignRegex, "_")
}
let safeBase64ToStr = safeBase64 => {
    safeBase64
        ->Js.String2.replaceByRe(underscoreSignRegex, "/")
        ->Js.String2.replaceByRe(minusSignRegex, "+")
        ->base64ToStr
}

type timeoutID
@val external setTimeout: (unit => unit, int) => timeoutID = "setTimeout"
@val external clearTimeout: (timeoutID) => unit = "clearTimeout"