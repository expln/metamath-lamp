let nbsp = Js_string2.fromCharCode(160)
let circleChar = Js_string2.fromCharCode(9679)

let currTimeStr = () => Js.Date.now()->Js.Date.fromFloat->Js.Date.toISOString
let compareDates = (a:Js_date.t, b:Js_date.t):int => {
    let t1 = a->Js_date.getTime
    let t2 = b->Js_date.getTime
    if (t1 < t2) {
        -1
    } else if (t2 < t1) {
        1
    } else {
        0
    }
}

let floatToPctStr = pct => (pct  *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString ++ "%"

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

type timeoutID = int
@val external setTimeout: (unit => unit, int) => timeoutID = "setTimeout"
@val external clearTimeout: (timeoutID) => unit = "clearTimeout"
let stubTimeoutId: timeoutID = 0

type version<'a> = {
    ver:int,
    val:'a
}

let versionMake = (val:'a):version<'a> => {
    {
        ver:0,
        val
    }
}

let versionSet = (version:version<'a>, newVal:'a):version<'a> => {
    {
        ver:version.ver+1,
        val:newVal
    }
}