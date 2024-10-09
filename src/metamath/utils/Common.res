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

let strToRegex = (str:string):result<Js_re.t,string> => {
    try {
        Ok(Js_re.fromString(str))
    } catch {
        | exn => {
            Error(
                exn->Js_exn.asJsExn->Belt_Option.flatMap(Js_exn.message)
                    ->Belt.Option.getWithDefault(`could not create a regular expression from string '${str}'`)
            )
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

type arrayQueue<'a> = {
    mutable begin:int,
    mutable end:int,
    mutable maxEnd:int,
    mutable data: array<'a>,
}

let arrayQueueMake = (initSize:int):arrayQueue<'a> => {
    let data = Expln_utils_common.createArray(initSize)
    {
        begin:0,
        end:-1,
        maxEnd: data->Js.Array2.length-1,
        data,
    }
}

let arrayQueueAdd = (q:arrayQueue<'a>, elem:'a):unit => {
    if (q.end == q.maxEnd) {
        Js.Console.log(`q.end == q.maxEnd`)
        q.data = Belt_Array.concat(q.data, Expln_utils_common.createArray(q.data->Js_array2.length))
        q.maxEnd = q.data->Js.Array2.length-1
    }
    q.end = q.end + 1
    q.data[q.end] = elem
}

let arrayQueuePop = (q:arrayQueue<'a>):option<'a> => {
    if (q.begin <= q.end) {
        let res = q.data->Array.getUnsafe(q.begin)
        q.begin = q.begin + 1
        Some(res)
    } else {
        None
    }
    
}

let arrayQueueReset = (q:arrayQueue<'a>):unit => {
    q.begin = 0
    q.end = -1
}

let createVarTypeComparator = (
    ~varTypes:Belt_HashMapInt.t<int>, 
    ~typeOrder:Belt_HashMapInt.t<int>
): Expln_utils_common.comparator<int> => {
    let varOrderByType = Belt_HashMapInt.make(~hintSize=varTypes->Belt_HashMapInt.size)
    varTypes->Belt_HashMapInt.forEach((var,typ) => {
        switch typeOrder->Belt_HashMapInt.get(typ) {
            | Some(tOrder) => varOrderByType->Belt_HashMapInt.set(var,tOrder)
            | None => ()
        }
    })
    (a:int,b:int) => {
        switch varOrderByType->Belt_HashMapInt.get(a) {
            | None => {
                switch varOrderByType->Belt_HashMapInt.get(b) {
                    | None => 0.0
                    | Some(_) => 1.0
                }
            }
            | Some(aTypOrder) => {
                switch varOrderByType->Belt_HashMapInt.get(b) {
                    | None => -1.0
                    | Some(bTypOrder) => Expln_utils_common.intCmp(aTypOrder,bTypOrder)
                }
            }
        }
    }
}

let createVarNameComparator = (varNames:Belt_HashMapInt.t<string>): Expln_utils_common.comparator<int> => {
    (a:int,b:int) => {
        switch varNames->Belt_HashMapInt.get(a) {
            | None => {
                switch varNames->Belt_HashMapInt.get(b) {
                    | None => 0.0
                    | Some(_) => 1.0
                }
            }
            | Some(aStr) => {
                switch varNames->Belt_HashMapInt.get(b) {
                    | None => -1.0
                    | Some(bStr) => aStr->String.localeCompare(bStr)
                }
            }
        }
    }
}

let createTypeOrderFromStr = (~sortDisjByType:string, ~typeNameToInt:string=>option<int>):Belt_HashMapInt.t<int> => {
    let typeOrderInDisj = Belt_HashMapInt.make(~hintSize=4)
    sortDisjByType->getSpaceSeparatedValuesAsArray->Js.Array2.forEach(typStr => {
        switch typeNameToInt(typStr) {
            | Some(i) => typeOrderInDisj->Belt_HashMapInt.set(i,typeOrderInDisj->Belt_HashMapInt.size)
            | None => ()
        }
    })
    typeOrderInDisj
}