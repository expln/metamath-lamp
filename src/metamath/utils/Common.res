let nbsp = String.fromCharCode(160)
let circleChar = String.fromCharCode(9679)

type mmException = {
    msg:string,
    begin?:int,
}
exception MmException(mmException)

type exnData = {
    exn:option<exn>,
    msg:string,
    stack:string,
}

let panic = (msg:string):'a => {
    raise(MmException({msg:msg}))
}

let jsErrorToExnData = (exn:exn):exnData => {
    let jsExn = Error.fromException(exn)
    {
        exn:Some(exn),
        msg: jsExn->Option.flatMap(Error.message)->Option.getOr("Unknown error."),
        stack: jsExn->Option.flatMap(Error.stack)->Option.getOr(""),
    }
}

let catchExn = (run:unit=>'a): result<'a,exnData> => {
    try {
        Ok(run())
    } catch {
        | MmException({msg}) => Error({ exn:None, msg, stack: "", })
        | exn => Error(jsErrorToExnData(exn))
    }
}

let currTimeStr = () => Date.now()->Date.fromTime->Date.toISOString
let compareDates = (a:Date.t, b:Date.t):float => {
    let t1 = a->Date.getTime
    let t2 = b->Date.getTime
    if (t1 < t2) {
        -1.0
    } else if (t2 < t1) {
        1.0
    } else {
        0.0
    }
}

let floatToPctStr = pct => (pct  *. 100.)->Math.round->Belt.Float.toInt->Belt_Int.toString ++ "%"

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

let strToRegex = (str:string):result<RegExp.t,string> => {
    try {
        Ok(RegExp.fromString(str))
    } catch {
        | exn => {
            Error(
                exn->Exn.asJsExn->Belt_Option.flatMap(Exn.message)
                    ->Belt.Option.getWithDefault(`could not create a regular expression from string '${str}'`)
            )
        }
    }
}

let splitByRegex = (str,regex) => {
    str
        ->String.splitByRegExp(regex)
        ->Array.map(strOpt => strOpt->Belt_Option.getWithDefault("")->String.trim)
        ->Array.filter(str => str->String.length > 0)
}

let newLineRegex = %re("/[\n\r]/")
let multilineTextToNonEmptyLines = splitByRegex(_, newLineRegex)

let whitespaceDelimRegex = %re("/[\s\n\r\f]/")
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
        ->String.replaceRegExp(plusSignRegex, "-")
        ->String.replaceRegExp(forwardSlashSignRegex, "_")
}
let safeBase64ToStr = safeBase64 => {
    safeBase64
        ->String.replaceRegExp(underscoreSignRegex, "/")
        ->String.replaceRegExp(minusSignRegex, "+")
        ->base64ToStr
}

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
        maxEnd: data->Array.length-1,
        data,
    }
}

let arrayQueueAdd = (q:arrayQueue<'a>, elem:'a):unit => {
    if (q.end == q.maxEnd) {
        Console.log(`q.end == q.maxEnd`)
        q.data = Belt_Array.concat(q.data, Expln_utils_common.createArray(q.data->Array.length))
        q.maxEnd = q.data->Array.length-1
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
                    | Some(bTypOrder) => Int.compare(aTypOrder,bTypOrder)
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
    sortDisjByType->getSpaceSeparatedValuesAsArray->Array.forEach(typStr => {
        switch typeNameToInt(typStr) {
            | Some(i) => typeOrderInDisj->Belt_HashMapInt.set(i,typeOrderInDisj->Belt_HashMapInt.size)
            | None => ()
        }
    })
    typeOrderInDisj
}