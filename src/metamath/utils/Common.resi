let nbsp:string
let circleChar:string

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

let jsErrorToExnData: exn => exnData
let catchExn:(unit=>'a) => result<'a,exnData>

let currTimeStr: unit => string
let compareDates: (Date.t, Date.t) => float
let floatToPctStr: float => string

type cache<'depVer,'dep,'data>
let cacheMake: ( ~recalc:'dep=>'data, ~depVerEq: ('depVer,'depVer) => bool ) => cache<'depVer,'dep,'data>
let cacheGetByDepVer: (cache<'depVer,'dep,'data>, 'depVer) => option<'data>
let cacheGet: (cache<'depVer,'dep,'data>, 'depVer, 'dep) => 'data

let multilineTextToNonEmptyLines: string => array<string>
let getSpaceSeparatedValuesAsArray: string => array<string>

let strToBase64: string => string
let base64ToStr: string => string
let strToSafeBase64: string => string
let safeBase64ToStr: string => string

let strToRegex: string => result<RegExp.t,string>

type version<'a> = {
    ver:int,
    val:'a
}
let versionMake: 'a => version<'a>
let versionSet: (version<'a>, 'a) => version<'a>

type arrayQueue<'a>
let arrayQueueMake: int => arrayQueue<'a>
let arrayQueueAdd: (arrayQueue<'a>, 'a) => unit
let arrayQueuePop: arrayQueue<'a> => option<'a>
let arrayQueueReset: arrayQueue<'a> => unit

let createVarTypeComparator: (
    ~varTypes:Belt_HashMapInt.t<int>, 
    ~typeOrder:Belt_HashMapInt.t<int>
) => Expln_utils_common.comparator<int>
let createVarNameComparator: Belt_HashMapInt.t<string> => Expln_utils_common.comparator<int>
let createTypeOrderFromStr: (~sortDisjByType:string, ~typeNameToInt:string=>option<int>) => Belt_HashMapInt.t<int>