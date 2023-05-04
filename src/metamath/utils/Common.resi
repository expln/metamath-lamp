let currTimeStr: unit => string

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