type jsonAny

type validator<'v> = 'v => result<'v,string>
type default<'v> = () => 'v

let asBoolOpt: (jsonAny, ~validator:validator<option<bool>>=?, ~default:default<option<bool>>=?, ()) => option<bool>
let asBool: (jsonAny, ~validator:validator<bool>=?, ~default:default<bool>=?, ()) => bool
let boolOpt: (jsonAny, string, ~validator:validator<option<bool>>=?, ~default:default<option<bool>>=?, ()) => option<bool>
let bool: (jsonAny, string, ~validator:validator<bool>=?, ~default:default<bool>=?, ()) => bool

let asNumOpt: (jsonAny, ~validator:validator<option<float>>=?, ~default:default<option<float>>=?, ()) => option<float>
let asNum: (jsonAny, ~validator:validator<float>=?, ~default:default<float>=?, ()) => float
let numOpt: (jsonAny, string, ~validator:validator<option<float>>=?, ~default:default<option<float>>=?, ()) => option<float>
let num: (jsonAny, string, ~validator:validator<float>=?, ~default:default<float>=?, ()) => float

let asIntOpt: (jsonAny, ~validator:validator<option<int>>=?, ~default:default<option<int>>=?, ()) => option<int>
let asInt: (jsonAny, ~validator:validator<int>=?, ~default:default<int>=?, ()) => int
let intOpt: (jsonAny, string, ~validator:validator<option<int>>=?, ~default:default<option<int>>=?, ()) => option<int>
let int: (jsonAny, string, ~validator:validator<int>=?, ~default:default<int>=?, ()) => int

let asStrOpt: (jsonAny, ~validator:validator<option<string>>=?, ~default:default<option<string>>=?, ()) => option<string>
let asStr: (jsonAny, ~validator:validator<string>=?, ~default:default<string>=?, ()) => string
let strOpt: (jsonAny, string, ~validator:validator<option<string>>=?, ~default:default<option<string>>=?, ()) => option<string>
let str: (jsonAny, string, ~validator:validator<string>=?, ~default:default<string>=?, ()) => string

let asJsonOpt: (jsonAny, ~validator:validator<option<Js_json.t>>=?, ~default:default<option<Js_json.t>>=?, ()) => option<Js_json.t>
let asJson: (jsonAny, ~validator:validator<Js_json.t>=?, ~default:default<Js_json.t>=?, ()) => Js_json.t
let jsonOpt: (jsonAny, string, ~validator:validator<option<Js_json.t>>=?, ~default:default<option<Js_json.t>>=?, ()) => option<Js_json.t>
let json: (jsonAny, string, ~validator:validator<Js_json.t>=?, ~default:default<Js_json.t>=?, ()) => Js_json.t

let asArrOpt: (jsonAny, jsonAny => 'a, ~validator:validator<option<array<'a>>>=?, ~default:default<option<array<'a>>>=?, ()) => option<array<'a>>
let asArr: (jsonAny, jsonAny => 'a, ~validator:validator<array<'a>>=?, ~default:default<array<'a>>=?, ()) => array<'a>
let arrOpt: (jsonAny, string, jsonAny => 'a, ~validator:validator<option<array<'a>>>=?, ~default:default<option<array<'a>>>=?, ()) => option<array<'a>>
let arr: (jsonAny, string, jsonAny => 'a, ~validator:validator<array<'a>>=?, ~default:default<array<'a>>=?, ()) => array<'a>

let asObjOpt: (jsonAny, jsonAny => 'a, ~validator:validator<option<'a>>=?, ~default:default<option<'a>>=?, ()) => option<'a>
let asObj: (jsonAny, jsonAny => 'a, ~validator:validator<'a>=?, ~default:default<'a>=?, ()) => 'a
let objOpt: (jsonAny, string, jsonAny => 'a, ~validator:validator<option<'a>>=?, ~default:default<option<'a>>=?, ()) => option<'a>
let obj: (jsonAny, string, jsonAny => 'a, ~validator:validator<'a>=?, ~default:default<'a>=?, ()) => 'a

let parseJson: (string, jsonAny => 'a, ~validator: validator<'a>=?, ~default: default<'a>=?, ()) => result<'a, string>
let fromJson: (Js.Json.t, jsonAny => 'a, ~validator: validator<'a>=?, ~default: default<'a>=?, ()) => result<'a, string>



// TEST ----------------------------------------------------------------------------------
let test_pathToStr: list<string> => string