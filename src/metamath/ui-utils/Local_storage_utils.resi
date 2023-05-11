let locStorWriteString: (string, string) => unit
let locStorReadString: string => option<string>
let locStorWriteBool: (string, bool) => unit
let locStorReadBool: string => option<bool>
let locStorWriteInt: (string, int) => unit
let locStorReadInt: string => option<int>
let locStorWriteFloat: (string, float) => unit
let locStorReadFloat: string => option<float>

let useStateFromLocalStorage: (
    ~key:string,
    ~fromString:option<string>=>'a,
    ~toString:'a=>string,
    ~tempMode:bool,
) => ('a, ('a=>'a) => unit)

let useStateFromLocalStorageBool: (~key:string,~default:bool,~tempMode:bool) => (bool, (bool=>bool) => unit)
let useStateFromLocalStorageStr: (~key:string,~default:string,~tempMode:bool) => (string, (string=>string) => unit)