let locStorWriteString: (string, string) => unit
let locStorReadString: string => option<string>
let locStorWriteBool: (string, bool) => unit
let locStorReadBool: string => option<bool>
let locStorWriteInt: (string, int) => unit
let locStorReadInt: string => option<int>
let locStorWriteFloat: (string, float) => unit
let locStorReadFloat: string => option<float>
