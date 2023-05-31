@module("fs")
external readFileSync: string => {..} = "readFileSync"

@module("fs")
external writeFileSync: (string,string) => unit = "writeFileSync"

let readStringFromFile = path => readFileSync(path)["toString"](.)
let writeStringToFile = (path, text) => writeFileSync(path, text)