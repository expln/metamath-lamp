open Raw_js_utils
open MM_api

type macro = {
    name: string,
    run:unit=>promise<unit>,
}

let macroModules:Belt_HashMapString.t<array<macro>> = Belt_HashMapString.make(~hintSize=4)

let registerMacroModule = (
    ~moduleName:string,
    ~macros:array<macro>,
):unit => {
    macroModules->Belt_HashMapString.set(moduleName, macros )
}

let apiRegisterMacroModule = (params:apiInput):promise<result<unit,string>> => {
    let params = params->apiInputToObjExn("apiRegisterMacroModule: got empty parameters.")
    let moduleName = reqStrExn(params["moduleName"], "'moduleName' must be a string.")
    let macros = reqArrExn(params["macros"], "'macros' must be an array.")->Array.map(macroObj => {
        {
            name: reqStrExn(macroObj["name"], "'name' attribute of a macro must be a string."),
            run: reqFuncExn(macroObj["run"], "'run' attribute of a macro must be a function.")
        }
    })
    registerMacroModule(~moduleName, ~macros)
    Promise.resolve(Ok(()))
}

let unregisterMacroModule = (
    moduleName:string,
):unit => {
    macroModules->Belt_HashMapString.remove(moduleName)
}

let apiUnregisterMacroModule = (params:apiInput):promise<result<unit,string>> => {
    let params = params->apiInputToObjExn("apiUnregisterMacroModule: got empty parameters.")
    let moduleName = reqStrExn(params["moduleName"], "'moduleName' must be a string.")
    unregisterMacroModule(moduleName)
    Promise.resolve(Ok(()))
}

let listRegisteredMacroModules = ():array<string> => {
    macroModules->Belt_HashMapString.keysToArray
}

let apiListRegisteredMacroModules = ():promise<result<JSON.t,string>> => {
    Promise.resolve(Ok(JSON.Encode.array(
        listRegisteredMacroModules()->Array.map(JSON.Encode.string(_))
    )))
}

let listRegisteredMacrosInModule = (moduleName:string):option<array<string>> => {
    macroModules->Belt_HashMapString.get(moduleName)->Option.map(macros => {
        macros->Array.map(macro => macro.name)
    })
}

let apiListRegisteredMacrosInModule = (params:apiInput):promise<result<JSON.t,string>> => {
    let params = params->apiInputToObjExn("apiListRegisteredMacrosInModule: got empty parameters.")
    let moduleName = reqStrExn(params["moduleName"], "'moduleName' must be a string.")
    let macroNames = switch listRegisteredMacrosInModule(moduleName) {
        | None => []
        | Some(arr) => arr
    }
    Promise.resolve(Ok(JSON.Encode.array(
        macroNames->Array.map(JSON.Encode.string(_))
    )))
}

let runMacro = (
    ~moduleName:string,
    ~macroName:string,
):promise<result<unit,string>> => {
    switch macroModules->Belt_HashMapString.get(moduleName) {
        | None => Promise.resolve(Error(`Cannot find a macro module with name "${moduleName}"`))
        | Some(macros) => {
            switch macros->Array.find(macro => macro.name == macroName) {
                | None => {
                    Promise.resolve(Error(`Cannot find a macro with name "${macroName}" in the module "${moduleName}"`))
                }
                | Some(macro) => macro.run()->Promise.thenResolve(_ => Ok())
            }
        }
    }
}

let apiRunMacro = (params:apiInput):promise<result<unit,string>> => {
    let params = params->apiInputToObjExn("apiRunMacro: got empty parameters.")
    let moduleName = reqStrExn(params["moduleName"], "'moduleName' must be a string.")
    let macroName = reqStrExn(params["macroName"], "'macroName' must be a string.")
    runMacro(~moduleName, ~macroName)
}

let initMacroApi = () => {
    setMacroApi({
        "registerMacroModule": makeApiFunc("macro.registerMacroModule", apiRegisterMacroModule),
        "unregisterMacroModule": makeApiFunc("macro.unregisterMacroModule", apiUnregisterMacroModule),
        "listRegisteredMacroModules": makeApiFunc("macro.listRegisteredMacroModules", _ => apiListRegisteredMacroModules()),
        "listRegisteredMacrosInModule": makeApiFunc("macro.listRegisteredMacrosInModule", apiListRegisteredMacrosInModule),
        "runMacro": makeApiFunc("macro.runMacro", apiRunMacro),
    })
}
