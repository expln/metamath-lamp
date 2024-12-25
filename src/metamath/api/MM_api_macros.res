type macro = {
    name: string,
    run:unit=>unit,
}

let macroModules:Belt_HashMapString.t<Belt_HashMapString.t<macro>> = Belt_HashMapString.make(~hintSize=4)

let registerMacroModule = (
    ~moduleName:string,
    ~macros:array<macro>,
):unit => {
    macroModules->Belt_HashMapString.set(
        moduleName,
        macros->Array.map(macro => (macro.name, macro))->Belt_HashMapString.fromArray
    )
}

let unregisterMacroModule = (
    ~moduleName:string,
):unit => {
    macroModules->Belt_HashMapString.remove(moduleName)
}

let listRegisteredMacroModules = ():array<string> => {
    macroModules->Belt_HashMapString.keysToArray
}

let listRegisteredMacrosInModule = (moduleName:string):option<array<string>> => {
    macroModules->Belt_HashMapString.get(moduleName)->Option.map(Belt_HashMapString.keysToArray(_))
}

let runMacro = (
    ~moduleName:string,
    ~macroName:string,
):unit => {
    switch macroModules->Belt_HashMapString.get(moduleName) {
        | None => Console.error(`Cannot find a macro module with name "${moduleName}"`)
        | Some(macros) => {
            switch macros->Belt_HashMapString.get(macroName) {
                | None => Console.error(`Cannot find a macro with name "${macroName}" in the module "${moduleName}"`)
                | Some(macro) => macro.run()
            }
        }
    }
}