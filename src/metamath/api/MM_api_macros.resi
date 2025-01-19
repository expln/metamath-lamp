let initMacroApi: () => unit
let unregisterMacroModule: string => unit
let listRegisteredMacroModules: unit => array<string>
let listRegisteredMacrosInModule: string => option<array<string>>
let runMacro: ( ~moduleName:string, ~macroName:string, ) => promise<result<unit,string>>