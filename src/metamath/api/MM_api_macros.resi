let initMacroApi: () => unit
let setOverrideMacroModuleName: option<string> => unit
let unregisterMacroModule: string => unit
let listRegisteredMacroModules: unit => array<string>
let listRegisteredMacrosInModule: string => option<array<string>>
let runMacro: ( ~moduleName:string, ~macroName:string, ) => result<unit,string>