open Expln_utils_files

type warning = {
    ord:int,
    code:int,
    path:string
}

let getWarningDescr = (code:int):string => {
    switch code {
        | 1 => `Suspicious-looking start-of-comment mark.`
        | 2 => `Suspicious-looking end-of-comment mark.`
        | 3 => `Deprecated feature.`
        | 4 => `Fragile pattern matching: matching that will remain complete even if additional constructors are added to one of the variant types matched.`
        | 5 => `Partially applied function: expression whose result has function type and is ignored.`
        | 6 => `Label omitted in function application.`
        | 8 => `Partial match: missing cases in pattern-matching.`
        | 9 => `Missing fields in a record pattern.`
        | 10 => `Expression on the left-hand side of a sequence that doesn't have type "unit" (and that is not a function, see warning number 5).`
        | 11 => `Redundant case in a pattern matching (unused match case).`
        | 12 => `Redundant sub-pattern in a pattern-matching.`
        | 14 => `Illegal backslash escape in a string constant.`
        | 16 => `Unerasable optional argument.`
        | 20 => `Unused function argument.`
        | 21 => `Non-returning statement.`
        | 22 => `React: optional argument annotations must have explicit 'option'.`
        | 23 => `Useless record "with" clause.`
        | 24 => `Bad module name: the source file name is not a valid ReScript module name.`
        | 26 => `Suspicious unused variable: unused variable that is bound with "let" or "as", and doesn't start with an underscore ("_") character.`
        | 27 => `Innocuous unused variable: unused variable that is not bound with "let" nor "as", and doesn't start with an underscore ("_") character.`
        | 28 => `Wildcard pattern given as argument to a constant constructor.`
        | 29 => `Unescaped end-of-line in a string constant (non-portable code).`
        | 30 => `Two labels or constructors of the same name are defined in two mutually recursive types.`
        | 32 => `Unused value declaration.`
        | 33 => `Unused open statement.`
        | 34 => `Unused type declaration.`
        | 35 => `Unused for-loop index.`
        | 36 => `Unused ancestor variable.`
        | 37 => `Unused constructor.`
        | 38 => `Unused extension constructor.`
        | 39 => `Unused rec flag.`
        | 43 => `Nonoptional label applied as optional.`
        | 44 => `Open statement shadows an already defined identifier.`
        | 45 => `Open statement shadows an already defined label or constructor.`
        | 46 => `Error in environment variable.`
        | 47 => `Illegal attribute payload.`
        | 48 => `Implicit elimination of optional arguments.`
        | 49 => `Absent cmi file when looking up module alias.`
        | 50 => `Unexpected documentation comment.`
        | 52 => `Fragile constant pattern.`
        | 53 => `Attribute cannot appear in this context`
        | 54 => `Attribute used more than once on an expression`
        | 56 => `Unreachable case in a pattern-matching (based on type information).`
        | 57 => `Ambiguous or-pattern variables under guard`
        | 59 => `Assignment to non-mutable value`
        | 60 => `Unused module declaration`
        | 61 => `Unboxable type in primitive declaration`
        | 62 => `Type constraint on GADT type declaration`
        | 101 => `Unused bs attributes`
        | 102 => `Polymorphic comparison introduced (maybe unsafe)`
        | 103 => `Fragile FFI definitions`
        | 104 => `bs.deriving warning with customized message`
        | 105 => `External name is inferred from val name is unsafe from refactoring when changing value name`
        | 106 => `Unimplemented primitive used:`
        | 107 => `Integer literal exceeds the range of representable integers of type int`
        | 108 => `Uninterpreted delimiters (for unicode)`
        | 109 => `Toplevel expression has unit type`
        | _ => `???`
    }
}

let parseMatch = (~match:string, ~ord:int):warning => {
    switch match->Js.String2.match_(%re("/Warning number (\d+)\s+(C:[^:]+:\d+)/")) {
        | None => Js.Exn.raiseError(`Could not parse a match: ${match}`)
        | Some(groups) => {
            {
                ord,
                code: groups[1]->Belt_Option.flatMap(Belt_Int.fromString)->Belt.Option.getWithDefault(-1),
                path:groups[2]->Belt.Option.getWithDefault("???"),
            }
        }
    }
}

let addToGroup = (~groups:Belt_HashMapInt.t<array<warning>>, ~warning:warning):unit => {
    switch groups->Belt_HashMapInt.get(warning.code) {
        | None => groups->Belt_HashMapInt.set(warning.code, [warning])
        | Some(arr) => arr->Js_array2.push(warning)->ignore
    }
}

let printToStr = (groups:Belt_HashMapInt.t<array<warning>>):string => {
    let res = []
    groups->Belt_HashMapInt.toArray
        ->Js.Array2.sortInPlaceWith(((code1,_),(code2,_)) => code1 - code2)
        ->Js.Array2.forEach(((code,arr)) => {
            res->Js.Array2.push("")->ignore
            res->Js.Array2.push(`Warning ${code->Belt_Int.toString}: ${getWarningDescr(code)}`)->ignore
            arr->Js.Array2.sortInPlaceWith((a,b) => a.ord - b.ord)->Js.Array2.forEach(warning => {
                res->Js.Array2.push(warning.path)->ignore
            })
        })
    res->Js.Array2.joinWith("\n")
}

let parseCompilerOutput = (~compilerOutputFilePath:string):unit => {
    let compilerOutputText = readStringFromFile(compilerOutputFilePath)
    switch compilerOutputText->Js.String2.match_(%re("/Warning number \d+\s+C:[^:]+:\d+/g")) {
        | None => Js.Console.log(`Could not find warnings`)
        | Some(matches) => {
            let matches = matches
                ->Js_array2.filter(Belt_Option.isSome)
                ->Js_array2.map(Belt_Option.getExn)
                ->Js_array2.mapi((match,i) => parseMatch(~match, ~ord=i))
            let groups = Belt_HashMapInt.make(~hintSize=matches->Js_array2.length)
            matches->Js.Array2.forEach(warning => addToGroup(~groups, ~warning))
            groups->printToStr->writeStringToFile(compilerOutputFilePath ++ ".parsed")
        }
    }
}