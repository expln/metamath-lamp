open MM_syntax_tree

type rec selection = {
    "children": array<selection>, 
    "text": string
}

type fragmentTransformState = string

type reactElemDto = string

type fragmentTransform = {
    canApply: {"selection": selection} => bool,
    displayName: {"selection": selection} => string,
    createInitialState: {"selection": selection} => fragmentTransformState,
    renderDialog: {
        "selection": selection,
        "setState": (fragmentTransformState => fragmentTransformState) => unit,
        "state": fragmentTransformState,
    } => reactElemDto,
}

let stateToObj: fragmentTransformState => {..}
let reactElemDtoToObj: reactElemDto => {..}
let objToObj: {..} => {..}
let objToFragmentTransformState: {..} => fragmentTransformState

let unsafeFunc: (string, unit => 'a) => result<'a, string>
let reqExn: (Js.Nullable.t<'a>, string) => 'a
let optExn: Js.Nullable.t<'a> => option<'a>
let isString: 'a => bool
let isBool: 'a => bool
let isArray: 'a => bool
let isObject: 'a => bool
let isFunction: 'a => bool
let reqStrExn: (Js.Nullable.t<string>, string) => string
let reqBoolExn: (Js.Nullable.t<bool>, string) => bool
let reqArrExn: (Js.Nullable.t<array<'b>>, string) => array<'b>
let reqObjExn: (Js.Nullable.t<'a>, string) => 'a
let reqFuncExn: (Js.Nullable.t<'a>, string) => 'a
let optStrExn: (Js.Nullable.t<string>, string) => option<string>

let syntaxTreeToSelection: childNode => selection
let arrStrToFragTransforms: array<string> => result<array<fragmentTransform>,string>