
type fragmentTransformState

type reactElemDto

type fragmentTransform = {
    canApply: {"step":Js_json.t} => bool,
    displayName: {"step":Js_json.t} => string,
    createInitialState: {"step":Js_json.t} => fragmentTransformState,
    renderDialog: 
        {
            "step":Js_json.t, 
            "state":fragmentTransformState, 
            "setState":(fragmentTransformState => fragmentTransformState) => unit
        } => reactElemDto,
}

let reactElemDtoToObj: reactElemDto => {..}
let objToObj: {..} => {..}
let objToFragmentTransformState: {..} => fragmentTransformState

let unsafeFunc: (string, unit => 'a) => result<'a, string>
let isObject: 'a => bool
let reqStrExn: (Js.Nullable.t<string>, string) => string
let optStrExn: (Js.Nullable.t<string>, string) => option<string>
let reqBoolExn: (Js.Nullable.t<bool>, string) => bool
let optBoolExn: (Js.Nullable.t<bool>, string) => option<bool>
let reqArrExn: (Js.Nullable.t<array<'b>>, string) => array<'b>
let reqObjExn: (Js.Nullable.t<'a>, string) => 'a
let optObjExn: (Js.Nullable.t<'a>, string) => option<'a>
let reqFuncExn: (Js.Nullable.t<'a>, string) => 'a

let arrStrToFragTransforms: array<string> => result<array<fragmentTransform>,string>