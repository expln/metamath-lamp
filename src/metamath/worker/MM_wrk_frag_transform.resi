
type fragmentTransformState

type reactElemDto

type fragmentTransform = {
    canApply: {"step":Js_json.t} => bool,
    displayName: {"step":Js_json.t} => string,
    createInitialState: {"step":Js_json.t} => fragmentTransformState,
    renderDialog: 
        {
            "state":fragmentTransformState, 
            "setState":(fragmentTransformState => fragmentTransformState) => unit
        } => reactElemDto,
}

let reactElemDtoToObj: reactElemDto => {..}
let objToObj: {..} => {..}
let objToFragmentTransformState: {..} => fragmentTransformState

let arrStrToFragTransforms: array<string> => result<array<fragmentTransform>,string>