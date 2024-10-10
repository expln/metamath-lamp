
type fragmentTransformState

type reactElemDto

type fragmentTransform = {
    canApply: {"step":JSON.t} => bool,
    displayName: {"step":JSON.t} => string,
    createInitialState: {"step":JSON.t} => fragmentTransformState,
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