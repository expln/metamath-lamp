open Raw_js_utils

type fragmentTransformState = string
type reactElemDto = string

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

external reactElemDtoToObj: reactElemDto => {..} = "%identity"
external objToObj: {..} => {..} = "%identity"
external objToFragmentTransformState: {..} => fragmentTransformState = "%identity"

let createTransformFromObject = (obj:{..}):fragmentTransform => {
    {
        canApply: params => obj["canApply"](. params),
        displayName: params => obj["displayName"](. params),
        createInitialState: params => obj["createInitialState"](. params),
        renderDialog: params => obj["renderDialog"](. params),
    }
}

let stringToFragTransforms = (str:string):result<array<fragmentTransform>,string> => {
    switch invokeExnFunc("Execute the transforms script", () => executeFunctionBody(str)) {
        | Error(msg) => Error(msg)
        | Ok(transforms) => {
            invokeExnFunc(
                "Convert objects from the transforms script to internal representation", 
                () => transforms["map"](. createTransformFromObject)
            )
        }
    }
}

let arrStrToFragTransforms = (texts:array<string>):result<array<fragmentTransform>,string> => {
    texts->Js_array2.reduce(
        (res,text) => {
            switch res {
                | Error(_) => res
                | Ok(arr) => {
                    switch stringToFragTransforms(text) {
                        | Error(msg) => Error(msg)
                        | Ok(newArr) => Ok(arr->Array.concat(newArr))
                    }
                }
            }
        },
        Ok([])
    )
}