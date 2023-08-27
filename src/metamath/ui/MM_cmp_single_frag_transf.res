open Expln_React_Modal
open MM_react_common
open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open Common

type rec selection = {
    "text": string,
    "children": array<selection>,
}

type fragmentTransformState = string
type reactElemDto = string

type fragmentTransform = {
    canApply: {"selection":selection} => bool,
    displayName: {"selection":selection} => string,
    createInitialState: {"selection":selection} => fragmentTransformState,
    renderDialog: 
        {
            "selection":selection, 
            "state":fragmentTransformState, 
            "setState":(fragmentTransformState => fragmentTransformState) => unit
        } => reactElemDto,
}

external stateToObj: fragmentTransformState => {..} = "%identity"
external reactElemDtoToObj: reactElemDto => {..} = "%identity"
external objToObj: {..} => {..} = "%identity"

let unsafeFunc = (title:string, func:unit=>'a):result<'a,string> => {
    try {
        Ok(func())
    } catch {
        | Js.Exn.Error(exn) => {
            let errMsg = `${title}: ${exn->Js.Exn.message->Belt_Option.getWithDefault("unknown error.")}.`
            Error(errMsg)
        }
        | _ => {
            let errMsg = `${title}: unknown error.`
            Error(errMsg)
        }
    }
}

let reqExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    switch nullable->Js.Nullable.toOption {
        | None => Js_exn.raiseError(`A required attribute is missing: ${msg}`)
        | Some(value) => value
    }
}

let optExn = (nullable:Js.Nullable.t<'a>):option<'a> => {
    nullable->Js.Nullable.toOption
}

let isString: 'a => bool = %raw(`obj => typeof obj === 'string'`)
let isBool: 'a => bool = %raw(`obj => typeof obj === 'boolean'`)
let isArray: 'a => bool = %raw(`obj => Array.isArray(obj)`)
let isObject: 'a => bool = %raw(`obj => obj !== undefined && obj !== null && !Array.isArray(obj) && typeof obj === 'object'`)
let isFunction: 'a => bool = %raw(`obj => typeof obj === 'function'`)

let reqStrExn = (nullable:Js.Nullable.t<'a>, msg:string):string => {
    let res = reqExn(nullable, msg)
    if (!isString(res)) {
        Js_exn.raiseError(`Not a string: ${msg}`)
    } else {
        res
    }
}

let reqBoolExn = (nullable:Js.Nullable.t<'a>, msg:string):bool => {
    let res = reqExn(nullable, msg)
    if (!isBool(res)) {
        Js_exn.raiseError(`Not a boolean: ${msg}`)
    } else {
        res
    }
}

let reqArrExn = (nullable:Js.Nullable.t<'a>, msg:string):array<'b> => {
    let res = reqExn(nullable, msg)
    if (!isArray(res)) {
        Js_exn.raiseError(`Not an array: ${msg}`)
    } else {
        res
    }
}

let reqObjExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isObject(res)) {
        Js_exn.raiseError(`Not an object: ${msg}`)
    } else {
        res
    }
}

let reqFuncExn = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    let res = reqExn(nullable, msg)
    if (!isFunction(res)) {
        Js_exn.raiseError(`Not a function: ${msg}`)
    } else {
        res
    }
}

let optStrExn = (nullable:Js.Nullable.t<'a>, msg:string):option<string> => {
    switch optExn(nullable) {
        | None => None
        | Some(res) => {
            if (!isString(res)) {
                Js_exn.raiseError(`Not a string: ${msg}`)
            } else {
                Some(res)
            }
        }
    }
}

@react.component
let make = (
    ~selection:selection,
    ~transform:fragmentTransform,
    ~onBack:unit=>unit,
    ~onInsertAbove:string=>unit,
    ~onInsertBelow:string=>unit,
    ~onUpdateCurrent:string=>unit,
):reElem => {
    let (error, setError) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    React.useEffect0(() => {
        let state = unsafeFunc("Creating initial state", 
            () => transform.createInitialState({"selection":selection})
        )
        switch state {
            | Error(msg) => setError(_ => Some(msg))
            | Ok(state) => setState(_ => Some(state))
        }
        None
    })

    let actCopyToClipboard = (text) => {
        copyToClipboard(text)->promiseMap(_ => {
            setCopiedToClipboard(timerId => {
                switch timerId {
                    | None => ()
                    | Some(timerId) => clearTimeout(timerId)
                }
                Some(setTimeout(
                    () => setCopiedToClipboard(_ => None),
                    1000
                ))
            })
        })->ignore
    }

    let rec rndCustomElem = (elem:{..}):reElem => {
        if (!isObject(elem)) {
            Js.Exn.raiseError("Each component must be an object")
        }
        let componentName = reqStrExn(elem["cmp"], 
            "Each component must have a string attribute 'cmp' which specifies component name"
        )
        switch componentName {
            | "Col" => rndCol(objToObj(elem))
            | "Row" => rndRow(objToObj(elem))
            | "Checkbox" => rndCheckbox(objToObj(elem))
            | "TextField" => rndTextField(objToObj(elem))
            | "Text" => rndText(objToObj(elem))
            | "span" => rndSpan(objToObj(elem))
            | "ApplyButtons" => rndApplyButtons(objToObj(elem))
            | "Divider" => rndDivider()
            | _ => Js_exn.raiseError(`Unrecognized component '${componentName}'`)
        }
    }
    and childrenToArray = (children:array<Js.Nullable.t<{..}>>, msg:string):reElem => {
        children
            ->Js_array2.mapi((child,i) => {
                let child = reqObjExn(child, `A child element is not a component object: ${msg}`)
                rndCustomElem(child)->React.cloneElement({
                    "key":optStrExn(child["key"], "optional 'key' attribute of any component must be a string")
                            ->Belt_Option.getWithDefault(Belt_Int.toString(i))
                })
            })
            ->React.array
    }
    and rndCol = (elem:{..}):reElem => {
        let msg = "Each 'Col' component must have a 'children' attribute which holds an array of components " 
                            ++ "comprising the content of this 'Col' component"
        <Col>
            {elem["children"]->reqArrExn(msg)->childrenToArray(msg)}
        </Col>
    }
    and rndRow = (elem:{..}):reElem => {
        let msg = "Each 'Row' component must have a 'children' attribute which holds an array of components " 
                            ++ "comprising the content of this 'Row' component"
        <Row>
            {elem["children"]->reqArrExn(msg)->childrenToArray(msg)}
        </Row>
    }
    and rndCheckbox = (elem:{..}):reElem => {
        let checked = reqBoolExn(elem["checked"], "Each Checkbox must have a boolean attribute 'checked'")
        let onChange = reqFuncExn(elem["onChange"], "Each Checkbox must have an attribute 'onChange' of type boolean => void")
        <FormControlLabel
            control={
                <Checkbox
                    checked
                    onChange=evt2bool(b => onChange(. b))
                />
            }
            label=reqStrExn(elem["label"], "Each Checkbox must have a string attribute 'label'")
        />
    }
    and rndTextField = (elem:{..}):reElem => {
        let onChange = reqFuncExn(elem["onChange"], "Each TextField must have an attribute 'onChange' of type string => void")
        <TextField
            label=reqStrExn(elem["label"], "Each TextField must have a string attribute 'label'")
            size=#small
            style=ReactDOM.Style.make(
                ~width=?optStrExn(elem["width"], "optional 'width' attribute of a TextField must be a string"), 
                ()
            )
            value=reqStrExn(elem["value"], "Each TextField must have a string attribute 'value'")
            onChange=evt2str(str => onChange(. str))
        />
    }
    and rndText = (elem:{..}):reElem => {
        <span 
            style=ReactDOM.Style.make(
                ~backgroundColor=?optStrExn(elem["bkgColor"], "optional 'bkgColor' attribute of a 'span' component must be a string"), 
                ()
            )
        >
            {React.string(reqStrExn(elem["value"], "Each Text component must have a string attribute 'value'"))}
        </span>
    }
    and rndSpan = (elem:{..}):reElem => {
        let msg = "Each 'span' component must have a 'children' attribute which holds an array of components " 
                            ++ "comprising the content of this 'span' component"
        <span>
            {elem["children"]->reqArrExn(msg)->childrenToArray(msg)}
        </span>
    }
    and rndDivider = ():reElem => {
        <Divider/>
    }
    and rndApplyButtons = (elem:{..}):reElem => {
        let result = reqStrExn(elem["result"], "Each ApplyButtons component must have a string attribute 'result'")
            ->getSpaceSeparatedValuesAsArray->Js.Array2.joinWith(" ")
        <Col>
            <Row>
                <IconButton title="Back" onClick={_=>onBack()} color="primary" > 
                    <MM_Icons.ArrowBack/> 
                </IconButton>
                <IconButton title="Copy to the clipboard" onClick={_=>actCopyToClipboard(result)} color="primary" > 
                    <MM_Icons.ContentCopy/> 
                </IconButton>
                <IconButton title="Add new step above" onClick={_=>onInsertAbove(result)} color="primary" >
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />
                </IconButton>
                <IconButton title="Add new step below" onClick={_=>onInsertBelow(result)} color="primary" >
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />
                </IconButton>
                <IconButton title="Update current step" onClick={_=>onUpdateCurrent(result)} color="primary" > 
                    <MM_Icons.Done/> 
                </IconButton>
            </Row>
            {
                if (copiedToClipboard->Belt.Option.isSome) {
                    React.string("Copied to the clipboard.")
                } else {React.null}
            }
        </Col>
    }

    let rndCustomContent = (state:fragmentTransformState) => {
        let params = {
            "selection":selection, 
            "state":state, 
            "setState": mapper => setState(Belt_Option.map(_, mapper))
        }
        let reElemDto = unsafeFunc("Rendering dialog (getting a DTO)", 
            () => transform.renderDialog(params)
        )
        switch reElemDto {
            | Error(msg) => {
                setError(_ => Some(msg))
                React.null
            }
            | Ok(reElemDto) => {
                let customElem = unsafeFunc("Rendering dialog (rendering a DTO)", 
                    () => reElemDto->reactElemDtoToObj->rndCustomElem
                )
                switch customElem {
                    | Error(msg) => {
                        setError(_ => Some(msg))
                        React.null
                    }
                    | Ok(customElem) => customElem
                }
            }
        }
    }

    let rndContent = () => {
        switch error {
            | Some(msg) => {
                <Col>
                    {React.string(`Error: ${msg}`)}
                    <Button title="Back" onClick={_=>onBack()} > <MM_Icons.CancelOutlined/> </Button>
                </Col>
            }
            | None => {
                switch state {
                    | None => {
                        <Col spacing=1.>
                            {React.string(`Initializing...`)}
                            <Button onClick={_=>onBack()} variant=#outlined>
                                {React.string("Cancel")}
                            </Button>
                        </Col>
                    }
                    | Some(state) => rndCustomContent(state)
                }
            }
        }
    }

    rndContent()
}