open Expln_React_Modal
open MM_react_common
open Expln_React_common
open Expln_React_Mui

type selection = {
    "text": string,
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

let req = (nullable:Js.Nullable.t<'a>, msg:string):'a => {
    switch nullable->Js.Nullable.toOption {
        | None => Js_exn.raiseError(`A required attribute is missing: ${msg}.`)
        | Some(value) => value
    }
}

let opt = (nullable:Js.Nullable.t<'a>):option<'a> => {
    nullable->Js.Nullable.toOption
}

@react.component
let make = (
    ~onBack:unit=>unit,
    ~onCopy:string=>unit,
    ~onApply:string=>unit,
    ~selection:selection,
    ~transform:fragmentTransform,
):reElem => {
    let (error, setError) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)

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

    let rec rndCustomElem = (elem:{..}):reElem => {
        let componentName = req(elem["cmp"], 
            "Each component must have a string attribute 'cmp' which specifies component name"
        )
        switch componentName {
            | "Col" => rndCol(objToObj(elem))
            | "Row" => rndRow(objToObj(elem))
            | "Checkbox" => rndCheckbox(objToObj(elem))
            | "TextField" => rndTextField(objToObj(elem))
            | "Text" => rndText(objToObj(elem))
            | "ApplyButtons" => rndApplyButtons(objToObj(elem))
            | "Divider" => rndDivider()
            | _ => Js_exn.raiseError(`Unrecognized component '${componentName}'`)
        }
    }
    and childrenToArray = (children:array<{..}>):reElem => {
        children
            ->Js_array2.mapi((child,i) => {
                rndCustomElem(child)->React.cloneElement({
                    "key":opt(child["key"])->Belt_Option.getWithDefault(Belt_Int.toString(i))
                })
            })
            ->React.array
    }
    and rndCol = (elem:{..}):reElem => {
        <Col>
            {
                childrenToArray(
                    req(
                        elem["children"], 
                        "Each 'Col' component must have a 'children' attribute which holds an array of components " 
                            ++ "comprising the content of this 'Col' component"
                    )
                )
            }
        </Col>
    }
    and rndRow = (elem:{..}):reElem => {
        <Row>
            {
                childrenToArray(
                    req(
                        elem["children"], 
                        "Each 'Row' component must have a 'children' attribute which holds an array of components " 
                            ++ "comprising the content of this 'Row' component"
                    )
                )
            }
        </Row>
    }
    and rndCheckbox = (elem:{..}):reElem => {
        let checked = req(elem["checked"], "Each Checkbox must have a boolean attribute 'checked'")
        let onChange = req(elem["onChange"], "Each Checkbox must have an attribute 'onChange' of type boolean => void")
        <FormControlLabel
            control={
                <Checkbox
                    checked
                    onChange=evt2bool(b => onChange(. b))
                />
            }
            label=req(elem["label"], "Each Checkbox must have a string attribute 'label'")
        />
    }
    and rndTextField = (elem:{..}):reElem => {
        let onChange = req(elem["onChange"], "Each TextField must have an attribute 'onChange' of type string => void")
        <TextField
            label=req(elem["label"], "Each TextField must have a string attribute 'label'")
            size=#small
            style=ReactDOM.Style.make(~width=?opt(elem["width"]), ())
            value=req(elem["value"], "Each TextField must have a string attribute 'value'")
            onChange=evt2str(str => onChange(. str))
        />
    }
    and rndText = (elem:{..}):reElem => {
        <span>
            {React.string(req(elem["value"], "Each Text component must have a string attribute 'value'"))}
        </span>
    }
    and rndDivider = ():reElem => {
        <Divider/>
    }
    and rndApplyButtons = (elem:{..}):reElem => {
        let result = req(elem["result"], "Each ApplyButtons component must have a string attribute 'result'")
        <Row>
            <Button title="Back" onClick={_=>onBack()} > <MM_Icons.CancelOutlined/> </Button>
            <Button title="Copy to the clipboard" onClick={_=>onCopy(result)} > <MM_Icons.ContentCopy/> </Button>
        </Row>
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