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

@react.component
let make = (
    ~onCancel:unit=>unit,
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
        switch elem["cmp"] {
            | "Col" => rndCol(elem)
            | "Row" => rndRow(elem)
            | "Checkbox" => rndCheckbox(elem)
            | "TextField" => rndTextField(elem)
            | "Text" => rndText(elem)
            | "ApplyButtons" => rndApplyButtons(elem)
            | "Divider" => rndDivider()
            | _ => Js_exn.raiseError(`Unrecognized component '${elem["cmp"]}'`)
        }
    }
    and childrenToArray = (elem):reElem => {
        elem["children"]
            ->Js_array2.mapi((child,i) => {
                rndCustomElem(child)->React.cloneElement({
                    "key":elem["key"]->Js.Nullable.toOption->Belt_Option.getWithDefault(Belt_Int.toString(i))
                })
            })
            ->React.array
    }
    and rndCol = (elem:{..}):reElem => {
        <Col>
            {childrenToArray(elem)}
        </Col>
    }
    and rndRow = (elem:{..}):reElem => {
        <Row>
            {childrenToArray(elem)}
        </Row>
    }
    and rndCheckbox = (elem:{..}):reElem => {
        <FormControlLabel
            control={
                <Checkbox
                    checked=elem["checked"]
                    onChange=evt2bool(b => elem["onChange"](. b))
                />
            }
            label=elem["label"]
        />
    }
    and rndTextField = (elem:{..}):reElem => {
        <TextField
            label=elem["label"]
            size=#small
            style=ReactDOM.Style.make(~width=?(elem["width"]->Js.Nullable.toOption), ())
            value=elem["value"]
            onChange=evt2str(str => elem["onChange"](. str))
        />
    }
    and rndText = (elem:{..}):reElem => {
        <span>
            {React.string(elem["value"])}
        </span>
    }
    and rndDivider = ():reElem => {
        <Divider/>
    }
    and rndApplyButtons = (elem:{..}):reElem => {
        <Row>
            <Button onClick={_=>onApply(elem["result"])} variant=#contained>
                {React.string("Apply")}
            </Button>
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
                <Col spacing=1.>
                    {React.string(`Error: ${msg}`)}
                    <Button onClick={_=>onCancel()} variant=#outlined>
                        {React.string("Cancel")}
                    </Button>
                </Col>
            }
            | None => {
                switch state {
                    | None => {
                        <Col spacing=1.>
                            {React.string(`Initializing...`)}
                            <Button onClick={_=>onCancel()} variant=#outlined>
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