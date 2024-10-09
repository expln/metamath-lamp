open MM_react_common
open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open Common
open MM_wrk_frag_transform
open Raw_js_utils

@react.component
let make = (
    ~step:Js_json.t,
    ~transform:fragmentTransform,
    ~onBack:unit=>unit,
    ~onInsertAbove:(bool,string)=>unit,
    ~onInsertBelow:(bool,string)=>unit,
    ~onUpdateCurrent:(bool,string)=>unit,
):reElem => {
    let (error, setError) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    React.useEffect0(() => {
        let state = invokeExnFunc("Creating initial state", 
            () => transform.createInitialState({"step":step})
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
            | "RadioGroup" => rndRadioGroup(objToObj(elem))
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
            ->Js.Array2.filter(child => child->Js.Nullable.toOption->Belt.Option.isSome)
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
        let disabled = optBoolExn(elem["disabled"], "optional 'disabled' attribute of a Checkbox must be a boolean")
        let onChange = reqFuncExn(elem["onChange"], 
            "Each Checkbox must have an attribute 'onChange' of type boolean => void")
        <FormControlLabel
            control={
                <Checkbox
                    checked
                    onChange=evt2bool(b => onChange(. b))
                    ?disabled
                />
            }
            label=reqStrExn(elem["label"], "Each Checkbox must have a string attribute 'label'")
        />
    }
    and rndRadioGroup = (elem:{..}):reElem => {
        let row = reqBoolExn(elem["row"], "Each RadioGroup must have a boolean attribute 'row'")
        let value = reqStrExn(elem["value"], "Each RadioGroup must have a string attribute 'value'")
        let disabled = optBoolExn(elem["disabled"], "optional 'disabled' attribute of a RadioGroup must be a boolean")
        let onChange = reqFuncExn(elem["onChange"], 
            "Each RadioGroup must have an attribute 'onChange' of type string => void")
        let options = reqArrExn(elem["options"], 
            "Each RadioGroup must have an array attribute 'options' of type array<[value:string,label:string]>")
        <RadioGroup row value onChange=evt2str(str => onChange(. str)) >
            {
                options
                    ->Js_array2.map(option => {
                        let value = option->Array.getUnsafe(0)
                        let label = option->Array.getUnsafe(1)
                        <FormControlLabel 
                            key=value value label control={ <Radio/> } 
                            style=ReactDOM.Style.make(~marginRight="30px", ())
                            ?disabled
                        />
                    })
                    ->React.array
            }
        </RadioGroup>
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
            autoFocus=?optBoolExn(elem["autoFocus"], "optional 'autoFocus' attribute of a TextField must be a boolean")
            onChange=evt2str(str => onChange(. str))
        />
    }
    and rndText = (elem:{..}):reElem => {
        <span 
            style=ReactDOM.Style.make(
                ~backgroundColor=?optStrExn(elem["backgroundColor"], "optional 'backgroundColor' attribute of a 'Text' component must be a string"), 
                ~fontWeight=?optStrExn(elem["fontWeight"], "optional 'fontWeight' attribute of a 'Text' component must be a string"), 
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
        let replaceSelection = optBoolExn(
            elem["replaceSelection"], 
            "the 'replaceSelection' attribute of ApplyButtons component must be a boolean"
        )->Belt.Option.getWithDefault(true)
        <Col>
            <Row>
                <IconButton title="Back" onClick={_=>onBack()} color="primary" > 
                    <MM_Icons.ArrowBack/> 
                </IconButton>
                <IconButton title="Copy to the clipboard" onClick={_=>actCopyToClipboard(result)} color="primary" > 
                    <MM_Icons.ContentCopy/> 
                </IconButton>
                <IconButton title="Add new step above" 
                    onClick={_=>onInsertAbove(replaceSelection, result)} color="primary" 
                >
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />
                </IconButton>
                <IconButton title="Add new step below" 
                    onClick={_=>onInsertBelow(replaceSelection, result)} color="primary"
                >
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />
                </IconButton>
                <IconButton title="Update current step" 
                    onClick={_=>onUpdateCurrent(replaceSelection, result)} color="primary" 
                > 
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
            "state":state, 
            "setState": mapper => setState(Belt_Option.map(_, mapper(_)))
        }
        let reElemDto = invokeExnFunc("Rendering dialog (getting a DTO)", 
            () => transform.renderDialog(params)
        )
        switch reElemDto {
            | Error(msg) => {
                setError(_ => Some(msg))
                React.null
            }
            | Ok(reElemDto) => {
                let customElem = invokeExnFunc("Rendering dialog (rendering a DTO)", 
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
                    <pre>
                        {React.string(`Error: ${msg}`)}
                    </pre>
                    <Button title="Back" onClick={_=>onBack()} > <MM_Icons.ArrowBack/> </Button>
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