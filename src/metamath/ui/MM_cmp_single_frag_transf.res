open MM_syntax_tree
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
    renderDialog: {"selection":selection, "state":fragmentTransformState, "setState":(fragmentTransformState => fragmentTransformState) => unit} => reactElemDto,
}

external stateToObj: fragmentTransformState => {..} = "%identity"
external reElemDtoToObj: reactElemDto => {..} = "%identity"

let unsafeFunc = (modalRef:modalRef, title:string, func:unit=>'a):result<'a,string> => {
    try {
        Ok(func())
    } catch {
        | Js.Exn.Error(exn) => {
            let errMsg = `${title}: ${exn->Js.Exn.message->Belt_Option.getWithDefault("unknown error.")}.`
            openInfoDialog( ~modalRef, ~title="Error", ~text=errMsg, (), )
            Error(errMsg)
        }
        | _ => {
            let errMsg = `${title}: unknown error.`
            openInfoDialog( ~modalRef, ~title="Error", ~text=errMsg, (), )
            Error(errMsg)
        }
    }
}



@react.component
let make = (
    ~modalRef: modalRef,
    ~onCancel:unit=>unit,
    ~onApply:string=>unit,
    ~selection:selection,
    ~transform:fragmentTransform,
):reElem => {
    let (error, setError) = React.useState(() => None)
    let (state, setState) = React.useState(() => None)

    React.useEffect0(() => {
        let state = unsafeFunc(modalRef, "Creating initial state", 
            () => transform.createInitialState({"selection":selection})
        )
        switch state {
            | Error(msg) => setError(_ => Some(msg))
            | Ok(state) => setState(_ => Some(state))
        }
        None
    })

    let rndCustomContent = (state:fragmentTransformState) => {
        let params = {
            "selection":selection, 
            "state":state, 
            "setState": mapper => setState(st => {
                switch st {
                    | None => None
                    | Some(st) => Some(mapper(st))
                }
            })
        }
        let reElemDto = unsafeFunc(modalRef, "Rendering dialog (getting a DTO)", 
            () => transform.renderDialog(params)
        )
        switch reElemDto {
            | Error(msg) => {
                setError(_ => Some(msg))
                React.null
            }
            | Ok(reElemDto) => {
                React.string(`CustomContent`)
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