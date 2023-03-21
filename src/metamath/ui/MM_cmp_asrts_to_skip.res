open Expln_React_common
open Expln_React_Mui
open MM_parser
open Common

type state = {
    text: string,
    applyRegex: bool,
    regex: string,
    textAfterRegex: string,
}

type asrtsToSkipResult = {
    regex: string,
    asrtsToSkip: array<string>,
}

let makeInitState = (
    ~initText:string, 
    ~initRegex:string, 
) => {
    {
        text: initText,
        applyRegex: false,
        regex: initRegex,
        textAfterRegex: "",
    }
}

let unsetApplyRegex = (st:state) => { ...st, applyRegex:false }

let setText = (st:state,text) => { ...st, text }

let setRegex = (st:state,regex) => { ...st, regex }

let toggleApplyRegex = (st:state) => {
    if (st.applyRegex) {
        { ...st, applyRegex:false }
    } else {
        let userRegex = Js.Re.fromString(st.regex)
        let textAfterRegex = st.text
            ->multilineTextToNonEmptyLines
            ->Js_array2.map(userRegex->Js.Re.exec_)
            ->Js_array2.filter(Belt_Option.isSome)
            ->Js_array2.map(resOpt => resOpt->Belt_Option.getExn->Js.Re.captures)
            ->Js_array2.filter(captures => captures->Js_array2.length > 1)
            ->Js_array2.map(captures => captures->Js_array2.sliceFrom(1))
            ->Expln_utils_common.arrFlatMap(captures => captures)
            ->Js_array2.map(Js.Nullable.toOption)
            ->Js_array2.filter(Belt_Option.isSome)
            ->Js_array2.map(Belt_Option.getExn)
            ->Js_array2.joinWith("\n")
        { ...st, applyRegex:true, textAfterRegex }
    }
}

let textToArrayOfAsrts = str => {
    str->multilineTextToNonEmptyLines->Expln_utils_common.arrFlatMap(getSpaceSeparatedValuesAsArray)
}

@react.component
let make = (
    ~initText:string, 
    ~initRegex:string, 
    ~onSave: asrtsToSkipResult => unit,
    ~onCancel: unit => unit,
) => {
    let (state, setState) = React.useState(() => makeInitState( ~initText, ~initRegex, ))

    let actUpdateText = text => {
        setState(st => {
            let st = st->unsetApplyRegex
            setText(st,text)
        })
    }
    
    let actUpdateRegex = regex => {
        setState(st => {
            let st = st->unsetApplyRegex
            setRegex(st,regex)
        })
    }

    let actToggleApplyRegex = () => {
        setState(toggleApplyRegex)
    }

    let actOnSave = () => {
        onSave({
            regex: state.regex,
            asrtsToSkip: textToArrayOfAsrts(
                if (state.applyRegex) {
                    state.textAfterRegex
                } else {
                    state.text
                }
            )
        })
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>actOnSave()} variant=#contained>
                {React.string("Save")}
            </Button>
            <Button onClick={_=>onCancel()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
        </Row>
    }

    let rndText = () => {
        <Col>
            <TextField
                label="Assertions to skip"
                size=#small
                style=ReactDOM.Style.make(~width="800px", ())
                autoFocus=true
                multiline=true
                maxRows=10
                value=state.text
                onChange=evt2str(actUpdateText)
            />
            {
                if (state.applyRegex) {
                    React.null
                } else {
                    rndButtons()
                }
            }
        </Col>
    }

    let rndRegex = () => {
        <Col>
            <Row>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=state.applyRegex
                            onChange={_ => actToggleApplyRegex()}
                        />
                    }
                    label="apply regular expression: "
                />
                <TextField
                    label="Regex to apply to each line"
                    size=#small
                    style=ReactDOM.Style.make(~width="300px", ())
                    value=state.regex
                    onChange=evt2str(actUpdateRegex)
                />
            </Row>
            {
                if (state.applyRegex) {
                    <TextField
                        label="Assertions to skip (after regex applied)"
                        size=#small
                        style=ReactDOM.Style.make(~width="800px", ())
                        multiline=true
                        maxRows=10
                        value=state.textAfterRegex
                    />
                } else {
                    React.null
                }
            }
            {
                if (state.applyRegex) {
                    rndButtons()
                } else {
                    React.null
                }
            }
        </Col>
    }

    <Col spacing=1.>
        {rndText()}
        {rndRegex()}
    </Col>
}