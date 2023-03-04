open Expln_React_common
open Expln_React_Mui

type state = {
    text: string,
    applyRegex: bool,
    regex: string,
    textAfterRegex: string,
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

let unsetApplyRegex = (st) => { ...st, applyRegex:false }

let setText = (st,text) => { ...st, text }

let setRegex = (st,regex) => { ...st, regex }

let newLineRegex = %re("/[\n\r]/")
let toggleApplyRegex = (st) => {
    if (st.applyRegex) {
        { ...st, applyRegex:false }
    } else {
        let userRegex = Js.Re.fromString(st.regex)
        let textAfterRegex = st.text
            ->Js_string2.splitByRe(newLineRegex)
            ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
            ->Js_array2.filter(s => s->Js_string2.length > 0)
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

        Js.Console.log2("textAfterRegex", textAfterRegex)

        { ...st, applyRegex:true, textAfterRegex }
    }
}

type asrtsToSkipResult = {
    regex: string,
    asrtsToSkip: array<string>,
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

    let rndText = () => {
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
    }

    let rndRegex = () => {
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
    }

    <Col spacing=1.>
        {rndText()}
        {rndRegex()}
    </Col>
}