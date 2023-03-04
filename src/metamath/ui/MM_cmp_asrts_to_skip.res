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

        Js.Console.log2("textAfterRegex", textAfterRegex)

        st
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

    <Col>
        {rndText()}
    </Col>
}