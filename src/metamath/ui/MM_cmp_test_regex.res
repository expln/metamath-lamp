open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Common

let checkMarkSymbol = "\u2713"
let crossSymbol = "\u2717"

@react.component
let make = (
    ~initRegex:string,
    ~onSave: string=>unit,
    ~onCancel: unit=>unit,
) => {
    let (text, setText) = React.useState(() => "")
    let (regex, setRegex) = React.useState(() => initRegex)
    let (result, setResult) = React.useState(() => None)

    let actResetResult = () => {
        setResult(_ => None)
    }

    let actTextUpdated = str => {
        setText(_ => str)
        actResetResult()
    }

    let actRegexUpdated = str => {
        setRegex(_ => str)
        actResetResult()
    }

    let actTestRegex = () => {
        switch regex->strToRegex {
            | Error(msg) => setResult(_ => Some(Error(msg)))
            | Ok(re) => setResult(_ => Some(Ok(re->Js_re.test_(text))))
        }
    }

    let rndText = () => {
        <TextField
            label="Text to test regex on"
            size=#small
            style=ReactDOM.Style.make(~width="800px", ())
            autoFocus=true
            multiline=true
            maxRows=10
            value=text
            onChange=evt2str(actTextUpdated)
            onKeyDown=kbrdHnd(~key=keyEsc, ~act=onCancel, ())
        />
    }

    let rndRegex = () => {
        <Row>
            <TextField
                label="Regex"
                size=#small
                style=ReactDOM.Style.make(~width="722px", ())
                value=regex
                onChange=evt2str(actRegexUpdated)
                onKeyDown=kbrdHnd2(
                    kbrdClbkMake(~key=keyEnter, ~act=actTestRegex, ()),
                    kbrdClbkMake(~key=keyEsc, ~act=onCancel, ()),
                )
            />
            <Button 
                onClick=(_=>actTestRegex()) 
                variant=#contained 
                color="grey" 
                disabled={regex->String.trim->String.length == 0}
            > 
                { React.string("Test") }
            </Button>
        </Row>
    }

    let rndResult = () => {
        switch result {
            | None => React.string(`Click the "TEST" button to test the regex.`)
            | Some(Ok(res)) => {
                if (res) {
                    <span>
                        <span style=ReactDOM.Style.make(~color="green", ~fontWeight="bold", ())>
                            {React.string(checkMarkSymbol)}
                        </span>
                        {React.string(` the regex matches the text.`)}
                    </span>
                } else {
                    <span>
                        <span style=ReactDOM.Style.make(~color="red", ~fontWeight="bold", ())>
                            {React.string(crossSymbol)}
                        </span>
                        {React.string(` the regex doesn't match the text.`)}
                    </span>
                }
            }
            | Some(Error(msg)) => {
                <span>
                    <span style=ReactDOM.Style.make(~color="red", ())>
                        {React.string("Error: ")}
                    </span>
                    {React.string(msg)}
                </span>
            }
        }
    }

    let rndButtons = () => {
        <Row alignItems=#center>
            <Button onClick=(_=>onSave(regex)) variant=#contained > 
                { React.string("Save regex") }
            </Button>
            <Button onClick={_=>onCancel()} > {React.string("Cancel")} </Button>
        </Row>
    }

    
    <Paper style=ReactDOM.Style.make(~padding="15px", ())>
        <Col spacing=2.>
            {rndText()}
            {rndRegex()}
            {rndResult()}
            {rndButtons()}
        </Col>
    </Paper>
}