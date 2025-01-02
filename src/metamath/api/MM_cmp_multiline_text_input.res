open Expln_React_common
open Expln_React_Mui

@react.component
let make = (
    ~initText: string = "",
    ~onOk: string => unit,
    ~onCancel: unit => unit,
) => {
    let (text, setText) = React.useState(() => initText)

    let actUpdateText = text => {
        setText(_ => text)
    }
    
    let actOnOk = () => {
        onOk(text)
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>actOnOk()} variant=#contained>
                {React.string("Ok")}
            </Button>
            <Button onClick={_=>onCancel()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
        </Row>
    }

    <Col spacing=1.>
        <TextField
            size=#small
            style=ReactDOM.Style.make(~width="800px", ())
            autoFocus=true
            multiline=true
            maxRows=10
            value=text
            onChange=evt2str(actUpdateText)
        />
        { rndButtons() }
    </Col>
}