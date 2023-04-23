open Expln_React_common
open Expln_React_Mui

@react.component
let make = (
    ~onImport: string => unit,
    ~onCancel: unit => unit,
) => {
    let (text, setText) = React.useState(() => "")

    let actUpdateText = text => {
        setText(_ => text)
    }
    
    let actOnImport = () => {
        onImport(text)
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>actOnImport()} variant=#contained>
                {React.string("Import")}
            </Button>
            <Button onClick={_=>onCancel()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
        </Row>
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <TextField
                label="Editor state in JSON format"
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
    </Paper>
}