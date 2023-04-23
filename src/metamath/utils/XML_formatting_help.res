open Expln_React_Mui

let exampleFormattedText =
`
A simple 
<a font-weight="bold" color="green" href="https://us.metamath.org">Metamath</a> 
proof
`

@react.component
let make = (
    ~onClose:unit=>unit,
) => {
    
    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col spacing=1.>
            <Button onClick={_=>onClose()} variant=#contained > {React.string("Close")} </Button>
            <Paper style=ReactDOM.Style.make( ~padding="10px", () )>
                <Static_XML_to_HTML xmlStr=exampleFormattedText />
            </Paper>
            <Paper style=ReactDOM.Style.make( ~padding="10px", () )>
                <pre>{exampleFormattedText->React.string}</pre>
            </Paper>
            <Button onClick={_=>onClose()} variant=#contained > {React.string("Close")} </Button>
        </Col>
    </Paper>
}