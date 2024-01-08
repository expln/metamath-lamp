open Expln_React_common
open Expln_React_Mui


@react.component
let make = (
    ~title:string,
    ~warningText:string,
    ~hideInit:bool,
    ~onHideChange:bool=>unit,
    ~onClose:unit=>unit,
) => {
    let (hide, setHide) = React.useState(() => hideInit)

    let actHideChange = newHide => {
        setHide(_ => newHide)
        onHideChange(newHide)
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <span style=ReactDOM.Style.make( ~fontWeight="bold", ~color="#FF7900", () ) >
                {title->React.string}
            </span>
            <table>
                <tbody>
                    <tr>
                        <td>
                            <span style=ReactDOM.Style.make(~color="#FF7900", () ) >
                                <MM_Icons.Warning/>
                            </span>
                        </td>
                        <td style=ReactDOM.Style.make(~padding="10px", () )>
                            {warningText->React.string}
                        </td>
                    </tr>
                </tbody>
            </table>
            <Row alignItems=#center>
                <Button onClick={_=>onClose()} variant=#contained >
                    {React.string("Ok")}
                </Button>
                <FormControlLabel
                    control={ <Checkbox checked=hide onChange=evt2bool(actHideChange) /> }
                    label="Don't show again"
                />
            </Row>
        </Col>
    </Paper>
}