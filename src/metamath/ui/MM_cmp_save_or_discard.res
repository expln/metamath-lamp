open Expln_React_Mui
open Expln_React_common
open MM_wrk_editor

@react.component
let make = (
    ~contOld:reElem,
    ~contNew:reElem,
    ~onDiscard:unit=>unit,
    ~onSave:unit=>unit,
    ~onContinueEditing:unit=>unit,
) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <Row>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {React.string("Discard changes or save?")}
                </span>
            </Row>
            <table>
                <tbody>
                    <tr>
                        <td>
                            <Button onClick={_=>onDiscard()}>
                                {React.string("Discard, use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            {contOld}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <Button onClick={_=>onSave()}>
                                {React.string("Save, use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            {contNew}
                        </td>
                    </tr>
                </tbody>
            </table>
            <Button onClick={_=>onContinueEditing()} >
                {React.string("Continue Editing")}
            </Button>
        </Col>
    </Paper>
}