open Expln_React_Mui
open Expln_React_common

@react.component
let make = (
    ~contOld:reElem,
    ~removeStmt:bool=false,
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
                                {React.string(
                                    if (removeStmt) {
                                        "Discard, remove step"
                                    } else {
                                        "Discard, use this \u2192"
                                    }
                                )}
                            </Button>
                        </td>
                        <td>
                            { if (removeStmt) { React.null } else { contOld } }
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
