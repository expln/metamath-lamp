open Expln_React_Mui
open MM_wrk_editor

@react.component
let make = (
    ~contOld:stmtCont,
    ~contNew:stmtCont,
    ~onDiscard:unit=>unit,
    ~onSave:unit=>unit,
    ~onContinueEditing:unit=>unit,
) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <Row>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {React.string("Discard or save?")}
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
                            {MM_cmp_user_stmt.rndContText(contOld)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <Button onClick={_=>onSave()}>
                                {React.string("Save, use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            {MM_cmp_user_stmt.rndContText(contNew)}
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