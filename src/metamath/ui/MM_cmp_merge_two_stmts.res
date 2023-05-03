open Expln_React_Mui
open MM_wrk_editor

@react.component
let make = (
    ~stmt1:userStmt,
    ~stmt2:userStmt,
    ~onCancel:unit=>unit,
    ~onStmtSelected:(userStmt,userStmt)=>unit
) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <Row>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {React.string("Merging ")}
                </span>
                <span>
                    {MM_cmp_user_stmt.rndContText(~stmtCont=stmt1.cont, ())}
                </span>
            </Row>
            <table>
                <tbody>
                    <tr>
                        <td>
                            <Button onClick={_=>onStmtSelected(stmt1,stmt2)}>
                                {React.string("Use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            {
                                React.string(stmt1.label ++ " [ " ++ stmt1.jstfText ++ " ]")
                            }
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <Button onClick={_=>onStmtSelected(stmt2,stmt1)}>
                                {React.string("Use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            {
                                React.string(stmt2.label ++ " [ " ++ stmt2.jstfText ++ " ]")
                            }
                        </td>
                    </tr>
                </tbody>
            </table>
            <Button onClick={_=>onCancel()} >
                {React.string("Cancel")}
            </Button>
        </Col>
    </Paper>
}