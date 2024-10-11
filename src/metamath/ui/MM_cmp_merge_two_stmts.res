open Expln_React_Mui
open MM_wrk_editor

@react.component
let make = (
    ~stmt1:userStmt,
    ~stmt2:userStmt,
    ~onCancel:unit=>unit,
    ~onStmtSelected:(userStmt,userStmt)=>unit
) => {

    let rndStmt = (stmt:userStmt) => {
        let jstf = if (stmt.typ == E) {"HYP"} else {"[ " ++ stmt.jstfText ++ " ]"}
        React.string(stmt.label ++ " " ++ jstf)
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <Row>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {React.string("Merging ")}
                </span>
                <span>
                    {MM_cmp_user_stmt.rndContText(~stmtCont=stmt1.cont)}
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
                            { rndStmt(stmt1) }
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <Button onClick={_=>onStmtSelected(stmt2,stmt1)}>
                                {React.string("Use this \u2192")}
                            </Button>
                        </td>
                        <td>
                            { rndStmt(stmt2) }
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