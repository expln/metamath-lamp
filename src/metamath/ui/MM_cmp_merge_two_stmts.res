open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open MM_parser
open Expln_React_Modal
open MM_statements_dto

@react.component
let make = (
    ~stmt1:userStmt,
    ~stmt2:userStmt,
    ~onCancel:unit=>unit,
    ~onStmtSelected:(userStmt,userStmt)=>unit
) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                {React.string("Merging two statements")}
            </span>
            <span>
                {MM_cmp_user_stmt.rndContText(stmt1.cont)}
            </span>
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